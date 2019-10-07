{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}

module Dismantle.XML
  ( loadXML
  , XMLException(..)
  , DT.ISA(..)
  , DT.Endianness(..)
  , DT.OperandPayload(..)
  , DT.FormOverride(..)
  , DT.InstFieldDescriptor(..)
  , DT.UnusedBitsPolicy(..)
  ) where

import qualified Control.Exception as E
import           Control.Monad (forM)
import qualified Control.Monad.State.Strict as MS
import           Data.List (stripPrefix, find, nub)
import           Data.List.Split as LS
import qualified Data.Map as M
import           Data.Maybe (catMaybes, fromMaybe)
import qualified Data.Set as S
import           Data.Void (Void)
import qualified System.IO.Strict as SIO
import qualified Text.Megaparsec as P
import           Text.Printf (printf)
import qualified Text.XML.Light as X

import qualified Dismantle.Tablegen as DT
import qualified Dismantle.Tablegen.ByteTrie as BT
import qualified Dismantle.Tablegen.Parser.Types as PT

data XMLException = MissingChildElement String X.Element
                  | MissingAttr String X.Element
                  | MissingField X.Element String
                  | InvalidChildElement String X.Element
                  | InvalidAttr String X.Element
                  | MultipleChildElements String X.Element
                  | MnemonicError String X.Element
                  | InvalidPattern String
                  | MismatchingFieldLengths [Field] [BT.Bit]
                  | InvalidXmlFile String
  deriving Show

instance E.Exception XMLException

-- | Monad for keeping track of how many times we've seen a particular mnemonic;
-- since there are duplicates, we need to add qualifiers to the names to distinguish
-- them from each other.
newtype XML a = XML (MS.StateT XMLState IO a)
  deriving ( Functor
           , Applicative
           , Monad
           , MS.MonadState XMLState
           , MS.MonadIO
           )

data XMLState = XMLState { usedNames :: M.Map String Int
                         , xmlPath :: Maybe FilePath
                         }

runXML :: XML a -> IO a
runXML (XML a) = MS.evalStateT a (XMLState M.empty Nothing)

qname :: String -> X.QName
qname str = X.QName str Nothing Nothing

-- | Given a path to the directory containing all XML instruction files, build an ISA
-- descriptor.
loadXML :: (X.Element -> Bool) -> String -> FilePath -> IO DT.ISADescriptor
loadXML fltr arch dirPath = runXML $ do
  MS.modify' $ \st -> st { xmlPath = Just dirPath }
  let fullPath = dirPath ++ "/a32_encindex.xml"
  fileStr <- MS.liftIO $ SIO.readFile fullPath
  xmlElement <- case X.parseXMLDoc fileStr of
    Just c -> return c
    Nothing -> E.throw $ InvalidXmlFile fullPath
  let iclass_sects = filter fltr (X.findElements (qname "iclass_sect") xmlElement)
  instrs <- fmap concat $ forM iclass_sects $ \iclass_sect -> do
    fields <- iclassFields iclass_sect
    matchPattern <- iclassMatchPattern iclass_sect
    matchFlds <- iclassMatchFields fields iclass_sect
    negPatterns <- iclassNegPatterns fields iclass_sect
    let leaves = X.filterElements isLeaf iclass_sect
        isLeaf elt = X.qName (X.elName elt) == "tr" &&
                     X.findAttr (qname "class") elt == Just "instructiontable" &&
                     X.findAttr (qname "undef") elt /= Just "1" &&
                     X.findAttr (qname "unpred") elt /= Just "1" &&
                     X.findAttr (qname "reserved_nop_hint") elt /= Just "1"
    descs <- forM leaves $ \leaf -> xmlLeaf matchPattern negPatterns matchFlds leaf
    return descs
  return $ DT.ISADescriptor { DT.isaInstructions = instrs
                            , DT.isaOperands = S.toList (S.fromList (concatMap instrOperandTypes instrs))
                            , DT.isaErrors = []
                            }

instrOperandTypes :: DT.InstructionDescriptor -> [DT.OperandType]
instrOperandTypes idesc = map DT.opType (DT.idInputOperands idesc ++ DT.idOutputOperands idesc)

-- | Given an iclass_sect, process the regdiagram child to obtain a bitpattern to
-- match.
iclassMatchPattern :: X.Element -> XML [BT.Bit]
iclassMatchPattern iclass_sect = do
  rd <- getChild "regdiagram" iclass_sect
  let boxes = X.findChildren (qname "box") rd
  mask <- fmap concat $ forM boxes $ \box -> do
    let width = maybe 1 read (X.findAttr (qname "width") box)
        cs = X.findChildren (qname "c") box
    boxMask <- fmap concat $ forM cs $ \c -> do
      let colspan = maybe 1 read (X.findAttr (qname "colspan") c)
      case X.strContent c of
        "1" -> return $ [BT.ExpectedBit True]
        "(1)" -> return $ [BT.ExpectedBit True]
        "0" -> return $ [BT.ExpectedBit False]
        "(0)" -> return $ [BT.ExpectedBit False]
        _ -> return $ replicate colspan BT.Any
    if length boxMask == width
      then return boxMask
      else E.throw $ InvalidAttr "width" box
  return mask

-- | Get all the named fields from an iclass. This includes some bits that are
-- operand bits (things like "opc"), and some that are used to disambiguate the
-- instruction ("Rn", "cond", "S").
iclassFields :: X.Element -> XML [Field]
iclassFields iclass_sect = do
  rd <- getChild "regdiagram" iclass_sect
  let boxes = X.findChildren (qname "box") rd
  fields <- fmap catMaybes $ forM boxes $ \box -> do
    let width = maybe 1 read (X.findAttr (qname "width") box)
    case X.findAttr (qname "usename") box of
      Just "1" -> do
        name <- getAttr "name" box
        hibit <- read <$> getAttr "hibit" box
        return $ Just $ Field { fieldName = name
                              , fieldHibit = hibit
                              , fieldWidth = width
                              }
      Nothing -> return Nothing
  return fields

-- NOTE: all decode_constraints have op="!=".
-- | Given a precomputed field list and an iclass, constraint all the negative bit
-- patterns from the <decode_constraint> element of the iclass.
--
-- Each <decode_constraint> looks like this:
--   <decode_constraint name="fld_1:...:fld_n" op="!=" val="a0a1...am" />
-- where each ai is a single digit, and m is equal to the sum of all the field widths
-- of fld_1...fld_n. We compute the overall bit pattern by starting with an open bit
-- pattern (all "Any"s) and for each field we encounter, setting that bit pattern
-- according to the corresponding sequence of "val"s.
iclassNegPatterns :: [Field]
                     -- ^ List of all fields from regdiagram
                  -> X.Element
                     -- ^ iclass
                  -> XML [[BT.Bit]]
iclassNegPatterns flds iclass_sect = do
  -- Find all decode_constraint elements
  constraints <- case X.findChild (qname "decode_constraints") iclass_sect of
    Just constraints -> return $ X.findChildren (qname "decode_constraint") constraints
    Nothing -> return [] -- no constraints
  -- For each one, compute the corresponding bit pattern
  forM constraints $ \constraint -> do
    names <- LS.splitOn ":" <$> getAttr "name" constraint
    constraintFields <- traverse (lookupField iclass_sect flds) names
    valStr <- getAttr "val" constraint
    valPattern <- case traverse charToBit valStr of
      Nothing -> E.throw (InvalidPattern valStr)
      Just pat -> return pat
    computePattern constraintFields valPattern (replicate 32 BT.Any)

lookupField :: X.Element -> [Field] -> String -> XML Field
lookupField iclass_sect flds name = case find (\fld -> fieldName fld == name) flds of
  Nothing -> E.throw $ MissingField iclass_sect name
  Just fld -> return fld

placeAt :: Int -> [a] -> [a] -> [a]
placeAt ix subList l =
  let (prefix, rst) = splitAt ix l
      suffix = drop (length subList) rst
  in prefix ++ subList ++ suffix

charToBit :: Char -> Maybe BT.Bit
charToBit '1' = Just $ BT.ExpectedBit True
charToBit '0' = Just $ BT.ExpectedBit False
charToBit 'x' = Just $ BT.Any
charToBit _   = Nothing

data Constraint = Constraint { constraintName :: String
                             , constraintPattern :: [BT.Bit]
                             }

data Field = Field { fieldName :: String
                   , fieldHibit :: Int
                   , fieldWidth :: Int
                   }
  deriving (Eq, Show)

-- | Given an iclass and a list of its fields, extract the fields we are going to
-- match against. We return a list of lists because sometimes there are multiple
-- fields concatenated together.
iclassMatchFields :: [Field] -> X.Element -> XML [[Field]]
iclassMatchFields flds iclass_sect = do
  let bitfieldElts = X.filterElements isBitfieldElt iclass_sect
      isBitfieldElt elt = X.qName (X.elName elt) == "th" && X.findAttr (qname "class") elt == Just "bitfields"
      getFields elt = do
        let fieldNames = LS.splitOn ":" (X.strContent elt)
        fields <- traverse (lookupField iclass_sect flds) fieldNames
        return fields
  fields <- traverse getFields bitfieldElts
  return fields

xmlLeaf :: [BT.Bit]
           -- ^ bit pattern to match iclass
        -> [[BT.Bit]]
           -- ^ list of negative bit patterns to rule out iclass
        -> [[Field]]
           -- ^ list of fields we are case-ing over
        -> X.Element
           -- ^ instruction table entry ("leaf")
        -> XML DT.InstructionDescriptor
xmlLeaf iclassPat iclassNegPats bitflds leaf = do
  -- First, gather all *positive* matches and overlay them over the match pattern
  let pats = X.filterChildren (\c -> X.findAttr (qname "class") c == Just "bitfield") leaf
  fldPats <- forM (zip bitflds pats) $ \(flds, fldElt) -> do
    let fldWidth = sum (fieldWidth <$> flds)
    case X.strContent fldElt of
      "" -> return $ replicate fldWidth BT.Any
      s | Just antiPat <- stripPrefix "!= " s -> return $ replicate fldWidth BT.Any
        | otherwise -> case traverse charToBit s of
            Just pat -> return pat
            Nothing -> E.throw $ InvalidPattern s
  leafMatchPat <- computeMatchPattern bitflds fldPats iclassPat
  -- Next, gather all *negative* matches and gather them into individual negative
  -- patterns
  leafNegPats <- fmap catMaybes $ forM (zip bitflds pats) $ \(flds, fldElt) -> do
    let fldWidth = sum (fieldWidth <$> flds)
    case X.strContent fldElt of
      s | Just antiPatStr <- stripPrefix "!= " s -> case traverse charToBit antiPatStr of
            Just antiPat -> do
              fullAntiPat <- computePattern flds antiPat (replicate 32 BT.Any)
              return $ Just fullAntiPat
            Nothing -> E.throw $ InvalidPattern s
      _ -> return Nothing
  mnemonic <- leafMnemonic leaf
  operands <- leafOperandDescriptors leaf
  return $ DT.InstructionDescriptor
    { DT.idMask = concat (reverse (LS.chunksOf 8 leafMatchPat))
    , DT.idNegMasks = (concat . reverse . LS.chunksOf 8) <$> nub (iclassNegPats ++ leafNegPats)
    , DT.idMnemonic = mnemonic
    , DT.idInputOperands = operands
    , DT.idOutputOperands = []
    , DT.idNamespace = ""
    , DT.idDecoderNamespace = ""
    , DT.idAsmString = ""
    , DT.idPseudo = False
    , DT.idDefaultPrettyVariableValues = []
    , DT.idPrettyVariableOverrides = []
    }
  -- FIXME: below is a fold, so we should be able to rewrite it as such
  where computeMatchPattern :: [[Field]] -> [[BT.Bit]] -> [BT.Bit] -> XML [BT.Bit]
        computeMatchPattern [] [] fullPat = return fullPat
        computeMatchPattern (flds:rstFlds) (pat:fldPats) fullPat = do
          fullPat' <- computePattern flds pat fullPat
          computeMatchPattern rstFlds fldPats fullPat'
        computeMatchPattern _ _ _ = error "computeMatchPattern"

-- | Given a leaf, open up the referenced file to get the corresponding iclass.
leaf_iclass :: X.Element -> XML X.Element
leaf_iclass leaf = do
  dirPath <- do
    mXmlDir <- MS.gets xmlPath
    case mXmlDir of
      Just dir -> return dir
      Nothing -> error "BAD"
  filePath <- case X.findAttr (qname "iformfile") leaf of
    Nothing -> E.throw $ MissingAttr "iformfile" leaf
    Just filePath -> return filePath
  let fullFilePath = dirPath ++ "/" ++ filePath
  fileStr <- MS.liftIO $ SIO.readFile fullFilePath
  xmlElement <- case X.parseXMLDoc fileStr of
    Just c -> return c
    Nothing -> E.throw $ InvalidXmlFile fullFilePath
  encName <- case X.findAttr (qname "encname") leaf of
    Just encName -> return encName
    Nothing -> do
      iformname <- case X.filterChild (\c -> X.findAttr (qname "class") c == Just "iformname") leaf of
        Nothing -> E.throw $ MnemonicError "no iformname" leaf
        Just iformnameElt -> case X.findAttr (qname "iformid") iformnameElt of
          Nothing -> E.throw $ MnemonicError "no iformid" leaf
          Just iformname -> return iformname
      label <- case X.findAttr (qname "label") leaf of
        Nothing -> E.throw $ MnemonicError "no label" leaf
        Just label -> return label
      return $ iformname ++ "_" ++ label
  let iclasses = X.findElements (qname "iclass") xmlElement
      matchingIclass iclass = let encodingElts = X.findChildren (qname "encoding") iclass
                                  correctEncoding encElt =
                                    X.findAttr (qname "name") encElt == Just encName
                                  matchingLabel = X.findAttr (qname "name") iclass ==
                                                  X.findAttr (qname "label") leaf
                              in any correctEncoding encodingElts || matchingLabel
  iclass <- case filter matchingIclass iclasses of
    [iclass] -> return iclass
    [] -> E.throw $ MnemonicError "no matching iclass" leaf
    _  -> E.throw $ MnemonicError "multiple matching iclasses" leaf
  return iclass

-- | Given a leaf element, open up the referenced file to discover the correct
-- mnemonic.
leafMnemonic :: X.Element -> XML String
leafMnemonic leaf = do
  iclass <- leaf_iclass leaf
  psname <- case X.findAttr (qname "psname") =<< X.findChild (qname "regdiagram") iclass of
    Just psname -> return psname
    Nothing -> E.throw $ MnemonicError "no psname" leaf
  case P.runParser nameParser "" psname of
    Left _ -> E.throw $ MnemonicError "psname parse error" leaf
    Right nm -> processName nm

  where processName :: String -> XML String
        processName nm = do
          names <- MS.gets usedNames
          case M.lookup nm names of
            Nothing -> do
              MS.modify' $ \s -> s { usedNames = M.insert nm 0 (usedNames s) }
              return nm
            Just nUses -> do
              MS.modify' $ \s -> s { usedNames = M.insertWith (\_ oldCount -> oldCount + 1) nm 0 (usedNames s) }
              return (printf "%s_%d" nm nUses)

leafOperandDescriptors :: X.Element -> XML [DT.OperandDescriptor]
leafOperandDescriptors leaf = do
  iclass <- leaf_iclass leaf
  rd <- case X.findChild (qname "regdiagram") iclass of
    Nothing -> E.throw $ MissingChildElement "regdiagram" iclass
    Just regdiagram -> return regdiagram
  let boxes = X.findElements (qname "box") rd
  descs <- fmap catMaybes $ forM boxes $ \box -> case X.findAttr (qname "usename") box of
    Just "1" -> do
      name <- case X.findAttr (qname "name") box of
        Nothing -> E.throw $ MissingAttr "name"box
        Just name -> return name
      hibit <- case X.findAttr (qname "hibit") box of
        Nothing -> E.throw $ MissingAttr "hibit" box
        Just s -> return $ read s
      let boxWidth = read $ fromMaybe "1" (X.findAttr (qname "width") box)
      let desc = DT.OperandDescriptor { DT.opName = name
                                      , DT.opChunks = [( DT.IBit (hibit - boxWidth + 1)
                                                       , PT.OBit 0
                                                       , fromIntegral boxWidth)]
                                      , DT.opType = DT.OperandType (printf "bv%d" boxWidth)
                                      }
      return $ Just desc
    Nothing -> return Nothing
  return descs

type Parser = P.Parsec Void String

nameParser :: Parser String
nameParser = do
  _ <- P.chunk "aarch32/instrs/"
  instrName <- P.takeWhileP Nothing (/= '/')
  _ <- P.chunk "/"
  encName <- P.takeWhileP Nothing (/= '.')
  _ <- P.chunk ".txt"
  return $ instrName <> "_" <> encName

-- | Given a list of fields, and a bit pattern whose length is the same as the sum of
-- all the field lengths, return a "full" bit pattern reflecting the effect of
-- combining all those patterns together.
computePattern :: [Field] -> [BT.Bit] -> [BT.Bit] -> XML [BT.Bit]
computePattern [] [] fullPat = return fullPat
computePattern (fld : rstFlds) pat fullPat | length pat >= fieldWidth fld = do
  let (fldPat, rstPat) = splitAt (fieldWidth fld) pat
      fullPat' = placeAt (31 - fieldHibit fld) fldPat fullPat
  computePattern rstFlds rstPat fullPat'
computePattern flds pat _ = E.throw $ MismatchingFieldLengths flds pat

getChild :: String -> X.Element -> XML X.Element
getChild name elt = case X.findChild (qname name) elt of
  Nothing -> E.throw $ MissingChildElement name elt
  Just child -> return child

getAttr :: String -> X.Element -> XML String
getAttr name elt = case X.findAttr (qname name) elt of
  Nothing -> E.throw $ MissingAttr name elt
  Just attr -> return attr
