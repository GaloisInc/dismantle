{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE MultiWayIf #-}

module Dismantle.ARM
  ( loadXML
  , XMLException(..)
  , DT.ISA(..)
  , DT.Endianness(..)
  , DT.OperandPayload(..)
  , DT.FormOverride(..)
  , DT.InstFieldDescriptor(..)
  , DT.UnusedBitsPolicy(..)
  ) where

import           Control.Applicative ( (<|>) )
import qualified Control.Exception as E
import           Control.Monad ( forM, void, when, unless, foldM )
import qualified Control.Monad.Fail as MF
import qualified Control.Monad.State.Strict as MS
import           Data.List (stripPrefix, find, nub, intersect, (\\) )
import           Data.List.Split as LS
import qualified Data.Map as M
import           Data.Maybe (catMaybes, fromMaybe, isJust)
import qualified Data.Set as S
import           Data.Void (Void)
import qualified System.IO.Strict as SIO
import qualified Text.Megaparsec as P
import qualified Text.Megaparsec.Char as P
import           Text.Printf (printf)
import qualified Text.XML.Light as X

import qualified Dismantle.Tablegen as DT
import qualified Dismantle.Tablegen.ByteTrie as BT
import qualified Dismantle.Tablegen.Parser.Types as PT

import Debug.Trace

data XMLException = MissingChildElement String X.Element
                  | NoMatchingChildElement X.Element
                  | MissingAttr String X.Element
                  | MissingField X.Element String
                  | MissingFieldForRegister RegisterInfo String
                  | InvalidChildElement String X.Element
                  | InvalidAttr String X.Element
                  | MultipleChildElements String X.Element
                  | MnemonicError String X.Element
                  | InvalidPattern String
                  | MismatchingFieldLengths [Field] [BT.Bit]
                  | InvalidXmlFile String
                  | InnerParserFailure String String
                  | UnexpectedAttributeValue String String

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
           , MF.MonadFail
           )


data InstructionLeaf = InstructionLeaf { ileafFull :: X.Element -- entire leaf
                                       , ileafiClass :: X.Element -- iclass
                                       , ileafSource :: X.Element -- the leaf in the encodings file
                                       , ileafSourceFile :: String -- name of the encodings file
                                       }


data NameUsage = NameUsage { numUses :: Int
                           , masksWithNames :: [([BT.Bit], String)]
                           }

data XMLState = XMLState { usedNames :: M.Map String NameUsage
                         , xmlPath :: Maybe FilePath
                         , xmlSubPath :: Maybe FilePath
                         }

runXML :: XML a -> IO a
runXML (XML a) = MS.evalStateT a (XMLState M.empty Nothing Nothing)

qname :: String -> X.QName
qname str = X.QName str Nothing Nothing

-- | Given a path to the directory containing all XML instruction files, build an ISA
-- descriptor.
loadXML :: (X.Element -> Bool) -> String -> FilePath -> FilePath -> IO DT.ISADescriptor
loadXML fltr arch encFileName dirPath = runXML $ do
  MS.modify' $ \st -> st { xmlPath = Just dirPath }
  loadSingleXML fltr (dirPath ++ "/" ++ encFileName)

loadSingleXML :: (X.Element -> Bool) -> FilePath -> XML DT.ISADescriptor
loadSingleXML fltr fullPath = do
  -- read file
  fileStr <- MS.liftIO $ SIO.readFile fullPath

  -- parse as XML
  xmlElement <- case X.parseXMLDoc fileStr of
    Just c -> return c
    Nothing -> E.throw $ InvalidXmlFile fullPath

  -- load instructions
  loadInstrs fltr xmlElement

loadInstrs :: (X.Element -> Bool) -> X.Element -> XML DT.ISADescriptor
loadInstrs fltr xmlElement = do
  -- format as instructions
  let iclass_sects = filter fltr (X.findElements (qname "iclass_sect") xmlElement)
  instrs <- fmap (catMaybes . concat) $ forM iclass_sects $ \iclass_sect -> do
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
    descs <- forM leaves $ \leaf -> do
      instructionLeaf <- load_leaf leaf
      xmlLeaf matchPattern negPatterns matchFlds instructionLeaf
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
        "0" -> return $ [BT.ExpectedBit False]
        -- If we see (1) or (0), interpret it as "any"
        -- "(1)" -> return $ [BT.ExpectedBit True]
        -- "(0)" -> return $ [BT.ExpectedBit False]
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
    nameAttr <- getAttr "name" constraint
    names <- nameExps nameAttr

    constraintFields <- traverse (lookupField iclass_sect flds) names
    valStr <- getAttr "val" constraint
    valPattern <- case traverse charToBit valStr of
      Nothing -> E.throw (InvalidPattern valStr)
      Just pat -> return pat

    computePattern constraintFields valPattern (replicate 32 BT.Any)

lookupField :: X.Element -> [Field] -> NameExp -> XML Field
lookupField iclass_sect flds nexp =
  case nexp of
    NameExpString name ->
      case find (\fld -> fieldName fld == name) flds of
        Nothing -> E.throw $ MissingField iclass_sect name
        Just fld -> return fld

    NameExpSlice subF hi lo -> do
      fld <- lookupField iclass_sect flds subF
      pure $ fld { fieldHibit = fieldHibit fld + hi
                 , fieldWidth = hi - lo
                 }

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

nameExps :: String -> XML [NameExp]
nameExps ns =
  case P.parseMaybe nameExpsParser ns of
    Nothing -> E.throw (InvalidPattern ns)
    Just ns -> pure ns

nameExpsParser :: P.Parsec Void String [NameExp]
nameExpsParser = P.sepBy nameExpParser (P.single ':')

nameExpParser :: P.Parsec Void String NameExp
nameExpParser = do
    name   <- NameExpString <$> parseName
    slices <- P.many $ parseSlice
    pure $ case slices of
      [] -> name
      [(hi, lo)] -> NameExpSlice name hi lo
      _ -> name  -- FIXME: this compensates for the errors in the XML
                 --        ex. cond<3:1><3:1>

  where
    -- TODO: whitespace?
    parseSlice = do
      P.single '<'
      hi <- parseInt
      P.single ':'
      lo <- parseInt
      P.single '>'
      pure (hi, lo)

    parseName = P.many (P.alphaNumChar P.<|> P.single '_')
    parseInt = read <$> P.many P.digitChar

data NameExp =
    NameExpString String
  | NameExpSlice  NameExp Int Int
  deriving(Show)

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
        fieldNames <- nameExps (X.strContent elt)
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
        -> InstructionLeaf
           -- ^ instruction table entry ("leaf")
        -> XML (Maybe DT.InstructionDescriptor)
xmlLeaf iclassPat iclassNegPats bitflds ileaf = do
  let leaf = ileafSource ileaf
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
  operandFlds <- leafFields ileaf
  operands <- leafGetOperands ileaf operandFlds
  -- leafMatchPat <- removeOperands operandFlds <$> computeMatchPattern bitflds
  -- fldPats iclassPat
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
  mMnemonic <- leafMnemonic leafMatchPat ileaf
  case mMnemonic of
    Just mnemonic -> do
      let desc = DT.InstructionDescriptor
            { DT.idMask = concat (reverse (LS.chunksOf 8 leafMatchPat))
            , DT.idNegMasks = (concat . reverse . LS.chunksOf 8) <$> nub (iclassNegPats ++ leafNegPats)
            -- { DT.idMask = leafMatchPat -- concat (reverse (LS.chunksOf 8 leafMatchPat))
            -- , DT.idNegMasks = iclassNegPats ++ leafNegPats -- (concat . reverse . LS.chunksOf 8) <$> nub (iclassNegPats ++ leafNegPats)
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
      return $ Just desc
    Nothing -> return Nothing
  -- FIXME: below is a fold, so we should be able to rewrite it as such
  where computeMatchPattern :: [[Field]] -> [[BT.Bit]] -> [BT.Bit] -> XML [BT.Bit]
        computeMatchPattern [] [] fullPat = return fullPat
        computeMatchPattern (flds:rstFlds) (pat:fldPats) fullPat = do
          fullPat' <- computePattern flds pat fullPat
          computeMatchPattern rstFlds fldPats fullPat'
        computeMatchPattern _ _ _ = error "computeMatchPattern"

        removeOperands :: [Field] -> [BT.Bit] -> [BT.Bit]
        removeOperands [] pat = pat
        removeOperands (fld:rstFlds) pat =
          removeOperands rstFlds (placeAt (31 - fieldHibit fld) (replicate (fieldWidth fld) BT.Any) pat)

parseEncList :: String -> [String]
parseEncList = LS.splitOn ", "



flatText :: X.Element -> String
flatText e = concat $ map flatContent (X.elContent e)
  where
    flatContent :: X.Content -> String
    flatContent (X.Text str) = X.cdData $ str
    flatContent (X.Elem e) = flatText e

parseElement :: Parser a -> X.Element -> XML a
parseElement p e = do
  let txt = flatText e
  case P.runParser p "" txt of
    Left err -> E.throw $ InnerParserFailure (show err) txt
    Right a -> return a


-- | Build operand descriptors out of the given fields
leafGetOperands :: InstructionLeaf -> [Field] -> XML [DT.OperandDescriptor]
leafGetOperands ileaf allfields = do
  let iclass = ileafiClass ileaf

  targetEncodings <- forChildren "encoding" iclass $ \encoding -> do
    getAttr "name" encoding

  explanations <- getChild "explanations" (ileafFull ileaf)
  subPath <- MS.gets xmlSubPath
  rawRegisters <- catMaybes <$> (forChildren "explanation" explanations $ \explanation -> do
    encs <- parseEncList <$> getAttr "enclist" explanation
    if | not $ null $ intersect encs targetEncodings
       , Just account <- X.findChild (qname "account") explanation -> do
         para <- getChild "intro" account >>= getChild "para"
         parseElement registerInfoParser para >>= \case
           Just rinfo -> do
             encodedin <- getAttr "encodedin" account
             symbol <- getChild "symbol" explanation
             symbolName <- parseElement registerNameParser symbol
             return $ Just (symbolName, (encodedin, rinfo))
           _ -> return Nothing
        | otherwise -> return Nothing)

  let processRegister (symbolName, (encodedin, rinfo)) = do
        unless (regIndexMode rinfo == IndexBasic || null encodedin) $
          E.throw $ UnexpectedAttributeValue "encodedin" encodedin
        case regIndexMode rinfo of
          IndexVector fieldNames -> do
            fields <- forM fieldNames $ \fieldName -> do
              lookupField (ileafSource ileaf) allfields (NameExpString fieldName)
            return $ (symbolName, rinfo, fields)
          IndexRegisterOffset baseregister -> do
            case lookup baseregister rawRegisters of
              Just rawinfo -> do
                (_, _, fields) <- processRegister (baseregister, rawinfo)
                return $ (symbolName, rinfo, fields)
              Nothing -> E.throw $ MissingField iclass baseregister
          IndexBasic -> do
            fieldDescription <- case regMode rinfo of
              ImplicitEncoding -> do
                unless (null encodedin || encodedin == symbolName) $ do
                  E.throw $ UnexpectedAttributeValue "encodedin" encodedin
                return symbolName
              _ -> return encodedin
            exps <- nameExps fieldDescription
            fields <- forM exps $ lookupField (ileafSource ileaf) allfields
            return $ (symbolName, rinfo, fields)

  rinfos <- mapM processRegister rawRegisters

  let usedFields = concat $ map (\(_, _, fields) -> fields) rinfos
  let unusedFields = allfields \\ usedFields

  registerOps <- forM rinfos $ \(name, rinfo, fields) -> do
    chunks <- forM fields $ \(Field name hibit width) -> do
      return (DT.IBit (hibit - width + 1), PT.OBit 0, fromIntegral width)
    opTypeBase <- case regKind rinfo of
      GPR -> return "GPR"
      SIMDandFP -> return "SIMDandFP"

    let opType =  DT.OperandType opTypeBase

    return $ DT.OperandDescriptor { DT.opName = name
                                  , DT.opChunks = chunks
                                  , DT.opType = opType
                                  }

  immediates <- forM unusedFields $ \(Field name hibit width) ->
    return $ DT.OperandDescriptor { DT.opName = name
                                  , DT.opChunks = [( DT.IBit (hibit - width + 1)
                                                   , PT.OBit 0
                                                   , fromIntegral width)]
                                  , DT.opType = DT.OperandType (printf "Bv%d" width)
                                  }
  return $ registerOps ++ immediates

-- | Given a leaf, get the entire referenced file
load_leaf :: X.Element -> XML InstructionLeaf
load_leaf leaf = do
  dirPath <- do
    mXmlDir <- MS.gets xmlPath
    case mXmlDir of
      Just dir -> return dir
      Nothing -> error "BAD"
  filePath <- case X.findAttr (qname "iformfile") leaf of
    Nothing -> E.throw $ MissingAttr "iformfile" leaf
    Just filePath -> return filePath
  MS.modify $ \s -> s { xmlSubPath = Just filePath }
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
  return $ InstructionLeaf xmlElement iclass leaf filePath


-- | Given a leaf element, open up the referenced file to discover the correct
-- mnemonic.
leafMnemonic :: [BT.Bit] -> InstructionLeaf -> XML (Maybe String)
leafMnemonic matchPat ileaf = do
  let iclass = ileafiClass ileaf
  let leaf = ileafSource ileaf
  psname <- case X.findAttr (qname "psname") =<< X.findChild (qname "regdiagram") iclass of
    Just psname -> return psname
    Nothing -> E.throw $ MnemonicError "no psname" leaf
  case P.runParser nameParser "" psname of
    Left _ -> E.throw $ MnemonicError "psname parse error" leaf
    Right nm -> processName nm

  where processName :: String -> XML (Maybe String)
        processName nm = do
          names <- MS.gets usedNames
          case M.lookup nm names of
            Nothing -> do
              let nameUsage = NameUsage { numUses = 0
                                        , masksWithNames = [(matchPat, nm)]
                                        }
              MS.modify' $ \s -> s { usedNames = M.insert nm nameUsage (usedNames s) }
              return $ Just nm
            Just nameUsage -> do
              case lookup matchPat (masksWithNames nameUsage) of
                Just _ -> return Nothing
                Nothing -> do
                  let newName = printf "%s_%d" nm (numUses nameUsage)
                      nameUsage' = NameUsage { numUses = numUses nameUsage + 1
                                             , masksWithNames = masksWithNames nameUsage ++ [(matchPat, newName)]
                                             }
                  MS.modify' $ \s -> s { usedNames = M.insert nm nameUsage' (usedNames s) }
                  return $ Just newName

leafFields :: InstructionLeaf -> XML [Field]
leafFields ileaf = do
  let iclass = ileafiClass ileaf
  rd <- case X.findChild (qname "regdiagram") iclass of
    Nothing -> E.throw $ MissingChildElement "regdiagram" iclass
    Just regdiagram -> return regdiagram
  let boxes = X.findElements (qname "box") rd
  fmap catMaybes $ forM boxes $ \box -> case X.findAttr (qname "usename") box of
    Just "1" -> do
      name <- case X.findAttr (qname "name") box of
        Nothing -> E.throw $ MissingAttr "name"box
        Just name -> return name
      hibit <- case X.findAttr (qname "hibit") box of
        Nothing -> E.throw $ MissingAttr "hibit" box
        Just s -> return $ read s
      let boxWidth = read $ fromMaybe "1" (X.findAttr (qname "width") box)

          fld = Field { fieldName = name
                      , fieldHibit = hibit
                      , fieldWidth = boxWidth
                      }
      return $ Just fld
    Nothing -> return Nothing

forChildrenWithAttr :: String -> X.Element -> (X.Element -> String -> XML a) -> XML [a]
forChildrenWithAttr aname elt m = catMaybes <$> (forM (X.elChildren elt) $ \child -> do
  case X.findAttr (qname aname) child of
    Just v -> Just <$> m child v
    Nothing -> return Nothing)

forChildren :: String -> X.Element -> (X.Element -> XML a) -> XML [a]
forChildren name elt m = mapM m $ X.filterChildrenName ((==) $ qname name) elt

type Parser = P.Parsec Void String

data RegisterDirection = Input | Output | InputOutput
  deriving (Ord, Eq, Show)

data RegisterKind = GPR | SIMDandFP
  deriving (Ord, Eq, Show)

data RegisterMode = Data | Accumulator | Base | Index | ImplicitEncoding | NoMode
  deriving (Ord, Eq, Show)

data RegisterIndexMode = IndexBasic | IndexRegisterOffset String | IndexVector [String]
  deriving (Ord, Eq, Show)

data RegisterInfo =
  RegisterInfo { regKind :: RegisterKind
               , regMode :: RegisterMode
               , regDirection :: RegisterDirection
               , regIndexMode :: RegisterIndexMode -- for a logical register, how is it computed
                                                   -- from the operand fields
               }
  deriving (Ord, Eq, Show)


registerNameParser :: Parser String
registerNameParser = do
  P.char '<'
  name <- P.takeWhile1P Nothing (/= '>')
  P.char '>'
  return name

-- | Scrape the register tooltip to determine which operands are used
-- as registers. The parser fails gracefully (returning 'Nothing') if
-- the tooltip is recognized, but not specifying a register. The parser
-- fails if the tooltip is unrecognized.

registerInfoParser :: Parser (Maybe RegisterInfo)
registerInfoParser = do
  P.optional $ encodingPreamble
  ws
  dropWords ["Is", "is", "An", "an", "The", "the", "optional", "a ", "Specifies", "If present,"]
  ws
  P.choice $
    (map (\s -> P.chunk s >> return Nothing) knownPrefixes)
    ++ [P.try knownLiteralPrefix >> return Nothing]
    ++ [Just <$> realRegisterInfoParser]
  where
    bitSizeStrings :: [String]
    bitSizeStrings =
      [ "3-bit", "4-bit", "5-bit", "6-bit", "8-bit", "12-bit", "16-bit", "24-bit", "32-bit", "64-bit", "128-bit" ]

    dropWords :: [String] -> Parser ()
    dropWords words =
      void $ P.optional $ P.many $
        (P.choice $ map (\w -> P.chunk w >> return ()) words) <|> (P.char ' ' >> return ())


    encodingPreamble :: Parser ()
    encodingPreamble = do
      P.chunk "For"
      P.many $ P.choice $ map P.chunk
        [ "half-precision"
        , "single-precision"
        , "double-precision"
        , "64-bit SIMD"
        , "128-bit SIMD"
        , "offset"
        , "or"
        , "the"
        , "vector"
        , "scalar"
        , "post-indexed"
        , "pre-indexed"
        , "encoding"
        , "variant"
        , "variants"
        , " "
        ]

      P.takeWhileP Nothing (/= ':')
      P.char ':'
      return ()

    bitSize :: Parser String
    bitSize = P.choice (map P.chunk bitSizeStrings)

    knownLiteralPrefix :: Parser ()
    knownLiteralPrefix = do
      P.optional $ englishNumber
      ws
      P.optional $ bitSize
      ws
      P.optional $ P.chunk "unsigned"
      ws
      void $ P.chunk "immediate"

    ws :: Parser ()
    ws = P.takeWhileP Nothing (== ' ') >> return ()

    englishNumber :: Parser Integer
    englishNumber =
      P.choice
        [ P.chunk "first" >> return 1
        , P.chunk "second" >> return 2
        , P.chunk "third" >> return 3
        , P.chunk "fourth" >> return 4
        , P.chunk "fifth" >> return 5
        ]

    realRegisterInfoParser :: Parser (RegisterInfo)
    realRegisterInfoParser = do
      P.optional $ englishNumber
      ws
      P.optional $ bitSize
      ws
      P.optional $ P.chunk "name of the"
      ws
      P.optional $ englishNumber
      ws
      (rkind, impliedDirection, impliedMode) <- kindParser
      ws
      rmode <- modeParser impliedMode
      ws
      rdir <- directionParser impliedDirection
      ws
      indexMode <- indexModeParser rkind
      return (RegisterInfo rkind rmode rdir indexMode)
      where
        kindParser :: Parser (RegisterKind, Maybe RegisterDirection, Maybe RegisterMode)
        kindParser =
          P.choice
            [ P.chunk "general-purpose" >> return (GPR, Nothing, Nothing)
            , P.chunk "Arm source" >> return (GPR, Just Input, Just ImplicitEncoding)
            , P.chunk "source general-purpose" >> return (GPR, Just Input, Just ImplicitEncoding)
            , P.chunk "destination general-purpose" >> return (GPR, Just Output, Just ImplicitEncoding)
            , P.chunk "SIMD&FP" >> return (SIMDandFP, Nothing, Nothing)
            ]

        modeParser :: Maybe RegisterMode -> Parser RegisterMode
        modeParser (Just impliedMode) = return impliedMode
        modeParser Nothing =
          P.choice
            [ P.chunk "data" >> return Data
            , P.chunk "accumulator" >> return Accumulator
            , P.chunk "base" >> return Base
            , P.chunk "index" >> return Index
            , return NoMode
            ]

        directionParser :: Maybe RegisterDirection -> Parser RegisterDirection
        directionParser (Just impliedDirection) = P.chunk "register" >> return impliedDirection
        directionParser Nothing =
          P.choice
              [ P.chunk "destination register" >> return Output
              , P.chunk "register to be transferred" >> return Input
              , P.chunk "source register" >> return Input
              , P.chunk "input register" >> return Input
              , P.chunk "output register" >> return Output
              , P.chunk "register holding address to be branched to" >> return Input
              , P.chunk "register to be accessed" >> return Input
              , P.chunk "register into which the status result of store exclusive is written" >> return Output
              , P.chunk "destination and source register" >> return InputOutput
              , P.chunk "destination and second source register" >> return InputOutput
              , P.chunk "source and destination register" >> return InputOutput
              , P.chunk "register" >> return InputOutput
              ]
        indexModeParser :: RegisterKind -> Parser RegisterIndexMode
        indexModeParser SIMDandFP = do
          P.takeWhileP Nothing (/= '.')
          P.choice
            [ vectorIndexModeParser
            , oneOffsetIndexModeParser
            , return IndexBasic
            ]
        indexModeParser _ = return IndexBasic

        oneOffsetIndexModeParser :: Parser RegisterIndexMode
        oneOffsetIndexModeParser = do
          P.chunk ". This is the next SIMD&FP register after <"
          nm <- P.takeWhile1P Nothing (/= '>')
          return $ IndexRegisterOffset nm

        vectorIndexModeParser :: Parser RegisterIndexMode
        vectorIndexModeParser = do
          P.chunk ". If <dt> is "
          P.takeWhileP Nothing (/= ',')
          P.chunk ", Dm is restricted to D0-D7. Dm is encoded in \"Vm<2:0>\", and x is encoded in \"M:Vm<3>\". If <dt> is "
          P.takeWhileP Nothing (/= ',')
          P.chunk ", Dm is restricted to D0-D15. Dm is encoded in \"Vm\", and x is encoded in \"M\"."
          return $ IndexVector ["size", "M", "Vm"]


knownPrefixes :: [String]
knownPrefixes =
  [ "shift amount", "one of:", "label of", "bit number", "width of the"
  , "shift to apply", "number of the", "destination vector for a doubleword operation"
  , "sequence of one or more of the following", "When <size> ==", "data type"
  , "constant of the specified type"
  , "location of the extracted result in the concatenation of the operands"
  , "alignment", "suffix"
  , "See Standard assembler syntax fields"
  , "see Standard assembler syntax fields"
  , "base register writeback"
  , "limitation on the barrier operation"
  , "stack pointer"
  , "index register is added to or subtracted from the base register"
  , "index register is added to the base register"
  , "offset is added to the base register"
  , "positive unsigned immediate byte offset"
  , "program label"
  , "condition"
  , "size of the"
  ]
  ++
  [ "least significant", "number of bits", "rotate amount", "bit position"
  , "optional shift amount", "address adjusted", "optional suffix"
  , "mode whose banked sp", "rotation applied to elements"
  , "element index"
  , "immediate offset", "number of fraction bits in"
  , "signed floating-point constant", "data size", "scalar"
  , "endianness to be selected", "number of mode to change to"
  , "sequence of one or more of following, specifying which interrupt mask bits are affected"
  , "unsigned immediate"
  -- fixme: these seem unused in arch32?
  , "system register encoding space"
  , "opc1 parameter within the System register encoding space"
  , "opc2 parameter within the System register encoding space"
  , "CRm parameter within the System register encoding space"
  , "CRn parameter within the System register encoding space"
  -- fixme: vector instructions
  , "vectors containing the table"
  , "destination vector for a quadword operation"
  , "list of one or more registers"
  , "list of consecutively numbered 32-bit SIMD&FP registers to be transferred"
  , "list of consecutively numbered 64-bit SIMD&FP registers to be transferred"
  , "list containing the 64-bit names of the SIMD&FP registers"
  , "list containing the 64-bit names of two SIMD&FP registers"
  , "list containing the 64-bit names of three SIMD&FP registers"
  , "list containing the 64-bit names of four SIMD&FP registers"
  , "list containing the single 64-bit name of the SIMD&FP register holding the element"
  , "list containing the 64-bit names of the two SIMD&FP registers holding the element"
  , "list containing the 64-bit names of the three SIMD&FP registers holding the element"
  , "list containing the 64-bit names of the four SIMD&FP registers holding the element"
  ]

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

getChildWith :: (X.Element -> Bool) -> X.Element -> XML X.Element
getChildWith f elt = case X.filterChild f elt of
  Nothing -> E.throw $ NoMatchingChildElement elt
  Just child -> return child

getChild :: String -> X.Element -> XML X.Element
getChild name elt = case X.findChild (qname name) elt of
  Nothing -> E.throw $ MissingChildElement name elt
  Just child -> return child

getAttr :: String -> X.Element -> XML String
getAttr name elt = case X.findAttr (qname name) elt of
  Nothing -> E.throw $ MissingAttr name elt
  Just attr -> return attr
