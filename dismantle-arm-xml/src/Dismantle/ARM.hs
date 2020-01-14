{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE ScopedTypeVariables #-}

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

import           Prelude hiding (fail)

import           Control.Applicative ( (<|>) )
import qualified Control.Exception as E
import           Control.Monad ( forM, void, when, unless, foldM, (>=>), zipWithM )
import           Control.Monad.Fail ( fail )
import qualified Control.Monad.Fail as MF
import           Control.Monad.Trans ( lift )
import qualified Control.Monad.State as MS
import qualified Control.Monad.Reader as R
import           Control.Monad.Trans.RWS.Strict ( RWST )
import qualified Control.Monad.Trans.RWS.Strict as RWS
import           Data.Either ( partitionEithers )
import           Data.List (stripPrefix, find, nub, intersect, (\\), intercalate, partition, isPrefixOf )
import           Data.List.Split as LS
import qualified Data.Map as M
import           Data.Maybe (catMaybes, fromMaybe, isJust, isNothing)
import qualified Data.Set as S
import           Data.Word ( Word8 )
import           Data.Void (Void)
import qualified System.IO.Strict as SIO
import           System.FilePath.Glob ( namesMatching )
import           System.FilePath ( (</>), (<.>), takeFileName )
import qualified Text.Megaparsec as P
import qualified Text.Megaparsec.Char as P
import           Text.Printf (printf)
import qualified Text.XML.Light as X

import qualified Dismantle.Tablegen as DT
import qualified Dismantle.Tablegen.ByteTrie as BT
import qualified Dismantle.Tablegen.Parser.Types as PT

import Debug.Trace

data InnerXMLException = MissingChildElement String X.Element
                       | NoMatchingChildElement X.Element
                       | MissingAttr String X.Element
                       | MissingField String Fields
                       | MissingEncoding String
                       | MissingFieldForRegister RegisterInfo String
                       | InvalidChildElement String X.Element
                       | InvalidAttr String X.Element
                       | MultipleChildElements String X.Element
                       | MnemonicError String
                       | InvalidPattern String
                       | MismatchedFieldWidth Field Int
                       | MissingRegisterInfo String [String]
                       | MismatchedWidth X.Element Int [QuasiBit]
                       | InvalidXmlFile String
                       | InnerParserFailure String String
                       | UnexpectedAttributeValue String String
                       | InvalidRegisterInfo RegisterInfo
                       | InvalidField X.Element
                       | BitdiffsParseError String String
                       | UnexpectedBitfieldLength [BT.Bit]
                       | NoUniqueiClass String [X.Element]
                       | MissingXMLFile String
                       | InvalidConstraints (PropTree (BitSection QuasiBit))
                       | InvalidPositiveConstraint [BitSection QuasiBit]
                       | InvalidNegativeConstraint [BitSection BT.Bit]
                       | XMLMonadFail String

  deriving Show

data XMLException = XMLException InnerXMLException (Maybe String) (Maybe FilePath)
  deriving Show

instance E.Exception InnerXMLException
instance E.Exception XMLException

throw :: InnerXMLException -> XML a
throw e = do
  env <- R.ask
  E.throw $ XMLException e (xmlCurrentEncoding env) (xmlCurrentFile env)


-- | Monad for keeping track of how many times we've seen a particular mnemonic;
-- since there are duplicates, we need to add qualifiers to the names to distinguish
-- them from each other.
newtype XML a = XML (RWST XMLEnv () XMLState IO a)
  deriving ( Functor
           , Applicative
           , Monad
           , MS.MonadState XMLState
           , R.MonadReader XMLEnv
           , MS.MonadIO
           )

instance MF.MonadFail XML where
  fail msg = throw $ XMLMonadFail msg

data InstructionLeaf = InstructionLeaf { ileafFull :: X.Element -- entire leaf
                                       , ileafiClass :: X.Element -- iclass
                                       }
  deriving Show

-- FIXME: unused currently
data NameUsage = NameUsage { numUses :: Int
                           , masksWithNames :: [([BT.Bit], String)]
                           }
  deriving Show

data XMLState = XMLState { usedNames :: M.Map String NameUsage
                         }
  deriving Show

data XMLEnv = XMLEnv { xmlCurrentFile :: Maybe FilePath
                     , xmlCurrentEncoding :: Maybe String
                     , xmlAllFiles :: [FilePath]
                     , xmlArchName :: String
                     }

runXML :: String -> [FilePath] -> XML a -> IO a
runXML archName allFiles (XML a) = fst <$> RWS.evalRWST a (XMLEnv Nothing Nothing allFiles archName) (XMLState M.empty)

qname :: String -> X.QName
qname str = X.QName str Nothing Nothing

-- | Given a path to the directory containing all XML instruction files, build an ISA
-- descriptor.
loadXML ::  String -> [FilePath] -> IO DT.ISADescriptor
loadXML arch xmlFiles = do
  runXML arch xmlFiles $ do
    instrs <- fmap concat $ forM xmlFiles $ (\f -> withParsedXMLFile f loadInstrs)
    return $ DT.ISADescriptor { DT.isaInstructions = instrs
                              , DT.isaOperands = S.toList (S.fromList (concatMap instrOperandTypes instrs))
                              , DT.isaErrors = []
                              }

withParsedXMLFile :: FilePath -> (X.Element -> XML a) -> XML a
withParsedXMLFile fullPath m = R.local (\e -> e { xmlCurrentFile = Just fullPath }) $ do
  fileStr <- MS.liftIO $ SIO.readFile fullPath
  case X.parseXMLDoc fileStr of
    Just c -> m c
    Nothing -> throw $ InvalidXmlFile fullPath

withEncodingName :: String -> XML a -> XML a
withEncodingName encnm = R.local (\e -> e { xmlCurrentEncoding = Just encnm })


loadInstrs :: X.Element -> XML [DT.InstructionDescriptor]
loadInstrs xmlElement = do
  -- format as instructions
  arch <- R.asks xmlArchName
  case X.findChild (qname "classes") xmlElement of
    Just classes -> do
      fmap concat $ forChildren "iclass" classes $ \iclass -> do
        let leaf = InstructionLeaf xmlElement iclass
        isa <- getAttr "isa" iclass
        if isa == arch && not (isAliasedInstruction leaf) then do
          (fields, qmasks, matchProp) <- iclassFieldsAndProp iclass
          encodings <- leafGetEncodings leaf fields
          operandEncodings <- leafGetOperands leaf fields encodings
          result <- forM operandEncodings $ \(encoding, operands) -> do
            withEncodingName (encName encoding) $ do
              xmlLeaf leaf matchProp fields encoding (operands ++ qmasks)
          return result
        else return []
    _ -> return []

consMaybe :: Maybe a -> [a] -> [a]
consMaybe Nothing as = as
consMaybe (Just a) as = a : as

instrOperandTypes :: DT.InstructionDescriptor -> [DT.OperandType]
instrOperandTypes idesc = map DT.opType (DT.idInputOperands idesc ++ DT.idOutputOperands idesc)


iclassFieldsAndProp :: X.Element -> XML (Fields, [DT.OperandDescriptor], PropTree (BitSection QuasiBit))
iclassFieldsAndProp iclass = do
  rd <- getChild "regdiagram" iclass
  fields <- forChildren "box" rd getBoxField
  let namedFields = filter fieldUseName fields

  return $ ( M.fromListWith mergeMap $ map (\field -> (fieldName field, field)) namedFields
           , quasiMasks fields
           , mkPropList (map fieldConstraint fields))
  where
    mergeMap :: Field -> Field -> Field
    mergeMap field field' = if field == field' then field else
      error $ "Unexpected duplicate field: " ++ (show field)

getBoxField :: X.Element -> XML Field
getBoxField box = do
  let width = maybe 1 read (X.findAttr (qname "width") box)
  hibit <- read <$> getAttr "hibit" box

  constraint <- case X.findAttr (qname "constraint") box of
    Just constraint -> do
      bits <- map Bit <$> parseString (P.chunk "!= " >> P.some bitParser) constraint
      unless (length bits == width) $
        throw $ MismatchedWidth box width bits
      return $ mkPropNegate $ PropLeaf $ BitSection (31 - hibit) bits
    Nothing -> do
      bits <- fmap concat $ forChildren "c" box $ \c -> do
        let content = X.strContent c
        case X.findAttr (qname "colspan") c of
          Just colspan -> do
            unless (null content) $
              throw $ InvalidChildElement "box" box
            return $ replicate (read colspan) (Bit $ BT.Any)
          Nothing | Just qbit <- quasiMaskBit content ->
           return [qbit]
          _ -> throw $ InvalidChildElement "box" box
      unless (length bits == width) $
        throw $ MismatchedWidth box width bits
      return $ PropLeaf $ BitSection (31 - hibit) bits
  let field = Field { fieldName = fromMaybe "" $ X.findAttr (qname "name") box
                    , fieldHibit = hibit
                    , fieldWidth = width
                    , fieldConstraint = constraint
                    , fieldUseName = X.findAttr (qname "usename") box == Just "1"
                    , fieldTypeOverride = Nothing
                    }
  return $ fieldOpTypeOverrides field

-- | A quasibit is either a BT.Bit or a "soft" required bit
data QuasiBit = Bit BT.Bit | QBit Bool
  deriving (Eq, Show)

-- | A quasimask specifies that a given bitpattern is "required" to have predictable
-- behavior, but not to disambiguate instructions.
quasiMaskBit :: String -> Maybe QuasiBit
quasiMaskBit s = case s of
  "1" -> Just $ Bit $ BT.ExpectedBit True
  "0" -> Just $ Bit $ BT.ExpectedBit False
  "x" -> Just $ Bit $ BT.Any
  "" -> Just $ Bit $ BT.Any
  "(1)" -> Just $ QBit True
  "(0)" -> Just $ QBit $ False
  _ -> Nothing

quasiToBit :: QuasiBit -> Maybe BT.Bit
quasiToBit (Bit b) = Just b
quasiToBit _ = Nothing

isQBit :: QuasiBit -> Bool
isQBit = isNothing . quasiToBit

flattenQuasiBit :: QuasiBit -> BT.Bit
flattenQuasiBit qb = fromMaybe BT.Any (quasiToBit qb)

quasiMasks :: [Field] -> [DT.OperandDescriptor]
quasiMasks fields = catMaybes $ map go (zip fields [0..])
  where
    go :: (Field, Int) -> Maybe DT.OperandDescriptor
    go (Field _ hibit width constraint useName _, i) =
      if ((not useName) && any (any isQBit) constraint) then
        Just $ DT.OperandDescriptor { DT.opName = printf "QuasiMask%d" i
                                    , DT.opChunks = [( DT.IBit (hibit - width + 1)
                                      , PT.OBit 0
                                      , fromIntegral width)]
                                    , DT.opType = DT.OperandType (printf "QuasiMask%d" width)
                                    }

      else Nothing
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
-- iclassNegPatterns :: [Field]
--                      -- ^ List of all fields from regdiagram
--                   -> X.Element
--                      -- ^ iclass
--                   -> XML [[BT.Bit]]
-- iclassNegPatterns flds iclass_sect = do
--   -- Find all decode_constraint elements
--   constraints <- case X.findChild (qname "decode_constraints") iclass_sect of
--     Just constraints -> return $ X.findChildren (qname "decode_constraint") constraints
--     Nothing -> return [] -- no constraints
--   -- For each one, compute the corresponding bit pattern
--   forM constraints $ \constraint -> do
--     nameAttr <- getAttr "name" constraint
--     names <- nameExps nameAttr

--     constraintFields <- traverse (lookupField flds) names
--     valStr <- getAttr "val" constraint
--     valPattern <- case traverse charToBit valStr of
--       Nothing -> throw (InvalidPattern valStr)
--       Just pat -> return pat

--     computePattern constraintFields valPattern (replicate 32 BT.Any)

type Fields = M.Map String Field

lookupField :: Fields -> NameExp -> XML Field
lookupField flds nexp =
  case nexp of
    NameExpString name ->
      case M.lookup name flds of
        Nothing -> throw $ MissingField name flds
        Just fld -> return fld

    NameExpSlice subF hi lo -> do
      fld <- lookupField flds subF
      pure $ fld { fieldHibit = fieldHibit fld + hi
                 , fieldWidth = hi - lo
                 }

nameExps :: String -> XML [NameExp]
nameExps ns = parseString nameExpsParser ns

nameExpsParser :: P.Parsec Void String [NameExp]
nameExpsParser = P.sepBy1 nameExpParser (P.single ':')

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

    parseName = P.some (P.alphaNumChar P.<|> P.single '_')
    parseInt = read <$> P.some P.digitChar

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
                   , fieldConstraint :: PropTree (BitSection QuasiBit)
                   , fieldUseName :: Bool
                   , fieldTypeOverride :: Maybe String
                   }
  deriving (Show, Eq)

-- | Given an iclass and a list of its fields, extract the fields we are going to
-- match against. We return a list of lists because sometimes there are multiple
-- fields concatenated together.
-- iclassMatchFields :: [Field] -> X.Element -> XML [[Field]]
-- iclassMatchFields flds iclass_sect = do
--   let bitfieldElts = X.filterElements isBitfieldElt iclass_sect
--       isBitfieldElt elt = X.qName (X.elName elt) == "th" && X.findAttr (qname "class") elt == Just "bitfields"
--       getFields elt = do
--         fieldNames <- nameExps (X.strContent elt)
--         fields <- traverse (lookupField flds) fieldNames
--         return fields
--   fields <- traverse getFields bitfieldElts
--   return fields

endianness :: [a] -> [a]
endianness bits = concat (reverse (LS.chunksOf 8 bits))


xmlLeaf :: InstructionLeaf
           -- ^ instruction table entry ("leaf")
        -> PropTree (BitSection QuasiBit)
           -- ^ instruction matching proposition
        -> Fields
           -- ^ all fields in scope
        -> Encoding
           -- ^ this encoding
        -> [DT.OperandDescriptor]
           -- ^ operands for this encoding
        -> XML DT.InstructionDescriptor
xmlLeaf ileaf matchProps fields encoding operands = do
  namespace <- leafMnemonic' ileaf
  let qencConstraints = fmap (fmap Bit) $ encConstraints encoding

  (encodingMatchPat, encodingNegPats) <- deriveConstrainedMasks (qencConstraints <> matchProps)

  when ("STC" `isPrefixOf ` encName encoding) $ do
    traceM $ prettyEncoding encoding encodingMatchPat encodingNegPats
    return ()

  let desc = DT.InstructionDescriptor
        { DT.idMask = map flattenQuasiBit (endianness encodingMatchPat)
        , DT.idNegMasks = endianness <$> nub encodingNegPats
        , DT.idMnemonic = encName encoding
        , DT.idInputOperands = operands
        , DT.idOutputOperands = []
        , DT.idNamespace = namespace
        , DT.idDecoderNamespace = ""
        , DT.idAsmString = encName encoding ++ "(" ++ prettyMask (endianness encodingMatchPat) ++ ") " ++ simpleOperandFormat operands
        , DT.idPseudo = False
        , DT.idDefaultPrettyVariableValues = []
        , DT.idPrettyVariableOverrides = []
        }
  return $ desc


prettyEncoding :: Encoding -> [QuasiBit] -> [[BT.Bit]] -> String
prettyEncoding encoding matchPat negPats =
  "Encoding: " ++ encName encoding ++ "\n"
    ++ "Mask: \n" ++ go matchPat
    ++ "\nNegative Masks: \n"
    ++ intercalate "\n" (map (go . map Bit) negPats)
  where
    go :: [QuasiBit] -> String
    go bits = "Endian-swapped: \n" ++ prettyMask (endianness bits)
              ++ "\nOriginal: \n" ++ prettyMask bits

prettyMask :: [QuasiBit] -> String
prettyMask qbits = concat $ intercalate ["."] $ map (map go) (LS.chunksOf 8 qbits)
  where
    go :: QuasiBit -> String
    go (Bit BT.Any) = "x"
    go (Bit (BT.ExpectedBit True)) = "1"
    go (Bit (BT.ExpectedBit False)) = "0"
    go (QBit True) = "(1)"
    go (QBit False) = "(0)"


simpleOperandFormat :: [DT.OperandDescriptor] -> String
simpleOperandFormat descs = intercalate ", " $ map go descs
  where
    go :: DT.OperandDescriptor -> String
    go desc = DT.opName desc ++ " ${" ++ DT.opName desc ++ "}"

parseEncList :: String -> [String]
parseEncList = LS.splitOn ", "



flatText :: X.Element -> String
flatText e = concat $ map flatContent (X.elContent e)
  where
    flatContent :: X.Content -> String
    flatContent (X.Text str) = X.cdData $ str
    flatContent (X.Elem e) = flatText e

parseElement :: Parser a -> X.Element -> XML a
parseElement p e = parseString p (flatText e)

parseString :: Parser a -> String -> XML a
parseString p txt = do
  currentFile <- (fromMaybe "") <$> R.asks xmlCurrentFile
  case P.runParser p currentFile txt of
    Left err -> throw $ InnerParserFailure (show err) txt
    Right a -> return a


data PropTree a = PropLeaf a | PropNegate (PropTree a) | PropList [PropTree a]
  deriving (Functor, Foldable, Traversable, Show, Eq)

mkPropNegate :: PropTree a -> PropTree a
mkPropNegate (PropList []) = PropList []
mkPropNegate a = PropNegate a

mkPropList :: [PropTree a] -> PropTree a
mkPropList [tree] = tree
mkPropList trees = PropList (filter (not . null) trees)

instance Semigroup (PropTree a) where
  a <> b = mkPropList [a, b]

splitPropTree :: PropTree (Either a b) -> (PropTree a, PropTree b)
splitPropTree tree = case tree of
  PropLeaf e -> case e of
    Left a -> (PropLeaf a, PropList [])
    Right b -> (PropList [], PropLeaf b)
  PropList es ->
    let (as, bs) = unzip $ map splitPropTree es
    in (mkPropList as, mkPropList bs)
  PropNegate p ->
    let (a, b) = splitPropTree p
    in (mkPropNegate a, mkPropNegate b)

separateNegativePropTree :: PropTree a -> PropTree (Either a a)
separateNegativePropTree tree = case tree of
  PropLeaf a -> PropLeaf $ Left a
  PropList as -> mkPropList $ map separateNegativePropTree as
  PropNegate p -> mkPropNegate $ fmap Right p

flatPropTree :: PropTree a -> Maybe [a]
flatPropTree tree = case tree of
  PropLeaf a -> return $ [a]
  PropList as -> concat <$> mapM flatPropTree as
  PropNegate _ -> Nothing

splitClauses :: forall a. Show a => PropTree a -> Maybe ([a], [[a]])
splitClauses tree = do
  let (pos, rest) = splitPropTree $ separateNegativePropTree tree
  flatpos <- flatPropTree pos
  negpos <- collectNegatedClauses rest
  return (flatpos, negpos)
  where
    collectNegatedClauses :: PropTree a -> Maybe [[a]]
    collectNegatedClauses tree' = case tree' of
      PropNegate a -> (:[]) <$> flatPropTree a
      PropList as -> concat <$> mapM collectNegatedClauses as
      PropLeaf _ -> Nothing


explodeAny :: [BT.Bit] -> [[BT.Bit]]
explodeAny (b : bits) = do
  b' <- go b
  bits' <- explodeAny bits
  return $ b' : bits'
  where
    go :: BT.Bit -> [BT.Bit]
    go b'@(BT.ExpectedBit _) = [b']
    go BT.Any = [BT.ExpectedBit True, BT.ExpectedBit False]
explodeAny [] = return []

explodeBitSections :: [BitSection BT.Bit] -> [[BitSection BT.Bit]]
explodeBitSections (bs : rest) = do
  bits <- explodeAny (sectBits bs)
  let bs' = bs { sectBits = bits }
  rest' <- explodeBitSections rest
  return $ bs' : rest'
explodeBitSections [] = return []

liftMaybe :: InnerXMLException -> Maybe a -> XML a
liftMaybe _ (Just a) = return a
liftMaybe e Nothing = throw e

deriveConstrainedMasks :: PropTree (BitSection QuasiBit) -> XML ([QuasiBit], [[BT.Bit]])
deriveConstrainedMasks constraints = case splitClauses constraints of
  Just (positiveConstraints, negativeConstraints) -> do
    mask' <- fmap (take 32 . map (fromMaybe (Bit BT.Any))) $
      liftMaybe (InvalidPositiveConstraint positiveConstraints) $
        computePattern mergeQBits positiveConstraints

    negMasks <- sequence $ do
      negConstraintBase <- negativeConstraints
      negConstraint <- explodeBitSections (map (fmap flattenQuasiBit) negConstraintBase)
      return $
        liftMaybe (InvalidNegativeConstraint negConstraint) $
          computeBitPattern 32 negConstraint

    return (mask', negMasks)
  Nothing -> fail $ "Malformed bitsection for constrained mask derivation: " ++ show constraints

fieldConstraintsParser :: Parser (PropTree (String, [BT.Bit]))
fieldConstraintsParser = do
  props <- outerParser
  P.eof
  return props
  where

    outerParser :: Parser (PropTree (String, [BT.Bit]))
    outerParser = do
      mkPropList <$> P.sepBy1 combinedParser ((void $ P.char ';') <|> (void $ P.chunk "&&"))

    combinedParser :: Parser (PropTree (String, [BT.Bit]))
    combinedParser = do
      P.takeWhileP Nothing (== ' ')
      props <- P.choice [ negParser
                        , P.between (P.char '(') (P.char ')') outerParser
                        , atomParser
                        ]
      P.takeWhileP Nothing (== ' ')
      return props

    negParser :: Parser (PropTree (String, [BT.Bit]))
    negParser = do
      P.char '!'
      P.between (P.char '(') (P.char ')') (mkPropNegate <$> outerParser)

    atomParser :: Parser (PropTree (String, [BT.Bit]))
    atomParser = do
      name <- P.takeWhile1P Nothing (/= ' ')
      negate <- (P.chunk " == " >> return False) <|> (P.chunk " != " >> return True)
      bits <- P.some bitParser
      P.takeWhileP Nothing (== ' ')
      if negate
        then return $ mkPropNegate $ PropLeaf (name, bits)
        else return $ PropLeaf (name, bits)

bitParser :: Parser BT.Bit
bitParser = do
  P.choice
    [ P.char '0' >> return (BT.ExpectedBit False)
    , P.char '1' >> return (BT.ExpectedBit True)
    , P.char 'x' >> return BT.Any
    ]
-- | A specialization of an instruction given a set of flags
data Encoding = Encoding { encName :: String
                         , encConstraints :: PropTree (BitSection BT.Bit)
                         }

data BitSection a = BitSection { sectBitPos :: Int
                               , sectBits :: [a]
                               }
  deriving (Show, Eq, Functor, Foldable)

sectWidth :: BitSection a -> Int
sectWidth bsect = length (sectBits bsect)

lookupPropSections :: Fields -> PropTree (String, [a]) -> XML (PropTree (BitSection a))
lookupPropSections fields tree = forM tree $ \(nm, mask) -> do
  field <- lookupField fields (NameExpString nm)
  unless (fieldWidth field == length mask) $
     throw $ MismatchedFieldWidth field (length mask)
  return $ BitSection (31 - fieldHibit field) mask

leafGetEncodings :: InstructionLeaf -> Fields -> XML [Encoding]
leafGetEncodings ileaf fields = do
  let iclass = ileafiClass ileaf
  forChildren "encoding" iclass $ \encoding -> do
    encName <- getAttr "name" encoding
    withEncodingName encName $ do
      case X.findAttr (qname "bitdiffs") encoding of
        Nothing -> return $ Encoding encName (PropList [])
        Just bitdiffs -> do
          parsedConstraints <- parseString fieldConstraintsParser bitdiffs
          constraints <- lookupPropSections fields parsedConstraints
          return $ Encoding encName constraints

parseExplanation :: X.Element -> XML (Maybe ([String], (String, (String, RegisterInfo))))
parseExplanation explanation = do
    encs <- parseEncList <$> getAttr "enclist" explanation
    case X.findChild (qname "account") explanation of
      Just account -> do
          -- FIXME: "table" operands are not stored under an "account" element
        para <- getChild "intro" account >>= getChild "para"
        parseElement registerInfoParser para >>= \case
          Just rinfo -> do
            encodedin <- getAttr "encodedin" account
            symbol <- getChild "symbol" explanation
            symbolName <- parseElement registerNameParser symbol
            return $ Just (encs, (symbolName, (encodedin, rinfo)))
          _ -> return Nothing
      Nothing -> return Nothing

processExplanations :: Fields -> [(String, (String, RegisterInfo))] -> XML [(String, RegisterInfo, [Field])]
processExplanations allfields explanations = mapM processExplanation explanations
  where
    processExplanation :: (String, (String, RegisterInfo)) -> XML (String, RegisterInfo, [Field])
    processExplanation (symbolName, (encodedin, rinfo)) = do
      unless (regIndexMode rinfo == IndexBasic || null encodedin) $
        throw $ UnexpectedAttributeValue "encodedin" encodedin
      case regIndexMode rinfo of
        IndexVector fieldNames -> do
          fields <- forM fieldNames $ \fieldName -> do
            lookupField allfields (NameExpString fieldName)
          return $ (symbolName, rinfo, fields)
        IndexRegisterOffset baseregister -> do
          case lookup baseregister explanations of
            Just rawinfo -> do
              (_, _, fields) <- processExplanation (baseregister, rawinfo)
              return $ (symbolName, rinfo, fields)
            Nothing -> throw $ MissingRegisterInfo baseregister (map fst explanations)
        IndexBasic -> do
          fieldDescription <- case regMode rinfo of
            ImplicitEncoding mimplicitName -> do
              let implicitName = fromMaybe symbolName mimplicitName
              unless (null encodedin || encodedin == implicitName) $ do
                throw $ UnexpectedAttributeValue "encodedin" encodedin
              return implicitName
            _ -> return encodedin
          when (fieldDescription == "") $ do
            throw $ InvalidRegisterInfo rinfo
          exps <- nameExps fieldDescription
          fields <- forM exps $ lookupField allfields
          return $ (symbolName, rinfo, fields)

getRegisterOperandType :: Int -> RegisterInfo -> XML DT.OperandType
getRegisterOperandType totalBits rinfo = DT.OperandType <$> case (regKind rinfo, regIndexMode rinfo) of
  (GPR, IndexBasic) | totalBits == 4 -> return "GPR4"
  (GPR, IndexBasic) | totalBits == 3 -> return "GPR3"
  (GPR, IndexRegisterOffset _) | totalBits == 4 -> return "GPR4_1"
  (SIMDandFP, IndexBasic) | totalBits == 5 -> return "SIMD5"
  (SIMDandFP, IndexBasic) | totalBits == 4 -> return "SIMD4"
  (SIMDandFP, IndexBasic) | totalBits == 3 -> return "SIMD3"
  (SIMDandFP, IndexBasic) | totalBits == 2 -> return "SIMD2"
  (SIMDandFP, IndexRegisterOffset _) | totalBits == 5 -> return "SIMD5_1"
  (SIMDandFP, IndexVector _) | totalBits == 7 -> return "SIMD7"
  _ -> throw $ InvalidRegisterInfo rinfo

-- | Build operand descriptors out of the given fields
leafGetOperands :: InstructionLeaf -> Fields -> [Encoding] -> XML [(Encoding, [DT.OperandDescriptor])]
leafGetOperands ileaf allfields encodings = do
  let iclass = ileafiClass ileaf

  let encodingNames = map encName encodings

  explanations <- getChild "explanations" (ileafFull ileaf)
  parsedExplanations <- fmap catMaybes $ forChildren "explanation" explanations parseExplanation

  registerSources <- fmap concat $ forM parsedExplanations $ \(encs, v) -> do
    let realencs = intersect encodingNames encs
    return $ [ (e, [v]) | e <- realencs ]

  let rawRegistersEncodings = M.assocs $ M.fromListWith (++) $
        registerSources
        ++ map (\enc -> (enc, [])) encodingNames

  forM rawRegistersEncodings $ \(encName', rawRegisters) -> do
    withEncodingName encName' $ do
      rinfos <- processExplanations allfields rawRegisters

      let usedFieldNames = concat $ map (\(_, _, fields) -> map fieldName fields) rinfos
      let unusedFields = M.elems $ M.withoutKeys allfields (S.fromList usedFieldNames)

      registerOps <- forM rinfos $ \(name, rinfo, fields) -> do
        let totalBits = sum $ map fieldWidth fields
        chunks <- forM fields $ \(Field name hibit width _ _ _) -> do
          return (DT.IBit (hibit - width + 1), PT.OBit 0, fromIntegral width)
        opType <- getRegisterOperandType totalBits rinfo

        return $ DT.OperandDescriptor { DT.opName = name
                                      , DT.opChunks = chunks
                                      , DT.opType = opType
                                      }

      immediates <- forM unusedFields $ \field@(Field name hibit width _ _ _) -> do
        let opType = getFieldOpType field
        return $ DT.OperandDescriptor { DT.opName = name
                                      , DT.opChunks = [( DT.IBit (hibit - width + 1)
                                                       , PT.OBit 0
                                                       , fromIntegral width)]
                                      , DT.opType = DT.OperandType opType
                                      }

      Just enc <- return $ find (\enc -> encName enc == encName') encodings
      return $ (enc, registerOps ++ immediates)

getFieldOpType :: Field -> String
getFieldOpType field = case fieldTypeOverride field of
  Just opType -> opType
  _ ->  printf "Bv%d" (fieldWidth field)

fieldOpTypeOverrides :: Field -> Field
fieldOpTypeOverrides field =
  let
    name = fieldName field
    width = fieldWidth field
    constraint = fieldConstraint field
  in case (name, width) of
    -- Used for register fields which must refer to the PC or they are unpredictable
    -- These do not appear in the register information sections, so we have to capture them ad-hoc
       ('R' : _ : [], 4) | Just (pos, []) <- splitClauses constraint
                        , subConstraint 4 pos == Just (replicate 4 (QBit True)) ->
         field { fieldUseName = True, fieldTypeOverride = Just "GPR4" }
       _ -> field
  where
    subConstraint len bitsects = do
      pat <- computePattern mergeQBits bitsects
      return $ take len $ catMaybes pat


findiClassByName :: X.Element -> String -> XML X.Element
findiClassByName e name = do
  classes <- getChild "classes" e
  iclasses <- fmap catMaybes $ forChildren "iclass" classes $ \iclass -> do
    case X.findAttr (qname "name") iclass of
      Just name' | name' == name -> return $ Just iclass
      _ -> return Nothing
  case iclasses of
    [iclass] -> return iclass
    _ -> throw $ NoUniqueiClass name iclasses


findXMLFile :: String -> XML FilePath
findXMLFile fileName = do
  allfiles <- R.asks xmlAllFiles
  case find (\f -> takeFileName f == fileName) allfiles of
    Just fn -> return fn
    Nothing -> throw $ MissingXMLFile fileName

isAliasedInstruction :: InstructionLeaf -> Bool
isAliasedInstruction ileaf = isJust $ X.findChild (qname "aliasto") (ileafFull ileaf)

leafMnemonic' :: InstructionLeaf -> XML String
leafMnemonic' ileaf = do
  let iclass = ileafiClass ileaf
  case X.findAttr (qname "psname") =<< X.findChild (qname "regdiagram") iclass of
    Just "" -> do
      case X.findChild (qname "aliasto") (ileafFull ileaf) of
        Just alias -> do
          iclassname <- getAttr "name" iclass
          aliasedXMLFile <- getAttr "refiform" alias >>= findXMLFile
          withParsedXMLFile aliasedXMLFile $ \aliasedXML -> do
            aliasediclass <- findiClassByName aliasedXML iclassname
            leafMnemonic' (InstructionLeaf aliasedXML aliasediclass)
        Nothing -> throw $ MnemonicError "empty psname and no alias found"
    Just psname -> parseString nameParser psname
    Nothing -> throw $ MnemonicError "no psname"

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

data RegisterMode = Data | Accumulator | Base | Index | ImplicitEncoding (Maybe String) | NoMode
  deriving (Ord, Eq, Show)

data RegisterIndexMode = IndexBasic | IndexRegisterOffset String | IndexVector [String]
  deriving (Ord, Eq, Show)

data RegisterInfo =
  RegisterInfo { regKind :: RegisterKind
               , regMode :: RegisterMode
               , regDirection :: RegisterDirection
               , regIndexMode :: RegisterIndexMode -- for a logical register, how is it computed
                                                   -- from the operand fields
               , regSourceText :: String
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
      sourceTxt <- P.stateInput <$> P.getParserState
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
      (rdir, mrmode) <- directionParser impliedDirection
      ws
      indexMode <- indexModeParser rkind
      return (RegisterInfo rkind (fromMaybe rmode mrmode) rdir indexMode sourceTxt)
      where
        kindParser :: Parser (RegisterKind, Maybe RegisterDirection, Maybe RegisterMode)
        kindParser =
          P.choice
            [ P.chunk "general-purpose" >> return (GPR, Nothing, Nothing)
            , P.chunk "Arm source" >> return (GPR, Just Input, Just $ ImplicitEncoding Nothing)
            , P.chunk "source general-purpose" >> return (GPR, Just Input, Just $ ImplicitEncoding Nothing)
            , P.chunk "destination general-purpose" >> return (GPR, Just Output, Just $ ImplicitEncoding Nothing)
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

        directionParser' :: Parser RegisterDirection
        directionParser' =
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
        directionParser :: Maybe RegisterDirection -> Parser (RegisterDirection, Maybe RegisterMode)
        directionParser (Just impliedDirection) = P.chunk "register" >> return (impliedDirection, Nothing)
        directionParser Nothing =
          (do
              P.chunk "register <"
              nm <- P.takeWhile1P Nothing (/= '>')
              P.chunk "> to be "
              direction <- P.choice [ P.chunk "loaded" >> return Input
                                    , P.chunk "stored" >> return Output
                                    ]
              return (direction, Just (ImplicitEncoding (Just nm)))
          )
          <|>
          (directionParser' >>= (\mode -> return (mode, Nothing)))


        indexModeParser :: RegisterKind -> Parser RegisterIndexMode
        indexModeParser SIMDandFP = do
          P.takeWhileP Nothing (/= '.')
          P.choice
            [ vectorIndexModeParser
            , oneOffsetIndexModeParser
            , return IndexBasic
            ]
        indexModeParser _ = do
          P.takeWhileP Nothing (/= '.')
          P.choice
            [ oneOffsetIndexModeParser
            , return IndexBasic
            ]

        oneOffsetIndexModeParser :: Parser RegisterIndexMode
        oneOffsetIndexModeParser = do
          P.chunk ". This is the next SIMD&FP register after <"
          nm <- P.takeWhile1P Nothing (/= '>')
          return $ IndexRegisterOffset nm
          <|> do
          P.choice
            [ P.chunk ". <Rt2> must be <R(t+1)>."
            , P.chunk ". This register must be <R(t+1)>."
            ]
          return $ IndexRegisterOffset "Rt"

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
  , "list of two or more registers to be loaded"
  , "list of two or more registers to be stored"
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

placeAt :: Int -> [a] -> [a] -> [a]
placeAt ix subList l =
  let (prefix, rst) = splitAt ix l
      suffix = drop (length subList) rst
  in prefix ++ subList ++ suffix

getSublist :: Int -> Int -> [a] -> [a]
getSublist ix len l =
  let (_, rst) = splitAt ix l
  in take len rst

mergeBits :: MF.MonadFail m => BT.Bit -> BT.Bit -> m BT.Bit
mergeBits b1 b2 = case (b1, b2) of
  (BT.ExpectedBit x, BT.ExpectedBit y) | x == y -> return $ BT.ExpectedBit y
  (BT.Any, x) -> return x
  (x, BT.Any) -> return x
  _ -> fail $ "Incompatible bits: " ++ show b1 ++ " " ++ show b2

mergeQBits :: MF.MonadFail m => QuasiBit -> QuasiBit -> m QuasiBit
mergeQBits qb1 qb2 = case (qb1, qb2) of
  (QBit x, QBit y) | x == y -> return $ QBit y
  (QBit x, Bit (BT.ExpectedBit y)) | x == y -> return $ Bit $ BT.ExpectedBit y
  (Bit (BT.ExpectedBit y), QBit x) | x == y -> return $ Bit $ BT.ExpectedBit y
  (x, Bit BT.Any) -> return x
  (Bit BT.Any, x) -> return x
  (Bit b1, Bit b2) -> Bit <$> mergeBits b1 b2
  _ -> fail $ "Incompatible qbits: " ++ show qb1 ++ " " ++ show qb2

zipWithSafe :: MF.MonadFail m => (a -> b -> m c) -> [a] -> [b] -> m [c]
zipWithSafe f a b = if (length a == length b) then zipWithM f a b else fail "zipWithSafe: length mismatch"

-- | Given a list of fields, and a bit pattern whose length is the same as the sum of
-- all the field lengths, return a "full" bit pattern reflecting the effect of
-- combining all those patterns together.
computePattern' :: MF.MonadFail m => (a -> b -> m b) -> [BitSection a] -> [b] -> m [b]
computePattern' f [] fullPat = return $ fullPat
computePattern' f (fld : rstFlds) fullPat = do
  let
    top = sectBitPos fld
    existing = getSublist top (length $ sectBits fld) fullPat
  resultMask <- zipWithSafe f (sectBits fld) existing
  let fullPat' = placeAt (sectBitPos fld) resultMask fullPat
  computePattern' f rstFlds fullPat'

computePattern :: forall m a b. MF.MonadFail m => (a -> a -> m a) -> [BitSection a] -> m [Maybe a]
computePattern merge bitsects = computePattern' go bitsects (repeat Nothing)
  where
    go :: a -> Maybe a -> m (Maybe a)
    go a (Just a') = Just <$> merge a a'
    go a _ = Just <$> return a

computeBitPattern :: MF.MonadFail m => Int -> [BitSection BT.Bit] -> m [BT.Bit]
computeBitPattern len bitsects = do
  result <- computePattern mergeBits bitsects
  return $ take len (map (fromMaybe BT.Any) result)


getChildWith :: (X.Element -> Bool) -> X.Element -> XML X.Element
getChildWith f elt = case X.filterChild f elt of
  Nothing -> throw $ NoMatchingChildElement elt
  Just child -> return child

getChild :: String -> X.Element -> XML X.Element
getChild name elt = case X.findChild (qname name) elt of
  Nothing -> throw $ MissingChildElement name elt
  Just child -> return child

getAttr :: String -> X.Element -> XML String
getAttr name elt = case X.findAttr (qname name) elt of
  Nothing -> throw $ MissingAttr name elt
  Just attr -> return attr
