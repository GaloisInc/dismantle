{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE StandaloneDeriving #-}

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
import           Control.Monad.Except ( throwError )
import qualified Control.Monad.Except as ME
import           Control.Monad ( forM, void, when, unless, foldM, (>=>), zipWithM )
import           Control.Monad.Fail ( fail )
import qualified Control.Monad.Fail as MF
import           Control.Monad.Trans ( lift, liftIO )
import qualified Control.Monad.State as MS
import qualified Control.Monad.Reader as R
import           Control.Monad.Trans.RWS.Strict ( RWST )
import qualified Control.Monad.Trans.RWS.Strict as RWS
import           Data.Either ( partitionEithers )
import           Data.List (stripPrefix, find, nub, intersect, (\\), intercalate, partition, isPrefixOf, sort )
import           Data.List.Split as LS
import qualified Data.Map as M
import           Data.Maybe (catMaybes, fromMaybe, isJust, isNothing)
import qualified Data.Set as S
import           Data.Word ( Word8 )
import           Data.Void (Void)
import           System.IO ( withFile, IOMode(..), hPutStrLn )
import qualified System.IO.Strict as SIO
import           System.FilePath.Glob ( namesMatching )
import           System.FilePath ( (</>), (<.>), takeFileName )
import qualified Text.Megaparsec as P
import qualified Text.Megaparsec.Char as P
import           Text.Printf (printf)
import qualified Text.XML.Light as X
import           Text.PrettyPrint.HughesPJClass ( (<+>), ($$), ($+$) )
import qualified Text.PrettyPrint.HughesPJClass as PP

import qualified Dismantle.Tablegen as DT
import qualified Dismantle.Tablegen.ByteTrie as BT
import qualified Dismantle.Tablegen.Parser.Types as PT

import Debug.Trace

data XMLException = MissingChildElement String
                       | NoMatchingChildElement
                       | MissingAttr String
                       | MissingField String Fields
                       | MissingEncoding String
                       | MissingFieldForRegister RegisterInfo String
                       | InvalidChildElement
                       | MnemonicError String
                       | InvalidPattern String
                       | MismatchedFieldWidth Field Int
                       | forall a. Show a => MismatchedFieldsForBits [Field] [a]
                       | MissingRegisterInfo String [String]
                       | forall a. Show a => MismatchedWidth Int [a]
                       | InvalidXmlFile String
                       | InnerParserFailure String String
                       | UnexpectedAttributeValue String String
                       | InvalidRegisterInfo RegisterInfo
                       | InvalidField Field
                       | BitdiffsParseError String String
                       | UnexpectedBitfieldLength [BT.Bit]
                       | NoUniqueiClass String
                       | MissingXMLFile String
                       | InvalidConstraints (PropTree (BitSection QuasiBit)) String
                       | XMLMonadFail String
                       | forall a b. (Show a, Show b) => MismatchedListLengths [a] [b]
                       | UnexpectedElements [X.Element]
                       | MissingEncodingTableEntry String
                       | MismatchedMasks [QuasiBit] [QuasiBit]
                       | MismatchedNegativeMasks [[BT.Bit]] [[BT.Bit]]


deriving instance Show XMLException


instance E.Exception OuterXMLException

data OuterXMLException = OuterXMLException XMLEnv XMLException


newtype XML a = XML (RWST XMLEnv () XMLState (ME.ExceptT OuterXMLException IO) a)
  deriving ( Functor
           , Applicative
           , Monad
           , MS.MonadState XMLState
           , R.MonadReader XMLEnv
           , MS.MonadIO
           )

instance MF.MonadFail XML where
  fail msg = throwError $ XMLMonadFail msg

instance ME.MonadError XMLException XML where
  throwError e = do
    env <- R.ask
    XML (lift $ throwError $ OuterXMLException env e)

  catchError (XML m) handler = do
    st <- MS.get
    env <- R.ask
    result <- liftIO $ ME.runExceptT $ RWS.runRWST m env st
    case result of
      Left (OuterXMLException _ e) -> handler e
      Right (a, st', ()) -> do
        MS.put st'
        return a

warnError :: XMLException -> XML ()
warnError e = do
  env <- R.ask
  let pretty = PP.nest 1 (PP.text "WARNING:" $$ (PP.pPrint $ OuterXMLException env e))
  logXML $ PP.render pretty

instance PP.Pretty XMLException where
  pPrint e = case e of
    UnexpectedElements elems ->
      PP.text "UnexpectedElements"
      <+> PP.brackets (PP.hsep (PP.punctuate (PP.text ",") (map simplePrettyElem elems)))
    MismatchedMasks mask mask' -> PP.text "MismatchedMasks"
      $$ PP.nest 1 (prettyMask mask $$ prettyMask mask')
    MismatchedNegativeMasks masks masks' -> PP.text "MismatchedNegativeMasks"
      $$ PP.nest 1 (PP.vcat (map (prettyMask . map Bit) masks))
      $$ PP.text "vs."
      $$ PP.nest 1 (PP.vcat (map (prettyMask . map Bit) masks'))
    _ -> PP.text $ show e

instance PP.Pretty OuterXMLException where
  pPrint (OuterXMLException env e) =
    PP.text "Error encountered while processing" <+> PP.text (xmlArchName env)
    <+> case xmlCurrentFile env of
      Just curFile -> PP.text "in" <+> PP.text curFile
      Nothing -> PP.empty
    <+> case xmlCurrentEncoding env of
      Just curEnc -> PP.text "for encoding" <+> PP.text curEnc
      Nothing -> PP.empty
    <+> (if (null (xmlCurrentPath env)) then PP.empty else PP.text "at XML path:")
    $$ PP.nest 1 (prettyElemPath $ xmlCurrentPath env)
    $$ PP.nest 1 (PP.pPrint e)

prettyElemPath :: [X.Element] -> PP.Doc
prettyElemPath es = go (reverse es)
  where
    go :: [X.Element] -> PP.Doc
    go (e : es) = simplePrettyElem e $$ (PP.nest 1 $ go es)
    go [] = PP.empty

simplePrettyElem :: X.Element -> PP.Doc
simplePrettyElem e = PP.text "<" PP.<> PP.text (X.qName (X.elName e))
  <+> (PP.hsep $ map prettyAttr (X.elAttribs e))
  PP.<> PP.text ">"
  where
    prettyAttr :: X.Attr -> PP.Doc
    prettyAttr at =
      PP.text (X.qName (X.attrKey at))
      PP.<> PP.text "="
      PP.<> PP.doubleQuotes (PP.text (X.attrVal at))

instance Show OuterXMLException where
  show e = PP.render (PP.pPrint e)


data InstructionLeaf = InstructionLeaf { ileafFull :: X.Element -- entire leaf
                                       , ileafiClass :: X.Element -- iclass
                                       }
  deriving Show


data XMLState = XMLState { encodingMap :: M.Map String Encoding
                         , encodingTableMap :: M.Map EncIndexIdent ([QuasiBit], [[BT.Bit]])
                         }
  deriving Show

data XMLEnv = XMLEnv { xmlCurrentFile :: Maybe FilePath
                     , xmlCurrentEncoding :: Maybe String
                     , xmlCurrentPath :: [X.Element]
                     , xmlAllFiles :: [FilePath]
                     , xmlArchName :: String
                     , xmlLog :: String -> IO ()
                     }

runXML :: String -> [FilePath] -> (String -> IO ()) -> XML a -> IO (Either OuterXMLException a)
runXML archName allFiles logf (XML a) = ME.runExceptT $
    fst <$> RWS.evalRWST a (XMLEnv Nothing Nothing [] allFiles archName logf) (XMLState M.empty M.empty)


qname :: String -> X.QName
qname str = X.QName str Nothing Nothing

-- | Given a path to the directory containing all XML instruction files, build an ISA
-- descriptor.
loadXML ::  String -> [FilePath] -> FilePath -> FilePath -> IO DT.ISADescriptor
loadXML arch xmlFiles xmlEncIndex logFile = do
 withFile logFile WriteMode $ \logfile -> do
   let doLog msg = hPutStrLn logfile msg
   result <- runXML arch xmlFiles doLog $ do
     withParsedXMLFile xmlEncIndex loadEncIndex
     instrs <- fmap concat $ forM xmlFiles $ (\f -> withParsedXMLFile f loadInstrs)
     return $ DT.ISADescriptor { DT.isaInstructions = instrs
                               , DT.isaOperands = S.toList (S.fromList (concatMap instrOperandTypes instrs))
                               , DT.isaErrors = []
                               }
   case result of
     Left err -> do
       doLog (show err)
       E.throw err
     Right desc -> return desc

logXML :: String -> XML ()
logXML msg = do
  logf <- R.asks xmlLog
  MS.liftIO (logf msg)

withParsedXMLFile :: FilePath -> (X.Element -> XML a) -> XML a
withParsedXMLFile fullPath m = R.local (\e -> e { xmlCurrentFile = Just fullPath }) $ do
  fileStr <- MS.liftIO $ SIO.readFile fullPath
  case X.parseXMLDoc fileStr of
    Just c -> withElement c $ m c
    Nothing -> throwError $ InvalidXmlFile fullPath

withEncodingName :: String -> XML a -> XML a
withEncodingName encnm = R.local (\e -> e { xmlCurrentEncoding = Just encnm })

withElement :: X.Element -> XML a -> XML a
withElement elem = R.local (\e -> e { xmlCurrentPath = elem : (xmlCurrentPath e) })

data EncIndexIdent =
    IdentEncodingName String
  | IdentFileClassName String FilePath
  deriving (Show, Eq, Ord)

getEncIndexIdent :: X.Element -> XML EncIndexIdent
getEncIndexIdent elt = case X.findAttr (qname "encname") elt of
  Just nm -> return $ IdentEncodingName nm
  Nothing -> do
    encoding <- getAttr "encoding" elt
    iformfile <- getAttr "iformfile" elt
    return $ IdentFileClassName encoding iformfile

getDecodeConstraints :: Fields -> X.Element -> XML (PropTree (BitSection BT.Bit))
getDecodeConstraints fields dcs = do
  rawConstraints <- forChildren "decode_constraint" dcs $ \dc -> do
    [name,"!=", val] <- getAttrs ["name", "op", "val"] dc
    flds <- nameExps name
    bits <- parseString (P.some bitParser) val
    return $ mkPropNegate $ PropLeaf $ (flds, bits)
  resolvePropTree fields (mkPropList rawConstraints)

instrTableConstraints :: [[Field]] -> X.Element -> XML (PropTree (BitSection BT.Bit))
instrTableConstraints tablefieldss tr = do
  "instructiontable" <- getAttr "class" tr
  let tds = X.filterChildren (\e -> X.findAttr (qname "class") e == Just "bitfield") tr
  fmap mkPropList $ forM (zip tds [0..]) $ \(td, i) -> do
    let fields = tablefieldss !! i
    let width = sum $ map fieldWidth fields
    bitwidth <- read <$> getAttr "bitwidth" td
    -- soft-throw this error since the XML is occasionally wrong but this is recoverable
    unless (width == bitwidth) $
      warnError $ MismatchedWidth bitwidth fields
    fmap concatPropTree $ parseConstraint width (X.strContent td) $ \bits -> do
      unpackFieldConstraints (fields, bits)

loadEncIndex :: X.Element -> XML ()
loadEncIndex encodingindex = do
  arch <- R.asks xmlArchName
  isa <- getAttr "instructionset" encodingindex
  unless (arch == isa) $
    throwError $ UnexpectedAttributeValue "instructionset" isa
  void $ forChildren "iclass_sect" encodingindex $ \iclass_sect -> do
    (fields, _, fieldConstraints) <- iclassFieldsAndProp iclass_sect
    decodeConstraints <- withChild "decode_constraints" iclass_sect $ \case
      Just dcs -> getDecodeConstraints fields dcs
      Nothing -> return emptyPropTree
    instructiontable <- getChild "instructiontable" iclass_sect
    bitfieldsElts <- resolvePath instructiontable $
      [ ("thead", Just ("class", "instructiontable"))
      , ("tr", Just ("id", "heading2")) ]
    fieldss <- case bitfieldsElts of
      [] -> return []
      [bitfieldsElt] -> forChildren "th" bitfieldsElt $ \th -> do
        "bitfields" <- getAttr "class" th
        nmes <- nameExps (X.strContent th)
        mapM (lookupField fields) nmes
      _ -> throwError $ UnexpectedElements bitfieldsElts
    tbody <- getChild "tbody" instructiontable
    forChildren "tr" tbody $ \tr -> do
      encident <- getEncIndexIdent tr
      constraints <- instrTableConstraints fieldss tr
      (mask, negMasks) <- deriveMasks fieldConstraints (decodeConstraints <> constraints)
      MS.modify' $ \st -> st { encodingTableMap = M.insert encident (mask, negMasks) (encodingTableMap st) }

loadInstrs :: X.Element -> XML [DT.InstructionDescriptor]
loadInstrs xmlElement = do
  -- format as instructions
  arch <- R.asks xmlArchName
  withChild "classes" xmlElement $ \case
    Just classes -> do
      fmap concat $ forChildren "iclass" classes $ \iclass -> do
        let leaf = InstructionLeaf xmlElement iclass
        isa <- getAttr "isa" iclass
        if isa == arch && not (isAliasedInstruction leaf) then do
          (fields, qmasks, iconstraints) <- iclassFieldsAndProp iclass
          encodings <- leafGetEncodings leaf fields qmasks iconstraints
          forM encodings $ \encoding -> do
            withEncodingName (encName encoding) $ do
              getDescriptor encoding
        else return []
    _ -> return []

instrOperandTypes :: DT.InstructionDescriptor -> [DT.OperandType]
instrOperandTypes idesc = map DT.opType (DT.idInputOperands idesc ++ DT.idOutputOperands idesc)


iclassFieldsAndProp :: X.Element -> XML (Fields, [Operand], PropTree (BitSection QuasiBit))
iclassFieldsAndProp iclass = do
  rd <- getChild "regdiagram" iclass
  fields <- forChildren "box" rd getBoxField
  namedFields <- fmap catMaybes $ forM fields $ \field -> do
    case (fieldName field, fieldUseName field) of
      (_ : _, True) -> return $ Just (fieldName field, field)
      (_, False) -> return Nothing
      _ -> ME.throwError $ InvalidField field

  return $ ( M.fromListWith mergeFields $ namedFields
           , quasiMaskOperands fields
           , mkPropList (map fieldConstraint fields))
  where
    mergeFields :: Field -> Field -> Field
    mergeFields field field' = if field == field' then field else
      error $ "Unexpected duplicate field: " ++ (show field)

constraintParser :: Parser ([BT.Bit], Bool)
constraintParser = do
  isPositive <- (P.chunk "!= " >> return False) <|> (return True)
  bits <- P.some bitParser
  return (bits, isPositive)

parseConstraint :: Int -> String -> ([BT.Bit] -> XML a) -> XML (PropTree a)
parseConstraint width "" m = PropLeaf <$> m (replicate width BT.Any)
parseConstraint width str m = do
  (bits, isPositive) <- parseString constraintParser str
  unless (length bits == width) $
    throwError $ MismatchedWidth width bits
  let sign = if isPositive then id else mkPropNegate
  sign . PropLeaf <$> m bits

getBoxField :: X.Element -> XML Field
getBoxField box = do
  let width = maybe 1 read (X.findAttr (qname "width") box)
  hibit <- read <$> getAttr "hibit" box

  constraint <- case X.findAttr (qname "constraint") box of
    Just constraint -> do
      parseConstraint width constraint $ \bits -> do
        return $ bitSectionHibit hibit (map Bit bits)
    Nothing -> do
      bits <- fmap concat $ forChildren "c" box $ \c -> do
        let content = X.strContent c
        case X.findAttr (qname "colspan") c of
          Just colspan -> do
            unless (null content) $
              throwError $ InvalidChildElement
            return $ replicate (read colspan) (Bit $ BT.Any)
          Nothing | Just qbit <- quasiMaskBit content ->
           return [qbit]
          _ -> throwError $ InvalidChildElement
      unless (length bits == width) $
        throwError $ MismatchedWidth width bits
      return $ PropLeaf $ bitSectionHibit hibit bits
  let field = Field { fieldName = fromMaybe "" $ X.findAttr (qname "name") box
                    , fieldHibit = hibit
                    , fieldWidth = width
                    , fieldConstraint = constraint
                    , fieldUseName = X.findAttr (qname "usename") box == Just "1"
                    , fieldTypeOverride = Nothing
                    }
  return $ fieldOpTypeOverrides field

data QuasiBit =
    Bit BT.Bit
    -- ^ a normal bittrie specifying a bitmask bit
  | QBit Bool
    -- ^ a qbit (quasi-bit) specifies that a given bitpattern is "required" to have defined
    -- behavior, but not to disambiguate instructions.
  deriving (Eq, Ord, Show)


-- | Does the first bit match against the second bit.
-- i.e. is the first bit at least as general as the second
matchesBit :: BT.Bit -> BT.Bit -> Bool
matchesBit bit bit' = case (bit, bit') of
  (_, BT.Any) -> True
  _ -> bit == bit'

matchesMask :: [BT.Bit] -> [BT.Bit] -> Bool
matchesMask mask mask' = length mask == length mask'
  && all (uncurry matchesBit) (zip mask mask')

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

flattenMask :: [QuasiBit] -> [BT.Bit]
flattenMask mask = map flattenQuasiBit mask

quasiMaskOperands :: [Field] -> [Operand]
quasiMaskOperands fields = catMaybes $ map go (zip fields [0..])
  where
    go :: (Field, Int) -> Maybe Operand
    go (Field name hibit width constraint useName _, i)
     | (not useName) && any (any isQBit) constraint
     , name' <- if name == "" then printf "QuasiMask%d" i else name
     = Just $ PsuedoOperand $
         DT.OperandDescriptor { DT.opName = printf "QuasiMask%d" i
                              , DT.opChunks = [( DT.IBit (hibit - width + 1)
                                , PT.OBit 0
                                , fromIntegral width)]
                                , DT.opType = DT.OperandType (printf "QuasiMask%d" width)
                              }
    go _ = Nothing

type Fields = M.Map String Field

lookupField :: Fields -> NameExp -> XML Field
lookupField flds nexp =
  case nexp of
    NameExpString name ->
      case M.lookup name flds of
        Nothing -> throwError $ MissingField name flds
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

endianness :: [a] -> [a]
endianness bits = concat (reverse (LS.chunksOf 8 bits))

getDescriptor :: Encoding -> XML DT.InstructionDescriptor
getDescriptor encoding = do
  let operandDescs = map operandDescOfOperand (encOperands encoding)

  logXML (PP.render $ PP.pPrint encoding)
  let desc = DT.InstructionDescriptor
        { DT.idMask = map flattenQuasiBit (endianness $ encMask encoding)
        , DT.idNegMasks = map endianness $ encNegMasks encoding
        , DT.idMnemonic = encName encoding
        , DT.idInputOperands = operandDescs
        , DT.idOutputOperands = []
        , DT.idNamespace = encMnemonic encoding
        , DT.idDecoderNamespace = ""
        , DT.idAsmString = encName encoding
          ++ "(" ++ PP.render (prettyMask (endianness $ encMask encoding)) ++ ") "
          ++ simpleOperandFormat (encOperands encoding)
        , DT.idPseudo = False
        , DT.idDefaultPrettyVariableValues = []
        , DT.idPrettyVariableOverrides = []
        }
  return $ desc

instance PP.Pretty Encoding where
  pPrint encoding =
    PP.text "Encoding:" <+> PP.text (encName encoding)
    $$ PP.text "Endian Swapped" $$ mkBody endianness
    $$ PP.text "Original" $$ mkBody id
    where
      mkBody endianswap = PP.nest 1 $
           prettyMask (endianswap $ encMask encoding)
           $$ PP.text "Negative Masks:"
           $$ PP.nest 1 (PP.vcat (map (prettyMask . endianswap . map Bit) (encNegMasks encoding)))

prettyMask :: [QuasiBit] -> PP.Doc
prettyMask qbits = PP.text $ concat $ intercalate ["."] $ map (map go) (LS.chunksOf 8 qbits)
  where
    go :: QuasiBit -> String
    go (Bit BT.Any) = "x"
    go (Bit (BT.ExpectedBit True)) = "1"
    go (Bit (BT.ExpectedBit False)) = "0"
    go (QBit True) = "I"
    go (QBit False) = "O"


simpleOperandFormat :: [Operand] -> String
simpleOperandFormat descs = intercalate ", " $ catMaybes $ map go descs
  where
    go :: Operand -> Maybe String
    go (PsuedoOperand _) = Nothing
    go (RealOperand desc) =
      let
        name = DT.opName desc
      in Just $ name ++ " ${" ++ DT.opName desc ++ "}"

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
    Left err -> throwError $ InnerParserFailure (show err) txt
    Right a -> return a

-- | Representation of a boolean formula with a 'PropList' as conjunction and 'PropNegate' as negation.
data PropTree a = PropLeaf a | PropNegate (PropTree a) | PropList [PropTree a]
  deriving (Functor, Foldable, Traversable, Show, Eq)

concatPropTree :: PropTree [a] -> PropTree a
concatPropTree tree = case tree of
  PropLeaf as -> mkPropList (map PropLeaf as)
  PropNegate tree' -> mkPropNegate (concatPropTree tree')
  PropList trees -> mkPropList $ map concatPropTree trees

emptyPropTree :: PropTree a
emptyPropTree = PropList []

-- These should always be used instead of the constructors in order to keep
-- a 'PropTree' wellformed (where possible) with respect to 'splitClauses'

-- | Negation of a 'PropTree' while avoiding double-negation
mkPropNegate :: PropTree a -> PropTree a
mkPropNegate (PropList []) = PropList []
mkPropNegate (PropNegate a) = a
mkPropNegate a = PropNegate a


-- | Concat 'PropTrees' while avoiding redundant nodes
mkPropList :: [PropTree a] -> PropTree a
mkPropList [tree] = tree
mkPropList trees = PropList (filter (not . null) trees)

instance Semigroup (PropTree a) where
  a <> b = mkPropList [a, b]

splitPropTree :: PropTree (Either a b) -> (PropTree a, PropTree b)
splitPropTree tree = case tree of
  PropLeaf e -> case e of
    Left a -> (PropLeaf a, emptyPropTree)
    Right b -> (emptyPropTree, PropLeaf b)
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

-- | If a 'PropTree' contains no negations, it can be flattened into a list of clauses
flatPropTree :: PropTree a -> Maybe [a]
flatPropTree tree = case tree of
  PropLeaf a -> return $ [a]
  PropList as -> concat <$> mapM flatPropTree as
  PropNegate _ -> Nothing


-- | Split a proptree into a list (conjunction) of positive clauses, and
-- a list (conjunction) of lists (disjunction) of negated clauses.
-- e.g.
--      (A & B) & !(A & C) & !(C & D) ==
--      (A & B) & (!A | !C) & (!C | !D) ==>
--      ([A,B], [[A, C], [C, D]])
-- Returns 'Nothing' if a tree contains double-negation (e.g. A & !(A & !C))
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

deriveMasks :: PropTree (BitSection QuasiBit)
            -> PropTree (BitSection BT.Bit)
            -> XML ([QuasiBit], [[BT.Bit]])
deriveMasks qbits bits = do
  let constraints = (fmap (fmap Bit) bits <> qbits)
  case deriveMasks' constraints of
    Left err -> throwError $ InvalidConstraints constraints err
    Right a -> return a

deriveMasks' :: forall m
             . ME.MonadError String m
            => PropTree (BitSection QuasiBit)
            -> m ([QuasiBit], [[BT.Bit]])
deriveMasks' constraints = case splitClauses constraints of
  Just (positiveConstraints, negativeConstraints) -> do
    mask' <- fmap (take 32 . map (fromMaybe (Bit BT.Any))) $
      withMsg "invalid positive constraint"
        (computePattern mergeQBits) positiveConstraints

    negMasks <- sequence $ do
      negConstraintBase <- negativeConstraints
      negConstraint <- explodeBitSections (map (fmap flattenQuasiBit) negConstraintBase)
      return $ withMsg "invalid negative constraint"
        (computeBitPattern 32) negConstraint
    return (mask', nub negMasks)
  Nothing -> throwError $ "Malformed bitsection for constrained mask derivation: " ++ show constraints
  where
    withMsg :: forall a b. Show b => String -> (b -> Either String a) -> b -> m a
    withMsg msg f b = case f b of
      Left err -> throwError $ msg ++ ": " ++ show b ++ ": " ++ err
      Right a -> return a

fieldConstraintsParser :: Parser (PropTree ([NameExp], [BT.Bit]))
fieldConstraintsParser = do
  props <- outerParser
  P.eof
  return props
  where

    outerParser :: Parser (PropTree ([NameExp], [BT.Bit]))
    outerParser = do
      mkPropList <$> P.sepBy1 combinedParser ((void $ P.char ';') <|> (void $ P.chunk "&&"))

    combinedParser :: Parser (PropTree ([NameExp], [BT.Bit]))
    combinedParser = do
      P.takeWhileP Nothing (== ' ')
      props <- P.choice [ negParser
                        , P.between (P.char '(') (P.char ')') outerParser
                        , atomParser
                        ]
      P.takeWhileP Nothing (== ' ')
      return props

    negParser :: Parser (PropTree ([NameExp], [BT.Bit]))
    negParser = do
      P.char '!'
      P.between (P.char '(') (P.char ')') (mkPropNegate <$> outerParser)

    atomParser :: Parser (PropTree ([NameExp], [BT.Bit]))
    atomParser = do
      name <- nameExpsParser
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

-- | A wrapper around 'DT.OperandDescriptor' to separate real and pseudo-operands
data Operand =
    RealOperand DT.OperandDescriptor
    -- ^ a real operand that is used by the semantics
  | PsuedoOperand DT.OperandDescriptor
    -- ^ a psuedo-operand that only exists in order to avoid having unused bits that
    -- would be otherwise zeroed-out on reassembly.
    -- A psuedo-operand is created when a field is unnamed (i.e. not corresponding to an operand)
    -- but has a quasimask - bits that are not present in the encoding mask but
    -- are required to be a specific value in order to have a defined instruction semantics
  deriving (Show)

operandDescOfOperand :: Operand -> DT.OperandDescriptor
operandDescOfOperand = go
  where
    go (RealOperand op) = op
    go (PsuedoOperand op) = op

-- | A specialization of an instruction given a set of flags
data Encoding = Encoding { encName :: String
                         -- ^ the unique name of this encoding (e.g. ADD_i_A1, ADDS_i_A1 )
                         , encMnemonic :: String
                         -- ^ the mnemonic of the instruction class that this encoding belongs to (e.g. aarch32_ADD_i_A )
                         -- shared between multiple encodings
                         , encConstraints :: PropTree (BitSection BT.Bit)
                         -- ^ the bitfield constraints that identify this specific encoding
                         , encOperands :: [Operand]
                         -- ^ the operand descriptors for this encoding
                         -- some operands comprise multiple fields (e.g. SIMD register Dm = Vm:M )
                         -- some operands are psuedo-operands derived from quasimask field bits
                         , encFields :: Fields
                         -- ^ the named bitfields of the instruction
                         , encIConstraints :: PropTree (BitSection QuasiBit)
                         -- ^ the constraints of this encoding that are common to all
                         -- the encodings of the instruction class it belongs to
                         , encMask :: [QuasiBit]
                         -- ^ the complete positive mask of this encoding (derived from the constraints)
                         , encNegMasks :: [[BT.Bit]]
                         -- ^ the complete negative masks of this encoding (derived from the constraints)
                         }
  deriving (Show)

encFullConstraints :: Encoding -> PropTree (BitSection QuasiBit)
encFullConstraints enc = (fmap (fmap Bit) $ encConstraints enc) <> encIConstraints enc

-- | Notably, the sectBitPos field starts from the most significant bit (i.e. bit 31 in aarch32)
data BitSection a = BitSection { sectBitPos :: Int
                               , sectBits :: [a]
                               }
  deriving (Show, Eq, Functor, Foldable)

sectWidth :: BitSection a -> Int
sectWidth bsect = length (sectBits bsect)

-- | Make a bitsection from a hibit of 32 bits
bitSectionHibit :: Int -> [a] -> BitSection a
bitSectionHibit hibit bits = BitSection (31 - hibit) bits

unpackFieldConstraints :: Show a => ([Field], [a]) -> XML [BitSection a]
unpackFieldConstraints (flds, bits) = do
  (rest, result) <- foldM go' (bits, []) flds
  case rest of
    [] -> return result
    _ -> throwError $ MismatchedFieldsForBits flds bits
  where
    go' :: ([a], [BitSection a]) -> Field -> XML ([a], [BitSection a])
    go' (as, results) nme = do
      (as', result) <- go nme as
      return (as', result : results)

    go :: Field -> [a] -> XML ([a], BitSection a)
    go field as = do
      let width = fieldWidth field
      let hibit = fieldHibit field
      let (chunk, rest) = splitAt width as
      unless (length chunk == width) $ do
        throwError $ MismatchedFieldsForBits flds bits
      return (rest, bitSectionHibit hibit chunk)

resolvePropTree :: forall a
                 . Show a
                => Fields
                -> PropTree ([NameExp], [a])
                -> XML (PropTree (BitSection a))
resolvePropTree fields tree =
  fmap concatPropTree $ mapM go tree
  where
    go :: ([NameExp], [a]) -> XML [BitSection a]
    go (nmes, as) = do
      fields' <- mapM (lookupField fields) nmes
      unpackFieldConstraints (fields', as)


leafGetEncodingConstraints :: InstructionLeaf -> Fields -> XML [(String, PropTree (BitSection BT.Bit))]
leafGetEncodingConstraints ileaf fields = do
  let iclass = ileafiClass ileaf
  forChildren "encoding" iclass $ \encoding -> do
    name <- getAttr "name" encoding
    withEncodingName name $ do
      case X.findAttr (qname "bitdiffs") encoding of
        Nothing -> return $ (name, emptyPropTree)
        Just bitdiffs -> do
          parsedConstraints <- parseString fieldConstraintsParser bitdiffs
          constraints <- resolvePropTree fields parsedConstraints
          return $ (name, constraints)

-- | Not all parsed explanations will be used (i.e. if they are only for encodings not for this architecture).
-- So we leave the "encodedin" field as uninterpreted here, as it can only be resolved with respect to
-- an architecture.
parseExplanation :: X.Element -> XML (Maybe ([String], (String, (String, RegisterInfo))))
parseExplanation explanation = do
    encs <- parseEncList <$> getAttr "enclist" explanation
    withChild "account" explanation $ \case
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
        throwError $ UnexpectedAttributeValue "encodedin" encodedin
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
            Nothing -> throwError $ MissingRegisterInfo baseregister (map fst explanations)
        IndexBasic -> do
          fieldDescription <- case regMode rinfo of
            ImplicitEncoding mimplicitName -> do
              let implicitName = fromMaybe symbolName mimplicitName
              unless (null encodedin || encodedin == implicitName) $ do
                throwError $ UnexpectedAttributeValue "encodedin" encodedin
              return implicitName
            _ -> return encodedin
          when (fieldDescription == "") $ do
            throwError $ InvalidRegisterInfo rinfo
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
  _ -> throwError $ InvalidRegisterInfo rinfo

lookupEncIndexMask :: InstructionLeaf -> String -> XML (Maybe (([QuasiBit], [[BT.Bit]]), Bool))
lookupEncIndexMask leaf encname = do
  tbl <- MS.gets encodingTableMap
  case M.lookup (IdentEncodingName encname) tbl of
    Just masks -> return $ Just (masks, True)
    Nothing -> do
      Just curFile <- R.asks xmlCurrentFile
      iclassname <- getAttr "name" (ileafiClass leaf)
      case M.lookup (IdentFileClassName iclassname curFile) tbl of
        Just masks -> return $ Just (masks, False)
        Nothing -> return Nothing

validateEncoding :: InstructionLeaf -> Encoding -> XML ()
validateEncoding leaf encoding = do
  lookupEncIndexMask leaf (encName encoding) >>= \case
    Just ((mask', negmasks'), exact) -> do
      let mask = encMask encoding
      let negmasks = encNegMasks encoding
      let check = if exact then (==) else matchesMask
      unless (check (flattenMask mask) (flattenMask mask')) $
        throwError $ MismatchedMasks mask mask'
      unless (length negmasks == length negmasks' && all (uncurry check) (zip (sort negmasks) (sort negmasks'))) $
        throwError $ MismatchedNegativeMasks negmasks negmasks'
    Nothing -> throwError $ MissingEncodingTableEntry (encName encoding)

-- | Build operand descriptors out of the given fields
leafGetEncodings :: InstructionLeaf
                 -> Fields
                 -> [Operand]
                 -> PropTree (BitSection QuasiBit)
                 -> XML [Encoding]
leafGetEncodings ileaf allfields operands iconstraints = do
  let iclass = ileafiClass ileaf
  mnemonic <- leafMnemonic' ileaf

  encodingConstraints <- leafGetEncodingConstraints ileaf allfields
  explanations <- getChild "explanations" (ileafFull ileaf)
  parsedExplanations <- fmap catMaybes $ forChildren "explanation" explanations parseExplanation

  registerSources <- fmap (M.fromListWith (++) . concat) $
    forM parsedExplanations $ \(encs, v) -> do
      return $ [ (e, [v]) | e <- encs ]

  forM encodingConstraints $ \(encName', constraints) -> withEncodingName encName' $ do
      let rawRegisters = fromMaybe [] $ M.lookup encName' registerSources
      rinfos <- processExplanations allfields rawRegisters

      let usedFieldNames = concat $ map (\(_, _, fields) -> map fieldName fields) rinfos
      let unusedFields = M.elems $ M.withoutKeys allfields (S.fromList usedFieldNames)

      registerOps <- mapM mkRegisterOp rinfos
      immediates <- mapM mkImmediateOp unusedFields
      (mask, negmasks) <- deriveMasks iconstraints constraints

      let encoding = Encoding { encName = encName'
                              , encMnemonic = mnemonic
                              , encConstraints = constraints
                              , encOperands = operands ++ map RealOperand (registerOps ++ immediates)
                              , encFields = allfields
                              , encIConstraints = iconstraints
                              , encMask = mask
                              , encNegMasks = negmasks
                              }
      validateEncoding ileaf encoding `ME.catchError` warnError

      MS.modify' $ \st -> st { encodingMap = M.insert encName' encoding (encodingMap st) }
      return encoding
  where
    mkRegisterOp :: (String, RegisterInfo, [Field]) -> XML DT.OperandDescriptor
    mkRegisterOp (name, rinfo, fields) = do
      let totalBits = sum $ map fieldWidth fields
      chunks <- forM fields $ \(Field name hibit width _ _ _) -> do
        return (DT.IBit (hibit - width + 1), PT.OBit 0, fromIntegral width)
      opType <- getRegisterOperandType totalBits rinfo

      return $ DT.OperandDescriptor { DT.opName = name
                                    , DT.opChunks = chunks
                                    , DT.opType = opType
                                    }

    mkImmediateOp :: Field -> XML DT.OperandDescriptor
    mkImmediateOp field@(Field name hibit width _ _ _) = do
      let opType = getFieldOpType field
      return $ DT.OperandDescriptor { DT.opName = name
                                    , DT.opChunks = [( DT.IBit (hibit - width + 1)
                                                     , PT.OBit 0
                                                     , fromIntegral width)]
                                    , DT.opType = DT.OperandType opType
                                    }

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
                        , subConstraint 4 pos == Right (replicate 4 (QBit True)) ->
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
    _ -> throwError $ NoUniqueiClass name


findXMLFile :: String -> XML FilePath
findXMLFile fileName = do
  allfiles <- R.asks xmlAllFiles
  case find (\f -> takeFileName f == fileName) allfiles of
    Just fn -> return fn
    Nothing -> throwError $ MissingXMLFile fileName

isAliasedInstruction :: InstructionLeaf -> Bool
isAliasedInstruction ileaf = isJust $ X.findChild (qname "aliasto") (ileafFull ileaf)

leafMnemonic' :: InstructionLeaf -> XML String
leafMnemonic' ileaf = do
  regdiagram <- getChild "regdiagram" iclass
  psname <- getAttr "psname" regdiagram
  case psname of
    "" -> lookupAlias
    _ -> parseString nameParser psname
  where
    iclass = ileafiClass ileaf

    lookupAlias :: XML String
    lookupAlias = do
      alias <- getChild "aliasto" (ileafFull ileaf)
      iclassname <- getAttr "name" iclass
      aliasedXMLFile <- getAttr "refiform" alias >>= findXMLFile
      withParsedXMLFile aliasedXMLFile $ \aliasedXML -> do
        aliasediclass <- findiClassByName aliasedXML iclassname
        leafMnemonic' (InstructionLeaf aliasedXML aliasediclass)

forChildrenWithAttr :: String -> X.Element -> (X.Element -> String -> XML a) -> XML [a]
forChildrenWithAttr aname elt m = catMaybes <$> (forM (X.elChildren elt) $ \child -> do
  case X.findAttr (qname aname) child of
    Just v -> Just <$> m child v
    Nothing -> return Nothing)

forChildren :: String -> X.Element -> (X.Element -> XML a) -> XML [a]
forChildren name elt m = mapM (\e -> withElement e $ m e) $ X.filterChildrenName ((==) $ qname name) elt

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

mergeBits :: ME.MonadError String m => BT.Bit -> BT.Bit -> m BT.Bit
mergeBits b1 b2 = case (b1, b2) of
  (BT.ExpectedBit x, BT.ExpectedBit y) | x == y -> return $ BT.ExpectedBit y
  (BT.Any, x) -> return x
  (x, BT.Any) -> return x
  _ -> throwError $ "Incompatible bits: " ++ show b1 ++ " " ++ show b2

mergeQBits :: ME.MonadError String m => QuasiBit -> QuasiBit -> m QuasiBit
mergeQBits qb1 qb2 = case (qb1, qb2) of
  (QBit x, QBit y) | x == y -> return $ QBit y
  (QBit x, Bit (BT.ExpectedBit y)) | x == y -> return $ Bit $ BT.ExpectedBit y
  (Bit (BT.ExpectedBit y), QBit x) | x == y -> return $ Bit $ BT.ExpectedBit y
  (x, Bit BT.Any) -> return x
  (Bit BT.Any, x) -> return x
  (Bit b1, Bit b2) -> Bit <$> mergeBits b1 b2
  _ -> throwError $ "Incompatible qbits: " ++ show qb1 ++ " " ++ show qb2

zipWithSafe :: forall m a b c. (Show a, Show b) => ME.MonadError String m => (a -> b -> m c) -> [a] -> [b] -> m [c]
zipWithSafe f a b = do
  unless (length a == length b) $
    throwError "zipWithSafe: length mismatch"
  zipWithM f' (zip a [0..]) b
  where
    f' :: (a, Int) -> b -> m c
    f' (a, i) b = f a b
      `ME.catchError`
      (\err -> throwError $ "zipWithSafe: " ++ show a ++ " and " ++ show b ++ " at " ++ show i ++ " " ++ err)

prependErr :: ME.MonadError String m => String -> String -> m a
prependErr msg err = throwError $ msg ++ " " ++ err

-- | Given a list of fields, and a bit pattern whose length is the same as the sum of
-- all the field lengths, return a "full" bit pattern reflecting the effect of
-- combining all those patterns together.
computePattern' :: (Show a, Show b) => ME.MonadError String m => (a -> b -> m b) -> [BitSection a] -> [b] -> m [b]
computePattern' f [] fullPat = return $ fullPat
computePattern' f (fld : rstFlds) fullPat = do
  let
    top = sectBitPos fld
    existing = getSublist top (length $ sectBits fld) fullPat
  resultMask <- zipWithSafe f (sectBits fld) existing
    `ME.catchError`
    (prependErr $ "computePattern': at position: " ++ show top)
  let fullPat' = placeAt (sectBitPos fld) resultMask fullPat
  computePattern' f rstFlds fullPat'

computePattern :: forall m a. Show a => ME.MonadError String m => (a -> a -> m a) -> [BitSection a] -> m [Maybe a]
computePattern merge bitsects =
  computePattern' go bitsects (repeat Nothing)
    `ME.catchError`
    (prependErr $ "computePattern: " ++ show bitsects)
  where
    go :: a -> Maybe a -> m (Maybe a)
    go a (Just a') = Just <$> merge a a'
    go a _ = Just <$> return a

computeBitPattern :: ME.MonadError String m => Int -> [BitSection BT.Bit] -> m [BT.Bit]
computeBitPattern len bitsects = do
  result <- computePattern mergeBits bitsects
  return $ take len (map (fromMaybe BT.Any) result)


getChildWith :: (X.Element -> Bool) -> X.Element -> XML X.Element
getChildWith f elt = case X.filterChild f elt of
  Nothing -> withElement elt $ throwError $ NoMatchingChildElement
  Just child -> return child

getChild :: String -> X.Element -> XML X.Element
getChild name elt = case X.findChild (qname name) elt of
  Nothing -> withElement elt $ throwError $ MissingChildElement name
  Just child -> return child

withChild :: String -> X.Element -> (Maybe X.Element -> XML a) -> XML a
withChild name elt m = do
  case X.findChild (qname name) elt of
    Just elt' -> withElement elt' $ m (Just elt')
    Nothing -> m Nothing


getAttr :: String -> X.Element -> XML String
getAttr name elt = case X.findAttr (qname name) elt of
  Nothing -> withElement elt $ throwError $ MissingAttr name
  Just attr -> return attr

getAttrs :: [String] -> X.Element -> XML [String]
getAttrs names elt = mapM (\name -> getAttr name elt) names

resolvePath :: X.Element -> [(String, Maybe (String, String))] -> XML [X.Element]
resolvePath elt path = withElement elt $ case path of
  (p : ps) -> do
    fmap concat $ forM (go p elt) $ \elt' ->
      resolvePath elt' ps
  _ -> return [elt]
  where
    go :: (String, Maybe (String, String)) -> X.Element -> [X.Element]
    go p e = X.filterChildren (getfilter p) e

    getfilter :: (String, Maybe (String, String)) -> X.Element -> Bool
    getfilter (nm, Nothing) e = X.qName (X.elName e) == nm
    getfilter (nm, Just (attrnm, attrval)) e =
      X.qName (X.elName e) == nm && X.findAttr (qname attrnm) e == Just attrval
