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
import           Data.Maybe (catMaybes, fromMaybe, isJust, isNothing, listToMaybe)
import           Data.PropTree ( PropTree )
import qualified Data.PropTree as PropTree
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

import           Dismantle.ARM.RegisterInfo ( registerInfoParser, RegisterInfo(..), RegisterIndexMode(..), RegisterKind(..), RegisterUsage(..) )
import qualified Dismantle.Tablegen as DT
import qualified Dismantle.Tablegen.ByteTrie as BT
import qualified Dismantle.Tablegen.Parser.Types as PT

import Debug.Trace

data XMLException = MissingChildElement String
                       | NoMatchingChildElement
                       | MissingAttr String
                       | MissingField String Fields
                       | MissingEncoding String
                       | MissingFieldForRegister (RegisterInfo String) String
                       | InvalidChildElement
                       | MnemonicError String
                       | InvalidPattern String
                       | MismatchedFieldWidth Field Int
                       | forall a. Show a => MismatchedFieldsForBits [Field] [a]
                       | MissingRegisterInfo String [RegisterInfo String]
                       | forall a. Show a => MismatchedWidth Int [a]
                       | InvalidXmlFile String
                       | forall e. (Show e, P.ShowErrorComponent e) => InnerParserFailure (P.ParseErrorBundle String e)
                       | UnexpectedAttributeValue String String
                       | forall a. Show a => InvalidRegisterInfo (RegisterInfo a)
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
                       | MismatchedRegisterInfos (RegisterInfo String) (RegisterInfo String)


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
    InnerParserFailure e -> PP.text "InnerParserFailure"
      $$ PP.nest 1 (PP.vcat $ (map PP.text $ lines (P.errorBundlePretty e)))
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
    go [e] = simplePrettyElem e <+> (case X.elLine e of
      Just l -> PP.text "| line:" <+> PP.int (fromIntegral l)
      Nothing -> PP.empty)
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
loadXML ::  String -> [FilePath] -> FilePath -> (String -> IO ()) -> IO DT.ISADescriptor
loadXML arch xmlFiles xmlEncIndex logFn = do
  result <- runXML arch xmlFiles logFn $ do
    withParsedXMLFile xmlEncIndex loadEncIndex
    instrs <- fmap concat $ forM xmlFiles $ (\f -> withParsedXMLFile f loadInstrs)
    return $ DT.ISADescriptor { DT.isaInstructions = instrs
                              , DT.isaOperands = S.toList (S.fromList (concatMap instrOperandTypes instrs))
                              , DT.isaErrors = []
                              }
  case result of
    Left err -> do
      logFn (show err)
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
    return $ PropTree.negate $ PropTree.clause $ (flds, bits)
  resolvePropTree fields (mconcat rawConstraints)

instrTableConstraints :: [[Field]] -> X.Element -> XML (PropTree (BitSection BT.Bit))
instrTableConstraints tablefieldss tr = do
  "instructiontable" <- getAttr "class" tr
  let tds = X.filterChildren (\e -> X.findAttr (qname "class") e == Just "bitfield") tr
  fmap mconcat $ forM (zip tds [0..]) $ \(td, i) -> do
    let fields = tablefieldss !! i
    let width = sum $ map fieldWidth fields
    bitwidth <- read <$> getAttr "bitwidth" td
    -- soft-throw this error since the XML is occasionally wrong but this is recoverable
    unless (width == bitwidth) $
      warnError $ MismatchedWidth bitwidth fields
    fmap PropTree.collapse $ parseConstraint width (X.strContent td) $ \bits -> do
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
      Nothing -> return mempty
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
           , mconcat (map fieldConstraint fields))
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
parseConstraint width "" m = PropTree.clause <$> m (replicate width BT.Any)
parseConstraint width str m = do
  (bits, isPositive) <- parseString constraintParser str
  unless (length bits == width) $
    throwError $ MismatchedWidth width bits
  let sign = if isPositive then id else PropTree.negate
  sign . PropTree.clause <$> m bits

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
      return $ PropTree.clause $ bitSectionHibit hibit bits
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

parseElement :: Show e => P.ShowErrorComponent e => P.Parsec e String a -> X.Element -> XML a
parseElement p e = withElement e $ parseString p (flatText e)

parseString :: Show e => P.ShowErrorComponent e => P.Parsec e String a -> String -> XML a
parseString p txt = do
  currentFile <- (fromMaybe "") <$> R.asks xmlCurrentFile
  line <- (fromMaybe 1 . (listToMaybe >=> X.elLine)) <$> R.asks xmlCurrentPath
  case P.runParser p currentFile txt of
    Left err -> throwError $ InnerParserFailure $
      err { P.bundlePosState = setLine line (P.bundlePosState err) }
    Right a -> return a


-- | Absorb parser errors for 'Maybe' types
parseStringCatch :: Show e => P.ShowErrorComponent e => P.Parsec e String (Maybe a) -> String -> XML (Maybe a)
parseStringCatch p txt = parseString p txt `ME.catchError` \err -> warnError err >> return Nothing

-- | Absorb parser errors for 'Maybe' types
parseElementCatch :: Show e => P.ShowErrorComponent e => P.Parsec e String (Maybe a) -> X.Element -> XML (Maybe a)
parseElementCatch p e = withElement e $ parseStringCatch p (flatText e)

setLine :: Integer -> P.PosState s -> P.PosState s
setLine line ps = ps { P.pstateSourcePos  = (P.pstateSourcePos ps) { P.sourceLine = P.mkPos (fromIntegral line) } }

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
deriveMasks' constraints = case PropTree.toConjunctsAndDisjuncts constraints of
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
      mconcat <$> P.sepBy1 combinedParser ((void $ P.char ';') <|> (void $ P.chunk "&&"))

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
      P.between (P.char '(') (P.char ')') (PropTree.negate <$> outerParser)

    atomParser :: Parser (PropTree ([NameExp], [BT.Bit]))
    atomParser = do
      name <- nameExpsParser
      negate <- (P.chunk " == " >> return False) <|> (P.chunk " != " >> return True)
      bits <- P.some bitParser
      P.takeWhileP Nothing (== ' ')
      if negate
        then return $ PropTree.negate $ PropTree.clause (name, bits)
        else return $ PropTree.clause (name, bits)

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
  fmap PropTree.collapse $ mapM go tree
  where
    go :: ([NameExp], [a]) -> XML [BitSection a]
    go (nmes, as) = do
      fields' <- mapM (lookupField fields) nmes
      unpackFieldConstraints (fields', as)


leafGetEncodingConstraints :: InstructionLeaf
                           -> Fields
                           -> XML [(String, PropTree (BitSection BT.Bit), [RegisterInfo String])]
leafGetEncodingConstraints ileaf fields = do
  let iclass = ileafiClass ileaf
  forChildren "encoding" iclass $ \encoding -> do
    name <- getAttr "name" encoding
    withEncodingName name $ do
      reginfos <- reginfoFromAsmTemplate encoding
      constraints <-  case X.findAttr (qname "bitdiffs") encoding of
        Nothing -> return mempty
        Just bitdiffs -> do
          parsedConstraints <- parseString fieldConstraintsParser bitdiffs
          resolvePropTree fields parsedConstraints
      return (name, constraints, reginfos)

-- | Parse the register info from the asmtemplate
reginfoFromAsmTemplate :: X.Element -> XML [RegisterInfo String]
reginfoFromAsmTemplate encoding = do
  asmtemplate <- getChild "asmtemplate" encoding
  fmap catMaybes $ forChildren "a" asmtemplate $ \a -> do
    case X.findAttr (qname "hover") a of
      Just explanation -> do
        symbolName <- parseElement registerNameParser a
        parseStringCatch (registerInfoParser symbolName Nothing) explanation
      Nothing -> return Nothing


registerNameParser :: Parser String
registerNameParser = braks <|> P.takeWhile1P Nothing (const True)
  where
    braks = do
      P.char '<'
      name <- P.takeWhile1P Nothing (/= '>')
      P.char '>'
      return name

-- | Not all parsed explanations will be used (i.e. if they are only for encodings not for this architecture).
-- So we leave the "encodedin" field as uninterpreted here, as it can only be resolved with respect to
-- an architecture.
parseExplanation :: X.Element -> XML (Maybe ([String], RegisterInfo String))
parseExplanation explanation = do
    encs <- parseEncList <$> getAttr "enclist" explanation

    mbody <- withChild "account" explanation $ \case
      Just account -> do
        para <- getChild "intro" account >>= getChild "para"
        return $ Just (account, para)
      Nothing -> withChild "definition" explanation $ \case
        Just definition -> do
          intro <- getChild "intro" definition
          return $ Just (definition, intro)
        Nothing -> return Nothing

    case mbody of
      Just (body, content) -> withElement body $ do
        encodedin <- getAttr "encodedin" body
        symbol <- getChild "symbol" explanation
        symbolName <- parseElement registerNameParser symbol
        let encodedin' = if null encodedin then Nothing else Just encodedin
        parseElementCatch (registerInfoParser symbolName encodedin') content >>= \case
          Just rinfo -> return $ Just (encs, rinfo)
          _ -> return Nothing
      Nothing -> return Nothing


-- | Assign fields to the "encodedin" section of the register.
-- Currently some registers might drop off due to lookup failures. This is (usually) due
-- the register index mode not correctly being interpreted as an 'IndexRegisterOffset' and therefore
-- referring to a field that is actually a logical operand (i.e. Sm1 == Sm + 1)
lookupRegisterInfoFields :: Fields
                         -> [RegisterInfo String]
                         -> XML [RegisterInfo [Field]]
lookupRegisterInfoFields allfields rinfos = catMaybes <$> mapM go rinfos
  where
    go ::  RegisterInfo String -> XML (Maybe (RegisterInfo [Field]))
    go rinfo = (Just <$> go' rinfo) `ME.catchError` \err -> do
      warnError err
      return Nothing

    go' :: RegisterInfo String -> XML (RegisterInfo [Field])
    go' rinfo = do
      case regIndexMode rinfo of
        IndexRegisterOffset baseregister -> do
          case find (\ri -> regSymbolName ri == baseregister) rinfos of
            Just rawinfo -> do
              rinfo' <- go' rawinfo
              return $ rinfo {regEncodedIn = (regEncodedIn rinfo')}
            Nothing -> throwError $ MissingRegisterInfo baseregister rinfos
        _ -> do
          when (null (regEncodedIn rinfo)) $
            throwError $ InvalidRegisterInfo rinfo
          exps <- nameExps (regEncodedIn rinfo)
          fields <- forM exps $ lookupField allfields
          return $ rinfo { regEncodedIn = fields }

mergeRegisterInfos :: [RegisterInfo String]
                   -> [RegisterInfo String]
                   -> XML [RegisterInfo String]
mergeRegisterInfos rinfos rinfos' = do
  let matched = M.assocs $ M.fromListWith (++) $ map (\rinfo -> (regSymbolName rinfo, [rinfo])) (rinfos ++ rinfos')
  forM matched $ \(_, (rinfo : rinfos)) -> do
    rinfo' <- foldM go rinfo rinfos
    return rinfo'
  where
    go :: RegisterInfo String -> RegisterInfo String -> XML (RegisterInfo String)
    go rinfo rinfo' = do
      unless (rinfo { regSourceText = "" }  == rinfo' { regSourceText = "" }) $
        warnError $ MismatchedRegisterInfos rinfo rinfo'
      return rinfo

getRegisterOperandType :: RegisterInfo [Field] -> XML DT.OperandType
getRegisterOperandType rinfo =
  let totalBits = sum $ map fieldWidth (regEncodedIn rinfo)
  in
  DT.OperandType <$> case (regKind rinfo, regIndexMode rinfo) of
  (GPR, IndexRegisterOffset _) | totalBits == 4 -> return "GPR4_1"
  (GPR, IndexSimple) | totalBits == 4 -> return "GPR4"
  (GPR, IndexSimple) | totalBits == 3 -> return "GPR3"
  (GPR, IndexSimple) | (regUsage rinfo) == Banked, totalBits == 6 -> return "GPR_banked"
  (SIMDandFP, IndexRegisterOffset _) | totalBits == 5 -> return "SIMD5_1"
  (SIMDandFP, IndexVector) | totalBits == 7 -> return "SIMD7"
  (SIMDandFP, IndexSimple) | totalBits == 5 -> return "SIMD5"
  (SIMDandFP, IndexSimple) | totalBits == 4 -> return "SIMD4"
  (SIMDandFP, IndexSimple) | totalBits == 3 -> return "SIMD3"
  (SIMDandFP, IndexSimple) | totalBits == 2 -> return "SIMD2"
  (SIMDandFP, IndexMul2) | totalBits == 5 -> return "SIMD5_Mul2"
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

  forM encodingConstraints $ \(encName', constraints, rinfos') -> withEncodingName encName' $ do
      let rinfos'' = fromMaybe [] $ M.lookup encName' registerSources
      rinfos <- mergeRegisterInfos rinfos' rinfos''
      rinfoFields <- lookupRegisterInfoFields allfields rinfos

      let usedFieldNames = concat $ map (\rinfo -> map fieldName (regEncodedIn rinfo)) rinfoFields
      let unusedFields = M.elems $ M.withoutKeys allfields (S.fromList usedFieldNames)

      registerOps <- mapM mkRegisterOp rinfoFields
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
    mkRegisterOp :: RegisterInfo [Field] -> XML DT.OperandDescriptor
    mkRegisterOp rinfo = do
      chunks <- forM (regEncodedIn rinfo) $ \(Field name hibit width _ _ _) -> do
        return (DT.IBit (hibit - width + 1), PT.OBit 0, fromIntegral width)
      opType <- getRegisterOperandType rinfo

      return $ DT.OperandDescriptor { DT.opName = regSymbolName rinfo
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
       ('R' : _ : [], 4) | (pos, []) <- PropTree.splitClauses constraint
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
