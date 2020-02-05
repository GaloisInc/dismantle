{-# LANGUAGE GADTs, DataKinds #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeApplications #-}

module Dismantle.ARM.ASL
  ( loadASL
  , encodingOpToInstDescriptor
  , instDescriptorsToISA
  )
  where

import           Debug.Trace

import           Prelude hiding ( fail )

import           GHC.Stack
import           GHC.TypeNats

import           Control.Exception ( try, assert )
import qualified Control.Exception as E

import qualified Data.Parameterized.NatRepr as NR

import           Control.Monad ( forM, forM_, zipWithM )
import           Control.Monad.Fail
import           Control.Monad.Identity
import           Control.Monad.Trans ( lift, liftIO, MonadIO )
import           Control.Monad.Trans.RWS.Strict ( RWST )
import qualified Control.Monad.Trans.RWS.Strict as RWS
import qualified Control.Monad.State as MS
import qualified Control.Monad.Reader as MR
import qualified Control.Monad.Writer as MW
import           Control.Monad.Except ( throwError, ExceptT, MonadError )
import qualified Control.Monad.Except as ME

import           Data.Maybe ( catMaybes, fromMaybe, isJust )
import           Data.List ( intercalate, groupBy )
import           Data.Map ( Map )
import qualified Data.Map as Map
-- FIXME: move or use library version of this
import qualified Dismantle.ARM.XML as Map ( fromListWithM )
import qualified Data.List.Split as LS
import qualified Data.Set as S
import qualified Data.Text as T
import           Data.Word ( Word8 )

import           System.IO ( withFile, IOMode(..), hPutStrLn )

import           Text.PrettyPrint.HughesPJClass ( (<+>), ($$), ($+$) )
import qualified Text.PrettyPrint.HughesPJClass as PP
import           Text.Printf (printf)

import qualified Language.ASL.Parser as ASL
import qualified Language.ASL.Syntax as ASL

import qualified Dismantle.ARM.XML as XML
import           Dismantle.ARM.XML ( ARMRegWidth, ARMBitSection
                                   , ARMBitMask, armRegWidthRepr
                                   , Encoding(..), Field(..) )
import qualified Dismantle.Tablegen as DT
import qualified Dismantle.Tablegen.Parser.Types as PT
import qualified Dismantle.Tablegen.ByteTrie as BT
import qualified Data.BitMask as BM

import           Data.PropTree ( PropTree )
import qualified Data.PropTree as PropTree

type ARMMaskTrie a = BM.MaskTrie BM.QuasiBit ARMRegWidth a

data ASLEnv = ASLEnv { envLogFn :: String -> IO ()
                     , envArchName :: String
                     , envContext :: ASLContext
                     -- ^ the current context for error reporting
                     }

data ASLContext = ASLContext { ctxASLEncoding :: Maybe (ASL.Instruction, ASL.InstructionEncoding)
                             , ctxXMLEncoding :: Maybe Encoding
                             , ctxStmt :: Maybe ASL.Stmt
                             , ctxFileName :: String
                             , ctxSourceLoc :: Maybe SrcLoc
                             }

data ASLState = ASLState { stMaskTrie :: ARMMaskTrie [(String, ARMBitMask BM.QuasiBit)]
                         , stEncodingMap :: Map String (ASL.Instruction, ASL.InstructionEncoding)
                         }

type ASLOut = ()

data ASLException =
    ASLFail String
  | UnsupportedArch String
  | MissingExpectedMaskTreeEntry
  | BadMaskTreeLookup String
  | forall a. Show a => InstructionIdentNameClash a a
  | NotImplementedYet String
  | IncompatibleFieldBits String
  | InvalidBitMaskLength Int
  | InvalidBitPosition Int
  | InvalidASLField ASL.InstructionField
  | InvalidXMLField XML.Field
  | FailedToDeriveMask String
  | MissingEncodingForIdentifier String
  | UnexpectedMatchingEntries (ARMBitMask BM.QuasiBit) [(String, ARMBitMask BM.QuasiBit)]
  | UnexpectedSplitFields (ARMBitMask FixedFieldBit)
  | ASLParserError String
  | UnexpectedFieldWidth String Int Int
  | MissingConstraintForField Encoding String [String]
  | MultipleExceptions [ASLException]

deriving instance Show ASLException

data OuterASLException = OuterASLException ASLContext ASLException

instance E.Exception OuterASLException

newtype ASL a = ASL (RWST ASLEnv ASLOut ASLState (ExceptT OuterASLException IO) a)
  deriving ( Functor
           , Applicative
           , Monad
           , MS.MonadState ASLState
           , MR.MonadReader ASLEnv
           , MW.MonadWriter ASLOut
           , MonadIO
           )

instance MonadFail ASL where
  fail msg = throwError $ ASLFail msg

instance MonadError ASLException ASL where
  throwError e = do
    ctx <- MR.asks envContext
    ASL (lift $ throwError $ OuterASLException ctx e)

  catchError (ASL m) handler = do
    st <- MS.get
    env <- MR.ask
    (liftIO $ ME.runExceptT $ RWS.runRWST m env st) >>= \case
      Left (OuterASLException _ e) -> handler e
      Right (a, st', out) -> do
        MS.put st'
        MW.tell out
        return a

throwErrorHere :: HasCallStack => ASLException -> ASL a
throwErrorHere e = do
  let (_, src): _ = getCallStack callStack
  ctx <- MR.asks envContext
  ASL (lift $ throwError $ OuterASLException (ctx { ctxSourceLoc = Just src }) e)

instance PP.Pretty OuterASLException where
  pPrint (OuterASLException ctx e) =
    let errPretty = PP.nest 1 $ PP.pPrint e in
    PP.text "Dismantle.ARM.ASL: Error encountered while processing:" <+> PP.text (ctxFileName ctx)
    <+> case ctxSourceLoc ctx of
          Just loc -> PP.text "at" <+> PP.text (prettySrcLoc loc)
          Nothing -> PP.empty
    <+> case (ctxASLEncoding ctx) of
          Just (instr, enc) -> PP.text "for ASL encoding:" <+> PP.text (encodingIdentifier instr enc)
          _ -> PP.empty
    <+> case (ctxXMLEncoding ctx) of
          Just enc -> PP.text "for XML encoding:" <+> PP.text (encName enc)
          _ -> PP.empty
    $$ case ctxStmt ctx of
          Just stmt -> PP.text "at statement:" $$ (PP.text $ show stmt) $$ errPretty
          _ -> errPretty

instance Show OuterASLException where
  show e = PP.render (PP.pPrint e)

instance PP.Pretty ASLException where
  pPrint e = case e of
    UnexpectedMatchingEntries mask [] ->
      PP.text "Missing entry for:"
      $$ prettyMask mask
    UnexpectedMatchingEntries mask matches ->
      let content = PP.nest 1 (PP.space $$ PP.text "Entries:" $$ PP.vcat (map prettyMatch matches)) in
      PP.text "Multiple entries found for:"
      $$ prettyMask mask
      $$ content
      where
        prettyMatch :: (String, ARMBitMask BM.QuasiBit) -> PP.Doc
        prettyMatch (nm, mask') = PP.text nm PP.<> PP.text ":" $$ PP.nest 1 (prettyMask mask')
    UnexpectedFieldWidth fieldName expected actual ->
      PP.text "Unexpected width for field: " <+> PP.text fieldName
      $$ PP.text "Expected:" <+> PP.int expected <+> PP.text "Got:" <+> PP.int actual
    MissingConstraintForField encoding fieldname foundfields ->
      PP.text "MissingConstraintForField"
      $$ PP.pPrint encoding
    MultipleExceptions es -> PP.vcat $ map PP.pPrint es
    _ ->
      PP.text (show e)


prettyMask :: ARMBitMask BM.QuasiBit -> PP.Doc
prettyMask mask = BM.prettySegmentedMask endianness mask

prettyInstructionEncoding :: ASL.Instruction -> ASL.InstructionEncoding -> PP.Doc
prettyInstructionEncoding instr enc = PP.hcat $ PP.punctuate (PP.text "/") $
   [ PP.text (T.unpack $ ASL.instName instr)
   , PP.text (T.unpack $ ASL.encName enc)
   , PP.text (show $ ASL.encInstrSet enc)
   ]

logMsg :: String -> ASL ()
logMsg msg = do
  logFn <- MR.asks envLogFn
  liftIO $ logFn msg

warnError :: ASLException -> ASL ()
warnError e = do
  ctx <- MR.asks envContext
  let pretty = PP.nest 1 (PP.text "WARNING:" $$ (PP.pPrint $ OuterASLException ctx e))
  logMsg $ PP.render pretty

runASL :: String
           -> FilePath
           -> (String -> IO ())
           -> ASL a
           -> IO (Either OuterASLException (a, ASLState, ASLOut))
runASL archName aslfile logFn (ASL m) =
  let
    initCtx = ASLContext { ctxFileName = aslfile
                            , ctxXMLEncoding = Nothing
                            , ctxSourceLoc = Nothing
                            , ctxASLEncoding = Nothing
                            , ctxStmt = Nothing
                            }
    initEnv = ASLEnv { envLogFn = logFn
                        , envContext = initCtx
                        , envArchName = archName
                        }
    initState = ASLState { stMaskTrie = BM.emptyMaskTrie
                            , stEncodingMap = Map.empty
                            }

  in ME.runExceptT $ RWS.runRWST m initEnv initState

execASL :: String
            -> FilePath
            -> (String -> IO ())
            -> ASL a
            -> IO (Either OuterASLException a)
execASL archName aslfile logFn m = do
  eresult <- runASL archName aslfile logFn m
  return $ do
    (a, _, _) <- eresult
    return a

-- | Wrapper around an 'Encoding' that includes an identifier pointing to which ASL
-- instruction this 'Encoding' points to, as well as how to concretize the given ASL
-- instruction to the given encoding.

data EncodingOp = EncodingOp { encASLIdent :: String
                             , encASLConstraints :: [FieldConstraint]
                             , encASLNegConstraints :: [[FieldConstraint]]
                             , encEncoding :: Encoding
                             }

-- | Annotate the 'Encoding's from the XML specification with an identifier for its correspoding
-- ASL encoding. Identifiers are derived with 'encodingIdentifier' and are checked for uniqueness.
-- Note that some encodings are duplicated in the ASL, so we check that any identifer clashes
-- point to syntactically-equivalent ASL instruction specifications.
--
-- The masks for ASL encodings are more permissive than the XML encodings, thus each ASL encoding
-- may correspond to multiple XML encodings. Each XML encoding is therefore annotated with any additional
-- constraints (positive and negative) on the named fields of its corresponding ASL encoding.
-- The 'EncodingOp' list here represents the bijective mapping: XML encoding <-> ASL encoding + constraints.
loadASL :: String -> FilePath -> [Encoding] -> (String -> IO ()) -> IO [EncodingOp]
loadASL archName aslFile xmlEncodings logFn = do
  logFn "Dismantle.ASL.ASL: loadASL"
  result <- runASL archName aslFile logFn $ do
    instrs <- parseInstsFile aslFile >>= deduplicateEncodings
    forM instrs $ \(instr, enc) -> do
      let ident = encodingIdentifier instr enc
      mask <- maskFromEncoding enc
      MS.modify' $ \st -> st { stMaskTrie = BM.addToMaskTrie mask [(ident, mask)] (stMaskTrie st) }
      MS.modify' $ \st -> st { stEncodingMap = Map.insert ident (instr,enc) (stEncodingMap st) }

    forM xmlEncodings $ \xmlEncoding -> withXMLEncoding xmlEncoding $ do
      aslIdent <- lookupEncoding xmlEncoding
      (instr, aslEncoding) <- lookupASLEncoding aslIdent

      withASLEncoding (instr, aslEncoding) $ do
        encodingop <- correlateEncodings aslIdent aslEncoding xmlEncoding
        logMsg $ "Correlated XML encoding: " ++ (encName xmlEncoding) ++ " with ASL " ++ aslIdent
        logMsg $ "with constraints:"
        logMsg $ PP.render (prettyFieldConstraints (encASLConstraints encodingop) (encASLNegConstraints encodingop))
        return encodingop

  case result of
    Left err -> do
      logFn (show err)
      E.throw err
    Right (a, _st, _out) -> return a

getTargetInstrSet :: ASL ASL.InstructionSet
getTargetInstrSet = do
  MR.asks envArchName >>= \case
    "A32" -> return ASL.A32
    "T32" -> return ASL.T32
    arch -> throwErrorHere $ UnsupportedArch arch

-- | A field bit is an optionally-tagged bit which corresponds to the named field that it belongs to.
newtype FieldBit = FieldBit (BM.QuasiBit, BM.WithBottom (BM.AsBit String))
  deriving (Eq, Ord, Show, BM.SemiMaskBit, BM.HasBottomMaskBit)

instance BM.ShowableBit FieldBit where
  showBit fieldBit = case nameOfFieldBit fieldBit of
    Just name -> BM.showBit (bitOfFieldBit fieldBit) ++ name
    Nothing -> BM.showBit (bitOfFieldBit fieldBit)

-- | A fixed-field bit is either a tagged bit corresponding to a field, or a restricted version
-- of the bit which only matches on equivalence. i.e. a fixed field bit only allows additional
-- constraints to be established for named fields.
newtype FixedFieldBit = FixedFieldBit (Either (BM.QuasiBit, BM.AsBit String) (BM.AsBit BM.QuasiBit))
  deriving (Eq, Ord, Show, BM.SemiMaskBit)

instance BM.ShowableBit FixedFieldBit where
  showBit fieldBit = case nameOfFixedFieldBit fieldBit of
    Just name -> BM.showBit (bitOfFixedFieldBit fieldBit) ++ name
    Nothing -> BM.showBit (bitOfFixedFieldBit fieldBit)

-- | Fix any unnamed bits in a 'FieldBit' as only being matchable with an exactly equivalent bit.
fixFieldBit :: FieldBit -> FixedFieldBit
fixFieldBit (FieldBit (bit, BM.BottomBit)) = FixedFieldBit (Right (BM.AsBit bit))
fixFieldBit (FieldBit (bit, BM.JustBit name)) = FixedFieldBit (Left (bit, name))

-- | Relax a 'FieldBit' to only include its tag.
dropBitOfFieldBit :: FieldBit -> FieldBit
dropBitOfFieldBit (FieldBit (_, name)) = FieldBit (BM.bottomBit, name)

bitOfFieldBit :: FieldBit -> BM.QuasiBit
bitOfFieldBit (FieldBit (bit, _))= bit

bitOfFixedFieldBit :: FixedFieldBit -> BM.QuasiBit
bitOfFixedFieldBit (FixedFieldBit (Left (bit, _))) = bit
bitOfFixedFieldBit (FixedFieldBit (Right (BM.AsBit bit))) = bit

nameOfFieldBit :: FieldBit -> Maybe String
nameOfFieldBit (FieldBit (_, BM.JustBit (BM.AsBit name))) = Just name
nameOfFieldBit (FieldBit _) = Nothing

nameOfFixedFieldBit :: FixedFieldBit -> Maybe String
nameOfFixedFieldBit (FixedFieldBit (Left (_, BM.AsBit name))) = Just name
nameOfFixedFieldBit (FixedFieldBit _) = Nothing

mkFieldBit ::  Maybe String -> BM.QuasiBit -> FieldBit
mkFieldBit (Just name) bit = FieldBit (bit, BM.JustBit (BM.AsBit name))
mkFieldBit Nothing bit = FieldBit (bit, BM.BottomBit)

equivFields :: FixedFieldBit -> FixedFieldBit -> Bool
equivFields fieldBit1 fieldBit2 = nameOfFixedFieldBit fieldBit1 == nameOfFixedFieldBit fieldBit2

computeMask :: [BM.BitSection ARMRegWidth FieldBit] -> ASL (BM.BitMask ARMRegWidth FieldBit)
computeMask fieldSects = case BM.computePattern armRegWidthRepr fieldSects of
  Left msg -> throwErrorHere $ IncompatibleFieldBits msg
  Right merged -> return $ merged

mergeBitMasks :: BM.SemiMaskBit bit
              => BM.ShowableBit bit
              => BM.BitMask n bit
              -> BM.BitMask n bit
              -> ASL (BM.BitMask n bit)
mergeBitMasks mask1 mask2 = case BM.mergeBitErr mask1 mask2 of
  Left msg -> throwErrorHere $ IncompatibleFieldBits msg
  Right merged -> return merged

data FieldConstraint = FieldConstraint { cFieldName :: String
                                       , cFieldMask :: BM.SomeBitMask BM.QuasiBit
                                       }
  deriving (Show, Eq)

instance PP.Pretty FieldConstraint where
  pPrint (FieldConstraint name (BM.SomeBitMask mask)) =
    PP.text name PP.<> PP.text ":" <+> BM.prettyMask mask

cFieldWidth :: FieldConstraint -> Int
cFieldWidth constraint = case (cFieldMask constraint) of
  BM.SomeBitMask mask -> BM.lengthInt mask

prettyFieldConstraints :: [FieldConstraint] -> [[FieldConstraint]] -> PP.Doc
prettyFieldConstraints constraints negconstraints =
  PP.text "Constraints: "
  $$ PP.vcat (map PP.pPrint constraints)
  $$ case negconstraints of
    [] -> PP.empty
    _ -> PP.text "Negative Constraints:"
         $$ PP.vcat (map (\negs -> PP.vcat (map PP.pPrint negs) $$ PP.space) negconstraints)

-- | Separate a mask over 'FieldBit's into individual field constraints.
splitFieldMask :: BM.BitMask ARMRegWidth FixedFieldBit -> ASL (Map String FieldConstraint)
splitFieldMask mask = do
  l <- liftM catMaybes $ forM (groupBy equivFields (BM.toList mask)) $ \(fieldBit : rst) ->
         case nameOfFixedFieldBit fieldBit of
           Just fieldName -> return $ Just (fieldName, fmap bitOfFixedFieldBit $ BM.someBitMaskFromCons fieldBit rst)
           Nothing -> return Nothing
  let noMerge a1 a2 = throwErrorHere $ UnexpectedSplitFields mask
  constraintMap <- Map.fromListWithM noMerge l
  return $ Map.mapWithKey FieldConstraint constraintMap

xmlFieldNameToASL :: String -> String
xmlFieldNameToASL name = case name of
  "type" -> "type1"
  _ -> name

aslFixedFieldMask :: ASL.InstructionEncoding -> ASL (ARMBitMask FixedFieldBit)
aslFixedFieldMask aslEncoding = do
  aslBitsects <- forM (ASL.encFields aslEncoding) $ \field -> do
    let
      fieldName = T.unpack $ ASL.instFieldName field
      fieldLobit = fromIntegral $ ASL.instFieldBegin field
      fieldWidth = fromIntegral $ ASL.instFieldOffset field
      fieldHibit = fieldLobit + fieldWidth - 1
      taggedBv = take fieldWidth $ repeat (mkFieldBit (Just fieldName) BM.bottomBit)
    case BM.bitSectionFromListHiBit fieldHibit taggedBv armRegWidthRepr of
      Nothing -> throwErrorHere $ InvalidASLField field
      Just bitsect -> return $ bitsect

  aslFieldMask <- computeMask aslBitsects
  aslTotalMask <- fmap (mkFieldBit Nothing) <$> maskFromEncoding aslEncoding
  fmap fixFieldBit <$> mergeBitMasks aslFieldMask aslTotalMask

xmlFixedFieldMask :: Encoding -> ASL (ARMBitMask FixedFieldBit, [ARMBitMask FixedFieldBit])
xmlFixedFieldMask xmlEncoding = do
  (xmlMasks, xmlNegMasks) <- liftM unzip $
    forM (filter XML.fieldUseName $ Map.elems $ XML.encFields xmlEncoding) $ \field -> do
      let
        fieldName = xmlFieldNameToASL $ XML.fieldName field
        fieldHibit = XML.fieldHibit field
        fieldWidth = XML.fieldWidth field
        constraint = fmap (fmap $ mkFieldBit (Just fieldName)) $ XML.fieldConstraint field
      case BM.deriveMasks armRegWidthRepr constraint of
        Left msg -> throwError $ IncompatibleFieldBits msg
        Right (mask, negmasks) -> do
          -- merge the tags of negative masks back into the mask to ensure we have all the field tags
          mask' <- foldM mergeBitMasks mask (map (fmap dropBitOfFieldBit) negmasks)
          return (mask', negmasks)
  xmlFieldMask <- foldM mergeBitMasks BM.bottomBit xmlMasks
  xmlTotalMask <- return $ fmap (mkFieldBit Nothing) $ XML.encMask xmlEncoding
  xmlFixedMask <- fmap fixFieldBit <$> mergeBitMasks xmlFieldMask xmlTotalMask
  return (xmlFixedMask, map (fmap fixFieldBit) (concat xmlNegMasks))

-- | Correlate an ASL encoding with an XML encoding by matching up their fields, and potentially
-- determining concrete instantiations for them.
-- Works by creating "tagged" versions of the constraint bitmask for each
-- representation of the encoding (i.e. where each bit knows which field it belongs to).
-- The resulting bitmasks are merged and the field constraint mappings are recovered.
--
-- This serves as a final check that we have found the correct encoding, since
-- every bit is checked that its tagged field name is the same in both masks.
correlateEncodings :: String -> ASL.InstructionEncoding -> Encoding -> ASL EncodingOp
correlateEncodings aslIdent aslEncoding xmlEncoding = do
  aslFixedMask <- aslFixedFieldMask aslEncoding
  (xmlFixedMask, xmlNegMasks) <- xmlFixedFieldMask xmlEncoding
  fixedMask <- mergeBitMasks aslFixedMask xmlFixedMask
  constraintMap <- splitFieldMask xmlFixedMask
  negConstraintMap <-
    liftM (Map.unionsWith (++) . map (fmap (\x -> [x]))) $ mapM splitFieldMask xmlNegMasks

  (posConstraints, negConstraints) <- liftM unzip $ forM (ASL.encFields aslEncoding) $ \field -> do
    let
      fieldName = T.unpack $ ASL.instFieldName field
    pos <- case Map.lookup fieldName constraintMap of
      Just (FieldConstraint _ (BM.SomeBitMask mask))
        | (BM.bottomBitMask $ BM.length mask) == mask -> return []
      Just constraint -> do
        let
          expectedWidth = fromIntegral $ ASL.instFieldOffset field
          constraintWidth = cFieldWidth constraint
        unless (constraintWidth == expectedWidth) $
          throwErrorHere $ UnexpectedFieldWidth fieldName expectedWidth constraintWidth
        return $ [constraint]
      Nothing -> throwErrorHere $ MissingConstraintForField xmlEncoding fieldName (Map.keys constraintMap)
    let neg = fromMaybe [] $ Map.lookup fieldName negConstraintMap
    return (pos, neg)

  return $ EncodingOp { encASLIdent = aslIdent
                      , encASLConstraints = concat posConstraints
                      , encASLNegConstraints = filter (not . null) negConstraints
                      , encEncoding = xmlEncoding
                      }

lookupMasks :: ARMBitMask BM.QuasiBit -> ASL String
lookupMasks mask = do
  masktree <- MS.gets stMaskTrie
  case BM.lookupMaskTrie mask masktree of
    [(result, _)] -> return result
    results | [(result, _)] <- filter exactMatch results -> return result
    x -> throwErrorHere $ UnexpectedMatchingEntries mask x
  where
    exactMatch :: (String, ARMBitMask BM.QuasiBit) -> Bool
    exactMatch (_, mask') = mask == mask'

-- | Lookup the ASL identifier for a given XML 'Encoding' using the 'MaskTrie' in the state.
-- Attempts a lookup with both the general instruction mask as well as the mask from this
-- specific encoding. This ambiguity is resolved later by resolving the ASL bits against
-- the constraints from the XML in 'correlateEncodings'.
lookupEncoding :: Encoding -> ASL String
lookupEncoding encoding = do
  (imask, inegmasks) <- case BM.deriveMasks NR.knownNat (encIConstraints encoding) of
    Left msg -> throwErrorHere $ FailedToDeriveMask msg
    Right (mask, negmasks) -> return $ (mask, negmasks)
  let mask = encMask encoding
  let negmasks = encNegMasks encoding

  lookupMasks imask
    `ME.catchError`
     (\e -> lookupMasks mask
       `ME.catchError`
       (\e' -> throwError $ MultipleExceptions [e, e']))


lookupASLEncoding ident = do
  encMap <- MS.gets stEncodingMap
  case Map.lookup ident encMap of
    Just result -> return result
    Nothing -> throwErrorHere $ MissingEncodingForIdentifier ident

endianness :: [a] -> [a]
endianness bits = concat (reverse (LS.chunksOf 8 bits))

instDescriptorsToISA :: [DT.InstructionDescriptor] -> DT.ISADescriptor
instDescriptorsToISA instrs =
  DT.ISADescriptor { DT.isaInstructions = instrs
                   , DT.isaOperands = S.toList (S.fromList (concatMap instrOperandTypes instrs))
                   , DT.isaErrors = []
                   }

instrOperandTypes :: DT.InstructionDescriptor -> [DT.OperandType]
instrOperandTypes idesc = map DT.opType (DT.idInputOperands idesc ++ DT.idOutputOperands idesc)

sectionToChunks :: ARMBitSection a -> [(DT.IBit, PT.OBit, Word8)]
sectionToChunks sect =
  map getChunk (BM.asContiguousSections sect)
  where
    -- BitSection positions are indexed from the start of the bitmask vector, which is
    -- the most significant bit for ARM (i.e. a big-endian index).
    -- Dismantle wants the little-endian index of the least significant bit of the operand.
    getChunk :: (Int, BM.SomeBitMask a) -> (DT.IBit, PT.OBit, Word8)
    getChunk (pos, BM.SomeBitMask mask) =
      let
        width = BM.lengthInt mask
        regwidth = fromIntegral $ NR.intValue armRegWidthRepr
        hibit = regwidth - pos - 1
        ibitpos = hibit - width + 1
      in (DT.IBit ibitpos, PT.OBit 0, fromIntegral width)


operandToDescriptor :: XML.Operand -> [DT.OperandDescriptor]
operandToDescriptor (XML.Operand name sect isPseudo) = case isPseudo of
  False -> [DT.OperandDescriptor { DT.opName = name
                                 , DT.opChunks = sectionToChunks sect
                                 , DT.opType = DT.OperandType $ printf "Bv%d" totalWidth
                                 }]
  True -> map mkPseudo $ zip [0..] (sectionToChunks sect)
  where
    totalWidth = BM.sectTotalSetWidth sect

    mkPseudo :: (Int, (DT.IBit, PT.OBit, Word8)) -> DT.OperandDescriptor
    mkPseudo (i, chunk@(_, _, width)) =
      DT.OperandDescriptor { DT.opName = printf "QuasiMask%d" i
                           , DT.opChunks = [chunk]
                           , DT.opType = DT.OperandType $ printf "QuasiMask%d" width
                           }


-- | As 'operandToDescriptor' but creates a single, disjointed pseudo-operand instead of multiple.
-- Currently using this will break instruction re-assembly for reasons that are not understood.
operandToDescriptor' :: XML.Operand -> [DT.OperandDescriptor]
operandToDescriptor' (XML.Operand name sect isPseudo) =
  [DT.OperandDescriptor { DT.opName = name
                        , DT.opChunks = sectionToChunks sect
                        , DT.opType = DT.OperandType $ printf opTypeFormat totalWidth
                        }]
  where
    totalWidth = BM.sectTotalSetWidth sect

    opTypeFormat :: String
    opTypeFormat = if isPseudo then "QuasiMask%d" else "Bv%d"

encodingOpToInstDescriptor :: EncodingOp -> DT.InstructionDescriptor
encodingOpToInstDescriptor encodingOp =
  let
    encoding = encEncoding encodingOp
    operandDescs = concat $ map operandToDescriptor $ encOperands $ encEncoding encodingOp
  in DT.InstructionDescriptor
       { DT.idMask = map BM.flattenQuasiBit (endianness $ BM.toList $ encMask encoding)
       , DT.idNegMasks = map (endianness . BM.toList) $ encNegMasks encoding
       , DT.idMnemonic = encName encoding
       , DT.idInputOperands = operandDescs
       , DT.idOutputOperands = []
       , DT.idNamespace = encMnemonic encoding
       , DT.idDecoderNamespace = ""
       , DT.idAsmString = encName encoding
         ++ "(" ++ PP.render (BM.prettySegmentedMask endianness (encMask encoding)) ++ ") "
         ++ simpleOperandFormat operandDescs
       , DT.idPseudo = False
       , DT.idDefaultPrettyVariableValues = []
       , DT.idPrettyVariableOverrides = []
       }

simpleOperandFormat :: [DT.OperandDescriptor] -> String
simpleOperandFormat descs = intercalate ", " $ map go descs
  where
    go :: DT.OperandDescriptor -> String
    go oper = DT.opName oper ++ " ${" ++ DT.opName oper ++ "}"

parseInstsFile :: FilePath -> ASL [ASL.Instruction]
parseInstsFile aslFile = do
  result <- liftIO $ do
    try (ASL.parseAslInstsFile aslFile) >>= \case
      Left (err :: IOError) -> return $ Left (show err)
      Right (Left errMsg) -> return $ Left (T.unpack errMsg)
      Right (Right result) -> return $ Right result
  case result of
    Left err -> do
      throwErrorHere $ ASLParserError err
    Right insts -> return insts

withContext :: (ASLContext -> ASLContext) -> ASL a -> ASL a
withContext f = MR.local (\env -> env { envContext = f (envContext env) })

withASLEncoding :: (ASL.Instruction, ASL.InstructionEncoding) -> ASL a -> ASL a
withASLEncoding aslEncoding = withContext (\ctx -> ctx { ctxASLEncoding = Just aslEncoding })


withXMLEncoding :: Encoding -> ASL a -> ASL a
withXMLEncoding xmlEncoding = withContext (\ctx -> ctx { ctxXMLEncoding = Just xmlEncoding })


getEncodings :: ASL.Instruction -> ASL [ASL.InstructionEncoding]
getEncodings instr = do
  targetInstrSet <- getTargetInstrSet
  return $ filter (\enc -> ASL.encInstrSet enc == targetInstrSet) (ASL.instEncodings instr)

deduplicateEncodings :: [ASL.Instruction] -> ASL [(ASL.Instruction, ASL.InstructionEncoding)]
deduplicateEncodings instrs = do
  pairs <- liftM concat $ forM instrs $ \instr -> do
    encodings <- getEncodings instr
    forM encodings $ \encoding -> do
      let ident = encodingIdentifier instr encoding
      return (ident, (instr, encoding))
  Map.elems <$> Map.fromListWithM assertEquivalentEncodings pairs


maskFromEncoding :: ASL.InstructionEncoding -> ASL (ARMBitMask BM.QuasiBit)
maskFromEncoding enc = case BM.fromList NR.knownNat $ maskToBits $ ASL.encOpcodeMask enc of
  Just mask -> do
    bitsects <- forM (ASL.encUnpredictable enc) $ \(pos, isset) -> do
      let qbit = BM.bitToQuasi True (BT.ExpectedBit isset)
      case BM.bitSectionFromListHiBit (fromIntegral pos) [qbit] armRegWidthRepr of
        Just bitsect -> return bitsect
        Nothing -> throwErrorHere $ InvalidBitPosition (fromIntegral pos)
    quasimask <- case BM.computePattern armRegWidthRepr bitsects of
      Left msg -> throwErrorHere $ FailedToDeriveMask msg
      Right result -> return result
    fmap BM.bitAsQuasi mask `mergeBitMasks` quasimask
  Nothing -> throwErrorHere $ InvalidBitMaskLength (length (ASL.encOpcodeMask enc))

assertEquivalentEncodings :: (ASL.Instruction, ASL.InstructionEncoding)
                          -> (ASL.Instruction, ASL.InstructionEncoding)
                          -> ASL (ASL.Instruction, ASL.InstructionEncoding)
assertEquivalentEncodings (instr1, enc1) (instr2, enc2) = withASLEncoding (instr1, enc1) $ do
  zipWithM (assertEqualBy id) (ASL.instPostDecode instr1) (ASL.instPostDecode instr2)
  zipWithM (assertEqualBy id) (ASL.instExecute instr1) (ASL.instExecute instr2)
  zipWithM (assertEqualBy id) (ASL.encDecode enc1) (ASL.encDecode enc2)
  assertEqualBy ASL.encName enc1 enc2
  assertEqualBy ASL.encInstrSet enc1 enc2
  assertEqualBy ASL.encFields enc1 enc2
  assertEqualBy ASL.encOpcodeMask enc1 enc2
  assertEqualBy ASL.encGuard enc1 enc2
  assertEqualBy ASL.encUnpredictable enc1 enc2

  return (instr1, enc1)

assertEqualBy :: Show b => Eq b => (a -> b) -> a -> a -> ASL ()
assertEqualBy f a a' = case (f a) == (f a') of
  False -> throwErrorHere $ InstructionIdentNameClash (f a) (f a')
  True -> return ()

encodingIdentifier :: ASL.Instruction -> ASL.InstructionEncoding -> String
encodingIdentifier instr enc =
  T.unpack (ASL.instName instr)
  ++ "/" ++ T.unpack (ASL.encName enc)
  ++ "/" ++ show (ASL.encInstrSet enc)
  ++ "/" ++ maskToString (ASL.encOpcodeMask enc)

maskToString :: ASL.Mask -> String
maskToString bits = map go bits
  where
    go :: ASL.MaskBit -> Char
    go b = case b of
      ASL.MaskBitEither -> 'x'
      ASL.MaskBitSet -> '1'
      ASL.MaskBitUnset -> '0'

maskToBitMask :: ASL.Mask -> (ARMBitMask BT.Bit)
maskToBitMask bits = case BM.fromList NR.knownNat (maskToBits bits) of
  Just result -> result
  Nothing -> error $ "Invalid mask length: " ++ show (length bits)

maskToBits :: ASL.Mask -> [BT.Bit]
maskToBits bits = map go bits
  where
    go :: ASL.MaskBit -> BT.Bit
    go b = case b of
      ASL.MaskBitEither -> BT.Any
      ASL.MaskBitSet -> BT.ExpectedBit True
      ASL.MaskBitUnset -> BT.ExpectedBit False
