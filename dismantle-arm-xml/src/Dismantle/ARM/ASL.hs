{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE ViewPatterns #-}
{-|
Module           : Dismantle.ARM.ASL
Copyright        : (c) Galois, Inc 2019-2020
Maintainer       : Daniel Matichuk <dmatichuk@galois.com>

This module associates each 'XML.Encoding' from "Dismantle.ARM.XML"
with an instruction/encoding pair from the ARM specification
language (ASL). A single ASL instruction encoding may correspond
to multiple 'Encoding's, where an 'Encoding' may specify additional
constraints on the ASL fields that are not present in its decode header.


The top-level interface is provided by 'loadASL', which associates each
'XML.Encoding' (instruction encoding from the XML specification) with an
'Encoding' (instruction encoding from the ASL specification).

-}
module Dismantle.ARM.ASL
  ( Encoding(..)
  , FieldConstraint(..)
  , loadASL
  , encodingIdentifier
  )
  where

import           GHC.Stack
import           GHC.TypeLits
import           Prelude hiding ( fail )

import           Control.Exception ( try )
import qualified Control.Exception as E
import           Control.Lens ( (&), (%~) )
import qualified Control.Lens as L
import           Control.Monad ( foldM, forM, forM_, zipWithM, void, unless, liftM )
import           Control.Monad.Fail
import           Control.Monad.Trans ( lift, liftIO, MonadIO )
import           Control.Monad.Trans.RWS.Strict ( RWST )
import qualified Control.Monad.Trans.RWS.Strict as RWS
import qualified Control.Monad.State as MS
import qualified Control.Monad.Reader as MR
import qualified Control.Monad.Writer as MW
import           Control.Monad.Except ( throwError, ExceptT, MonadError )
import qualified Control.Monad.Except as ME

import           Data.Maybe ( catMaybes, fromMaybe )
import qualified Data.List.NonEmpty as NE
import           Data.Map ( Map )
import qualified Data.Map as Map
import qualified Data.Parameterized.Classes as PC
import qualified Data.Parameterized.Map as MapF
-- FIXME: move or use library version of this
import qualified Dismantle.ARM.XML as Map ( fromListWithM )
import qualified Data.Text as T

import qualified Data.Parameterized.NatRepr as NR

import           Text.PrettyPrint.HughesPJClass ( (<+>), ($$) )
import qualified Text.PrettyPrint.HughesPJClass as PP

import qualified Language.ASL.Parser as ASL
import qualified Language.ASL.Syntax as ASL

import qualified Dismantle.ARM.XML as XML
import qualified Dismantle.Tablegen as DT

import qualified Dismantle.Tablegen.Patterns as BT
import qualified Data.BitMask as BM

data ASLEnv = ASLEnv { envLogFn :: String -> IO ()
                     , envArchName :: String
                     , envContext :: ASLContext
                     -- ^ the current context for error reporting
                     }

data ASLContext = ASLContext { ctxASLEncoding :: Maybe (ASL.Instruction, ASL.InstructionEncoding)
                             , ctxXMLEncoding :: Maybe XML.Encoding
                             , ctxStmt :: Maybe ASL.Stmt
                             , ctxFileName :: String
                             , ctxSourceLoc :: Maybe SrcLoc
                             }

data SizedTrie n =
  SizedTrie { _stRepr :: NR.NatRepr n
            , _stTrie :: BM.MaskTrie BM.QuasiBit n [(String, BM.BitMask n BM.QuasiBit)]
            }

$(L.makeLensesFor [("_stTrie", "stTrie")] ''SizedTrie)

data ASLState = ASLState { _stMaskTrie :: MapF.MapF NR.NatRepr SizedTrie
                         , _stEncodingMap :: Map String (ASL.Instruction, ASL.InstructionEncoding)
                         }

$(L.makeLenses ''ASLState)

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
  | forall n . InvalidBitPosition (NR.NatRepr n) Int
  | InvalidASLField ASL.InstructionField
  | forall n . InvalidXMLField (NR.NatRepr n) (XML.Field n)
  | FailedToDeriveMask String
  | MissingEncodingForIdentifier String
  | forall n . UnexpectedMatchingEntries (NR.NatRepr n) (BM.BitMask n BM.QuasiBit) [(String, BM.BitMask n BM.QuasiBit)]
  | forall n . UnexpectedSplitFields (NR.NatRepr n) (BM.BitMask n FixedFieldBit)
  | ASLParserError String
  | UnexpectedFieldWidth String Int Int
  | MissingConstraintForField XML.Encoding String [String]
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
  -- The only way for HasCallStack to be empty is for a user to manually
  -- construct one as an implicit argument, which is unlikely.
  src <- case getCallStack callStack of
           (_, src): _ -> return src
           []          -> error "throwErrorHere: Unexpected empty call stack"
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
          Just enc -> PP.text "for XML encoding:" <+> PP.text (XML.encName enc)
          _ -> PP.empty
    $$ case ctxStmt ctx of
          Just stmt -> PP.text "at statement:" $$ (PP.text $ show stmt) $$ errPretty
          _ -> errPretty

instance Show OuterASLException where
  show e = PP.render (PP.pPrint e)

instance PP.Pretty ASLException where
  pPrint e = case e of
    UnexpectedMatchingEntries _nr mask [] ->
      PP.text "Missing entry for:"
      $$ prettyMask mask
    UnexpectedMatchingEntries _nr mask matches ->
      let content = PP.nest 1 (PP.space $$ PP.text "Entries:" $$ PP.vcat (map prettyMatch matches)) in
      PP.text "Multiple entries found for:"
      $$ prettyMask mask
      $$ content
      where
        prettyMatch :: forall n . (String, BM.BitMask n BM.QuasiBit) -> PP.Doc
        prettyMatch (nm, mask') = PP.text nm PP.<> PP.text ":" $$ PP.nest 1 (prettyMask mask')
    UnexpectedFieldWidth fname expected actual ->
      PP.text "Unexpected width for field: " <+> PP.text fname
      $$ PP.text "Expected:" <+> PP.int expected <+> PP.text "Got:" <+> PP.int actual
    MissingConstraintForField encoding _fieldname _foundfields ->
      PP.text "MissingConstraintForField"
      $$ PP.pPrint encoding
    MultipleExceptions es -> PP.vcat $ map PP.pPrint es
    _ ->
      PP.text (show e)

prettyMask :: BM.BitMask n BM.QuasiBit -> PP.Doc
prettyMask mask = BM.prettySegmentedMask id mask

logMsg :: String -> ASL ()
logMsg msg = do
  logFn <- MR.asks envLogFn
  liftIO $ logFn msg

_warnError :: ASLException -> ASL ()
_warnError e = do
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
    n16 = NR.knownNat @16
    n32 = NR.knownNat @32
    t0 = MapF.insert n16 (SizedTrie n16 BM.emptyMaskTrie) $
         MapF.insert n32 (SizedTrie n32 BM.emptyMaskTrie) MapF.empty
    initState = ASLState { _stMaskTrie = t0
                         , _stEncodingMap = Map.empty
                         }

  in ME.runExceptT $ RWS.runRWST m initEnv initState

-- | Representation of an instruction encoding from the perspective of
-- ASL: given as a restriction of the fields described in an ASL
-- instruction/encoding pair in order to match a single 'XML.Encoding'
data Encoding = Encoding { encName :: String
                         -- ^ the name of the 'XML.Encoding' that this encoding corresponds to
                         , encASLIdent :: String
                         -- ^ a unique identifier derived from the ASL instruction/encoding pair
                         -- (via 'encodingIdentifier')
                         , encRealOperands :: [(String, Integer)]
                         -- ^ the names and widths of the operands for this encoding
                         , encPseudoOperands :: [(String, Integer)]
                         -- ^ the names and widths of the pseudo-operands for this encoding
                         -- (taken by the semantics but unused)
                         , encConstraint :: [FieldConstraint]
                         -- ^ additional constraints for the operands that further restrict
                         -- the ASL instruction/encoding to a specific 'XML.Encoding'
                         , encNegConstraints :: [[FieldConstraint]]
                         -- ^ additional negative constraints for the operands
                         }

-- | Annotate the 'Encoding's from the XML specification with an identifier for its correspoding
-- ASL encoding. Identifiers are derived with 'encodingIdentifier' and are checked for uniqueness.
-- Note that some encodings are duplicated in the ASL, so we check that any identifer clashes
-- point to syntactically-equivalent ASL instruction specifications.
--
-- The masks for ASL encodings are more permissive than the XML encodings, thus each ASL encoding
-- may correspond to multiple XML encodings. Each XML encoding is therefore annotated with any additional
-- constraints (positive and negative) on the named fields of its corresponding ASL encoding.
-- The list of pairs here represents the bijective mapping: XML encoding <-> ASL encoding + constraints.
loadASL :: String -> FilePath -> [XML.Encoding] -> (String -> IO ()) -> IO [(XML.Encoding, Encoding)]
loadASL archName aslFile xmlEncodings logFn = do
  logFn "Dismantle.ASL.ASL: loadASL"
  result <- runASL archName aslFile logFn $ do
    instrs <- parseInstsFile aslFile >>= deduplicateEncodings
    forM_ instrs $ \(instr, enc) -> addEncodingMask instr enc
    forM xmlEncodings $ \xmlEncoding -> withXMLEncoding xmlEncoding $ do
      withEncoding xmlEncoding $ \nr aslIdent aslMask -> do
        (instr, aslRawEncoding) <- lookupASLEncoding aslIdent

        withASLEncoding (instr, aslRawEncoding) $ do
          aslEncoding <- correlateEncodings aslIdent nr aslMask aslRawEncoding xmlEncoding
          logMsg $ "Correlated XML encoding: " ++ (XML.encName xmlEncoding) ++ " with ASL " ++ aslIdent
          logMsg $ "with constraints:"
          logMsg $ PP.render (prettyFieldConstraints (encConstraint aslEncoding) (encNegConstraints aslEncoding))
          return (xmlEncoding, aslEncoding)

  case result of
    Left err -> do
      logFn (show err)
      E.throw err
    Right (a, _st, _out) -> return a

getInstrFilter :: ASL (ASL.InstructionSet -> Bool)
getInstrFilter = do
  MR.asks envArchName >>= \case
    "A32" -> return $ \iset -> ASL.A32 == iset
    "T32" -> return $ \iset -> ASL.T32 == iset || ASL.T16 == iset
    arch -> throwErrorHere $ UnsupportedArch arch

-- | A field bit is an optionally-tagged bit which corresponds to the named field that it belongs to.
newtype FieldBit = FieldBit (BM.QuasiBit, BM.WithBottom (BM.AsBit String))
  deriving (Eq, Ord, Show, BM.SemiMaskBit, BM.HasBottomMaskBit)

instance BM.ShowableBit FieldBit where
  showBit fieldBit = case nameOfFieldBit fieldBit of
    Just name -> BM.showBit (bitOfFieldBit fieldBit) ++ "'" ++ name ++ "'"
    Nothing -> BM.showBit (bitOfFieldBit fieldBit)

-- | A fixed-field bit is either a tagged bit corresponding to a field, or a restricted version
-- of the bit which only matches on equivalence. i.e. a fixed field bit only allows additional
-- constraints to be established for named fields.
newtype FixedFieldBit = FixedFieldBit (Either (BM.QuasiBit, BM.AsBit String) (BM.AsBit BM.QuasiBit))
  deriving (Eq, Ord, Show, BM.SemiMaskBit)

instance BM.ShowableBit FixedFieldBit where
  showBit fieldBit = case nameOfFixedFieldBit fieldBit of
    Just name -> showFixed ++ "'" ++ name ++ "'"
    Nothing -> showFixed
    where
      showFixed :: String
      showFixed = case fieldBit of
        FixedFieldBit (Left (bit, _)) -> BM.showBit bit
        FixedFieldBit (Right (BM.AsBit bit)) -> "|" ++ BM.showBit bit ++ "|"

-- | Strictly separate named and unnamed fields.
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

computeMask :: (1 <= n) => NR.NatRepr n -> [BM.BitSection n FieldBit] -> ASL (BM.BitMask n FieldBit)
computeMask nr fieldSects = case BM.computePattern nr fieldSects of
  Left msg -> throwErrorHere $ IncompatibleFieldBits msg
  Right merged -> return $ merged

mergeBitMasks :: BM.SemiMaskBit bit
              => BM.ShowableBit bit
              => BM.BitMask n bit
              -> BM.BitMask n bit
              -> ASL (BM.BitMask n bit)
mergeBitMasks mask1 mask2 = case BM.mergeBitMasksErr mask1 mask2 of
  Left msg -> throwErrorHere $ IncompatibleFieldBits msg
  Right merged -> return merged

-- | A constraint on an ASL field which specifies its valid values for interpreting
-- an ASL instruction/encoding pair as a particular 'XML.Encoding'
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
splitFieldMask :: NR.NatRepr n -> BM.BitMask n FixedFieldBit -> ASL (Map String FieldConstraint)
splitFieldMask nr mask = do
  l <- liftM catMaybes $ forM (NE.groupBy equivFields (BM.toList mask)) $ \(fieldBit NE.:| rst) ->
         case nameOfFixedFieldBit fieldBit of
           Just fieldName -> return $ Just (fieldName, fmap bitOfFixedFieldBit $ BM.someBitMaskFromCons fieldBit rst)
           Nothing -> return Nothing
  let noMerge _ _ = throwErrorHere $ UnexpectedSplitFields nr mask
  constraintMap <- Map.fromListWithM noMerge l
  return $ Map.mapWithKey FieldConstraint constraintMap

-- | Calculate the real beginning index of a field
--
-- Fields are indexed starting from 31, even if the instruction is only 16 bits.
-- This function corrects the starting index such that it is valid for both 16
-- and 32 bit encodings (i.e., mapping 31 to 15 for T16 encodings).
fieldBeginIndex :: NR.NatRepr n -> Integer -> Integer
fieldBeginIndex nr begin0
  | Just PC.Refl <- PC.testEquality nr (NR.knownNat @16) = begin0 - 16
  | otherwise = begin0

aslFixedFieldMask :: (1 <= n) => ASL.InstructionEncoding -> NR.NatRepr n -> BM.BitMask n BM.QuasiBit -> ASL (BM.BitMask n FixedFieldBit)
aslFixedFieldMask aslEncoding nr aslMask = do
  aslBitsects <- forM (ASL.encFields aslEncoding) $ \field -> do
    -- NOTE: The field indexes we get from the ASL parser are always indexed
    -- from bit 31 (even if the instruction is 16 bits).  We have to correct the
    -- index coming from the ASL parser if we have a 16 bit instruction using
    -- fieldBeginIndex.
    let
      fieldName = T.unpack $ ASL.instFieldName field
      fieldLobit = fromIntegral $ fieldBeginIndex nr (ASL.instFieldBegin field)
      fieldWidth = fromIntegral $ ASL.instFieldOffset field
      fieldHibit = fieldLobit + fieldWidth - 1
      taggedBv = take fieldWidth $ repeat (mkFieldBit (Just fieldName) BM.bottomBit)
    case BM.bitSectionFromListHiBit fieldHibit taggedBv nr of
      Nothing -> throwErrorHere $ InvalidASLField field
      Just bitsect -> return $ bitsect

  aslFieldMask <- computeMask nr aslBitsects
  let aslTotalMask = fmap (mkFieldBit Nothing) aslMask
  fmap fixFieldBit <$> mergeBitMasks aslFieldMask aslTotalMask

withXmlFixedFieldMask :: XML.Encoding
                      -> (forall n . NR.NatRepr n -> BM.BitMask n FixedFieldBit -> [BM.BitMask n FixedFieldBit] -> ASL a)
                      -> ASL a
withXmlFixedFieldMask XML.Encoding { XML.encMask = emask
                                   , XML.encFields = fields
                                   , XML.encSize = size
                                   } k = do
  (xmlMasks, xmlNegMasks) <- liftM unzip $
    forM (filter XML.fieldUseName $ Map.elems fields) $ \field -> do
      let
        fieldName = XML.xmlFieldNameToASL $ XML.fieldName field
        constraint = fmap (fmap $ mkFieldBit (Just fieldName)) $ XML.fieldConstraint field
      case BM.deriveMasks size constraint of
        Left msg -> throwErrorHere $ IncompatibleFieldBits msg
        Right (mask, negmasks) -> do
          -- merge the tags of negative masks back into the mask to ensure we have all the field tags
          mask' <- foldM mergeBitMasks mask (map (fmap dropBitOfFieldBit) negmasks)
          return (mask', negmasks)
  let bottom = BM.bottomBitMask size
  xmlFieldMask <- foldM mergeBitMasks bottom xmlMasks
  xmlTotalMask <- return $ fmap (mkFieldBit Nothing) emask
  xmlFixedMask <- fmap fixFieldBit <$> mergeBitMasks xmlFieldMask xmlTotalMask
  k size xmlFixedMask (map (fmap fixFieldBit) (concat xmlNegMasks))

-- | Correlate an ASL encoding with an XML encoding by matching up their fields, and potentially
-- determining concrete instantiations for them.
-- Works by creating "tagged" versions of the constraint bitmask for each
-- representation of the encoding (i.e. where each bit knows which field it belongs to).
-- The resulting bitmasks are merged and the field constraint mappings are recovered.
--
-- This serves as a final check that we have found the correct encoding, since
-- every bit is checked that its tagged field name is the same in both masks.
correlateEncodings :: (1 <= n)
                   => String
                   -> NR.NatRepr n
                   -> BM.BitMask n BM.QuasiBit
                   -> ASL.InstructionEncoding
                   -> XML.Encoding
                   -> ASL Encoding
correlateEncodings aslIdent maskRepr aslMask aslRawEncoding xmlEncoding = do
  withXmlFixedFieldMask xmlEncoding $ \nr xmlFixedMask xmlNegMasks -> do
    case PC.testEquality maskRepr nr of
      Nothing -> error ("Mismatched instruction width for " ++ aslIdent)
      Just PC.Refl -> do
        aslFixedMask <- aslFixedFieldMask aslRawEncoding nr aslMask
        fixedMask <- xmlFixedMask `mergeBitMasks` aslFixedMask
        constraintMap <- splitFieldMask nr fixedMask
        negConstraintMap <-
          liftM (Map.unionsWith (++) . map (fmap (\x -> [x]))) $ mapM (splitFieldMask nr) xmlNegMasks

        (posConstraints, negConstraints) <- liftM unzip $ forM (ASL.encFields aslRawEncoding) $ \field -> do
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
        let (realops, pseudoops) = XML.getOperandDescriptors xmlEncoding
        return $ Encoding { encName = XML.encName xmlEncoding
                          , encASLIdent = aslIdent
                          , encRealOperands = map getSimpleOp realops
                          , encPseudoOperands = map getSimpleOp pseudoops
                          , encConstraint = concat posConstraints
                          , encNegConstraints = filter (not . null) negConstraints
                          }

getSimpleOp :: DT.OperandDescriptor -> (String, Integer)
getSimpleOp opd =
  let
    totalWidth = sum (map (\(_, _, w) -> w) (DT.opChunks opd))
  in (DT.opName opd, fromIntegral totalWidth)

lookupMasks :: forall n . NR.NatRepr n -> BM.BitMask n BM.QuasiBit -> ASL (String, BM.BitMask n BM.QuasiBit)
lookupMasks nr mask = do
  masktrees <- MS.gets _stMaskTrie
  masktree <- case MapF.lookup nr masktrees of
                Just (SizedTrie _ masktree) -> return masktree
                Nothing -> error $ "lookupMasks: Could not find mask trie for: " ++ show nr
  case BM.lookupMaskTrie mask masktree of
    [result] -> return result
    results | [result] <- filter exactMatch results -> return result
    x -> throwErrorHere $ UnexpectedMatchingEntries nr mask x
  where
    exactMatch :: (String, BM.BitMask n BM.QuasiBit) -> Bool
    exactMatch (_, mask') = mask == mask'

-- | Lookup the ASL identifier for a given XML 'Encoding' using the 'MaskTrie' in the state.
-- Attempts a lookup with both the general instruction mask as well as the mask from this
-- specific encoding. This ambiguity is resolved later by resolving the ASL bits against
-- the constraints from the XML in 'correlateEncodings'.
withEncoding :: XML.Encoding -> (forall n . (1 <= n) => NR.NatRepr n -> String -> BM.BitMask n BM.QuasiBit -> ASL a) -> ASL a
withEncoding XML.Encoding { XML.encIConstraints = iconstraints
                            , XML.encMask = mask
                            , XML.encSize = size
                            } k = do
  (imask, _inegmasks) <- case BM.deriveMasks size iconstraints of
    Left msg -> throwErrorHere $ FailedToDeriveMask msg
    Right (mask1, negmasks) -> return $ (mask1, negmasks)
  (str, bm) <- lookupMasks size imask
    `ME.catchError`
     (\e -> if imask == mask then throwError e else
         lookupMasks size mask
           `ME.catchError`
           (\e' -> throwError $ MultipleExceptions [e, e']))
  k size str bm

lookupASLEncoding :: String -> ASL (ASL.Instruction, ASL.InstructionEncoding)
lookupASLEncoding ident = do
  encMap <- MS.gets _stEncodingMap
  case Map.lookup ident encMap of
    Just result -> return result
    Nothing -> throwErrorHere $ MissingEncodingForIdentifier ident

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


withXMLEncoding :: XML.Encoding -> ASL a -> ASL a
withXMLEncoding xmlEncoding = withContext (\ctx -> ctx { ctxXMLEncoding = Just xmlEncoding })


getEncodings :: ASL.Instruction -> ASL [ASL.InstructionEncoding]
getEncodings instr = do
  instrFilter <- getInstrFilter
  return $ filter (instrFilter . ASL.encInstrSet) (ASL.instEncodings instr)

deduplicateEncodings :: [ASL.Instruction] -> ASL [(ASL.Instruction, ASL.InstructionEncoding)]
deduplicateEncodings instrs = do
  pairs <- liftM concat $ forM instrs $ \instr -> do
    encodings <- getEncodings instr
    forM encodings $ \encoding -> do
      let ident = encodingIdentifier instr encoding
      return (ident, (instr, encoding))
  Map.elems <$> Map.fromListWithM assertEquivalentEncodings pairs


maskFromEncoding :: (1 <= n) => NR.NatRepr n -> ASL.InstructionEncoding -> ASL (BM.BitMask n BM.QuasiBit)
maskFromEncoding nr enc = case BM.fromList nr $ maskToBits $ ASL.encOpcodeMask enc of
  Just mask -> do
    -- NOTE: We are doing an index correction here. The ASL parser returns
    -- unpredictable bits in the encoding indexed from 31 for both 16- and 32-
    -- bit instructions. This correction fixes it based on the expected
    -- instruction width.
    bitsects <- forM (ASL.encUnpredictable enc) $ \(fieldBeginIndex nr -> pos, isset) -> do
      let qbit = BM.bitToQuasi True (BT.ExpectedBit isset)
      case BM.bitSectionFromListHiBit (fromIntegral pos) [qbit] nr of
        Just bitsect -> return bitsect
        Nothing -> throwErrorHere $ InvalidBitPosition nr (fromIntegral pos)
    quasimask <- case BM.computePattern nr bitsects of
      Left msg -> throwErrorHere $ FailedToDeriveMask msg
      Right result -> return result
    fmap BM.bitAsQuasi mask `mergeBitMasks` quasimask
  Nothing -> throwErrorHere $ InvalidBitMaskLength (length (ASL.encOpcodeMask enc))

addEncodingMask :: ASL.Instruction -> ASL.InstructionEncoding -> ASL ()
addEncodingMask instr enc = do
  MS.modify' $ \st -> st & stEncodingMap %~ Map.insert ident (instr, enc)
  case ASL.encInstrSet enc of
    ASL.T16 -> do
      let (hiBits, _loBits) = splitAt 16 (ASL.encOpcodeMask enc)
      maskFromEncoding n16 (enc { ASL.encOpcodeMask = hiBits }) >>= doAdd n16
    _ -> maskFromEncoding n32 enc >>= doAdd n32
  where
    n16 = NR.knownNat @16
    n32 = NR.knownNat @32
    ident = encodingIdentifier instr enc

    doAdd :: forall n . NR.NatRepr n -> BM.BitMask n BM.QuasiBit -> ASL ()
    doAdd nr mask = do
      MS.modify' $ \st -> st & stMaskTrie . PC.ixF nr . stTrie %~ BM.addToMaskTrie mask [(ident, mask)]

assertEquivalentEncodings :: (ASL.Instruction, ASL.InstructionEncoding)
                          -> (ASL.Instruction, ASL.InstructionEncoding)
                          -> ASL (ASL.Instruction, ASL.InstructionEncoding)
assertEquivalentEncodings (instr1, enc1) (instr2, enc2) = withASLEncoding (instr1, enc1) $ do
  void $ zipWithM (assertEqualBy id) (ASL.instPostDecode instr1) (ASL.instPostDecode instr2)
  void $ zipWithM (assertEqualBy id) (ASL.instExecute instr1) (ASL.instExecute instr2)
  void $ zipWithM (assertEqualBy id) (ASL.encDecode enc1) (ASL.encDecode enc2)
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

-- | Derive a unique identifier for an instruction/encoding pair based on their
-- names as well as the mask of the encoding. Some instruction/encoding pairs are
-- duplicated in the ASL specification, however we validate that all key clashes
-- point to syntactically identical specifications when building the disassembler.
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

maskToBits :: ASL.Mask -> [BT.Bit]
maskToBits bits = map go bits
  where
    go :: ASL.MaskBit -> BT.Bit
    go b = case b of
      ASL.MaskBitEither -> BT.Any
      ASL.MaskBitSet -> BT.ExpectedBit True
      ASL.MaskBitUnset -> BT.ExpectedBit False
