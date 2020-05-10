{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeFamilies #-}
module Dismantle.Tablegen.Patterns (
  Pattern(..),
  showPattern,
  Bit(..),
  TrieM(..),
  TrieState(..),
  emptyTrieState,
  TrieError(..),
  PatternSet(..),
  emptyPatternSet,
  patternBytes,
  assertMapping,
  -- * Hash helper
  HashableUVec(..),
  -- * Index Helpers
  LinkedTableIndex(..),
  FlatTableIndex(..),
  nextTableIndex,
  defaultElementIndex
  ) where

import           Control.DeepSeq (NFData(..))
import qualified Control.Monad.Except as E
import qualified Control.Monad.Fail as CMF
import qualified Control.Monad.State.Strict as St
import           Data.Bifunctor ( bimap )
import           Data.Bits
import qualified Data.ByteString as BS
import           Data.Coerce ( coerce )
import qualified Data.HashMap.Strict as HM
import qualified Data.Hashable as DH
import           Data.Int ( Int32 )
import qualified Data.List as DL
import qualified Data.Map as M
import qualified Data.Vector.Generic as VG
import qualified Data.Vector.Generic.Mutable as VGM
import qualified Data.Vector.Unboxed as VU
import qualified Data.Vector.Unboxed.Mutable as VUM
import           Data.Word ( Word8 )
import           Numeric as N


-- | A bit with either an expected value ('ExpectedBit') or an
-- unconstrained value ('Any')
data Bit = ExpectedBit !Bool
         | Any
         deriving (Eq, Ord, Show)

instance NFData Bit where
  rnf _ = ()

-- | A wrapper around a sequence of 'Bit's
data Pattern = Pattern { requiredMask :: BS.ByteString
                       -- ^ The mask of bits that are considered for the
                       -- pattern to match
                       , trueMask :: BS.ByteString
                       -- ^ The bits that must be set (or not) in the positions
                       -- selected by the 'requiredMask'
                       , negativePairs :: [(BS.ByteString, BS.ByteString)]
                       -- ^ a list of "negative" masks, where the lefthand side of
                       -- each pair is the mask of bits that must /not/ match for
                       -- this pattern to apply, and the righthand side is the bits
                       -- that must be set (or not) in the positions selected by the
                       -- lefthand side in order to reject the pattern.
                       --
                       -- NOTE that this is only used to resolve conflicts
                       -- between multiple potential matches for a pattern.  The
                       -- negativePairs are ignored if the positive masks are
                       -- sufficient to uniquely identify an encoding.
                       --
                       -- This is fine unless the input data has errors.
                       }
               deriving (Eq, Ord, Show)

instance DH.Hashable Pattern where
  hashWithSalt slt p = slt `DH.hashWithSalt` requiredMask p
                           `DH.hashWithSalt` trueMask p
                           `DH.hashWithSalt` negativePairs p

showPattern :: Pattern -> String
showPattern Pattern{..} =
  let hexStr v = (<>) "0x" $ N.showHex v ""
      hexBS b = hexStr <$> BS.unpack b
  in
  "Pattern { requiredMask = " ++ show (hexBS requiredMask) ++
  ", trueMask = " ++ show (hexBS trueMask) ++
  (if null negativePairs then ""
    else let showNegPair p = bimap hexBS hexBS p in
      ", negativePairs = " ++ (show $ map showNegPair negativePairs)
  ) ++
  " }"

-- | Return the number of bytes occupied by a 'Pattern'
patternBytes :: Pattern -> Int
patternBytes = BS.length . requiredMask

data TrieError = OverlappingBitPattern [(Pattern, [String], Int)]
               | OverlappingBitPatternAt Int [Word8] [(Pattern, [String], Int)]
               -- ^ Byte index, byte, patterns
               | InvalidPatternLength Pattern
               | MonadFailErr String
  deriving (Eq)

instance Show TrieError where
  show err = case err of
    OverlappingBitPattern patList -> "OverlappingBitPattern " ++ showPatList patList
    OverlappingBitPatternAt ix bytes patList ->
      "OverlappingBitPatternAt index:" ++ show ix ++
      " bytesSoFar: " ++ show bytes ++
      " matching patterns: " ++ showPatList patList
    InvalidPatternLength p -> "InvalidPatternLength " ++ showPattern p
    MonadFailErr str -> "MonadFailErr " ++ show str
    where showPat (p, mnemonics, numBytes) = "(" ++ showPattern p ++ ", " ++ show mnemonics ++ ", " ++ show numBytes ++ ")"
          showPatList pats = "[" ++ DL.intercalate "," (showPat <$> pats) ++ "]"

newtype PatternSet = PatternSet { patternSetBits :: Integer }
  deriving (Eq, Ord, Show, DH.Hashable, Bits)

emptyPatternSet :: PatternSet
emptyPatternSet = PatternSet 0

-- | The state of the 'TrieM' monad
data TrieState e = TrieState { tsPatterns :: !(M.Map Pattern (LinkedTableIndex, e))
                             -- ^ The 'Int' is the index of the element into the
                             -- 'btPayloads' vector; these are allocated
                             -- up-front so that we don't need to maintain a
                             -- backwards mapping from e -> Int (which would put
                             -- an unfortunate 'Ord' constraint on e).
                             , tsPatternMnemonics :: !(M.Map Pattern String)
                             -- ^ A mapping of patterns to their mnemonics
                             , tsPatternSets :: !(HM.HashMap Pattern PatternSet)
                             -- ^ Record the singleton 'PatternSet' for each
                             -- pattern; when constructing the key for
                             -- 'tsCache', all of these will be looked up and
                             -- ORed together to construct the actual set.
                             , tsCache :: !(HM.HashMap (Int, PatternSet) LinkedTableIndex)
                             -- ^ A map of trie levels to tries to allow for
                             -- sharing of common sub-trees.  The 'Int' is an
                             -- index into 'tsTables'
                             , tsTables :: !(M.Map LinkedTableIndex (VU.Vector LinkedTableIndex))
                             -- ^ The actual tables, which point to either other
                             -- tables or terminal elements in the 'tsPatterns'
                             -- table.
                             , tsTableCache :: !(HM.HashMap HashableUVec LinkedTableIndex)
                             , tsEltIdSrc :: LinkedTableIndex
                             -- ^ The next element ID to use
                             , tsTblIdSrc :: LinkedTableIndex
                             -- ^ The next table ID to use
                             }

emptyTrieState :: TrieState e
emptyTrieState =
  TrieState { tsPatterns = M.empty
            , tsCache = HM.empty
            , tsPatternMnemonics = M.empty
            , tsPatternSets = HM.empty
            , tsTables = M.empty
            , tsTableCache = HM.empty
            , tsEltIdSrc = firstElementIndex
            , tsTblIdSrc = firstTableIndex
            }

newtype TrieM e a = TrieM { unM :: St.StateT (TrieState e) (E.Except TrieError) a }
  deriving (Functor,
            Applicative,
            Monad,
            E.MonadError TrieError,
            St.MonadState (TrieState e))

instance CMF.MonadFail (TrieM e) where
    fail msg = E.throwError $ MonadFailErr msg

-- | Assert a mapping from a bit pattern to a value.
--
-- The bit pattern must have a length that is a multiple of 8.  This
-- function can error out with a pure error ('TrieError') if an
-- overlapping bit pattern is asserted.
assertMapping :: String -> BS.ByteString -> BS.ByteString -> [(BS.ByteString, BS.ByteString)] -> a -> TrieM a ()
assertMapping mnemonic patReq patTrue patNegPairs val
  | BS.length patReq /= BS.length patTrue || BS.null patReq =
    E.throwError (InvalidPatternLength pat)
  | otherwise = do
      pats <- St.gets tsPatterns
      case M.lookup pat pats of
        Just _ -> do
            -- Get the mnemonic already mapped to this pattern
            mnemonics <- St.gets tsPatternMnemonics
            case M.lookup pat mnemonics of
              Just oldMnemonic -> E.throwError (OverlappingBitPattern [(pat, [mnemonic, oldMnemonic], patternBytes pat)])
              Nothing -> E.throwError (OverlappingBitPattern [(pat, [mnemonic], patternBytes pat)])
        Nothing -> do
          eid <- St.gets tsEltIdSrc
          patIdNum <- St.gets (M.size . tsPatterns)
          let patSet = PatternSet { patternSetBits = bit patIdNum }
          St.modify' $ \s -> s { tsPatterns = M.insert pat (eid, val) (tsPatterns s)
                               , tsPatternSets = HM.insert pat patSet (tsPatternSets s)
                               , tsPatternMnemonics = M.insert pat mnemonic (tsPatternMnemonics s)
                               , tsEltIdSrc = nextElementIndex eid
                               }
  where
    pat = Pattern patReq patTrue patNegPairs


-- Internal helper types

newtype instance VU.Vector LinkedTableIndex = V_FTI (VU.Vector Int32)
newtype instance VUM.MVector s LinkedTableIndex = MV_FTI (VUM.MVector s Int32)

instance VGM.MVector VUM.MVector LinkedTableIndex where
  basicLength (MV_FTI mv) = VGM.basicLength mv
  basicUnsafeSlice i l (MV_FTI mv) = MV_FTI (VGM.basicUnsafeSlice i l mv)
  basicOverlaps (MV_FTI mv) (MV_FTI mv') = VGM.basicOverlaps mv mv'
  basicUnsafeNew l = MV_FTI <$> VGM.basicUnsafeNew l
  basicInitialize (MV_FTI mv) = VGM.basicInitialize mv
  basicUnsafeReplicate i x = MV_FTI <$> VGM.basicUnsafeReplicate i (coerce x)
  basicUnsafeRead (MV_FTI mv) i = coerce <$> VGM.basicUnsafeRead mv i
  basicUnsafeWrite (MV_FTI mv) i x = VGM.basicUnsafeWrite mv i (coerce x)
  basicClear (MV_FTI mv) = VGM.basicClear mv
  basicSet (MV_FTI mv) x = VGM.basicSet mv (coerce x)
  basicUnsafeCopy (MV_FTI mv) (MV_FTI mv') = VGM.basicUnsafeCopy mv mv'
  basicUnsafeMove (MV_FTI mv) (MV_FTI mv') = VGM.basicUnsafeMove mv mv'
  basicUnsafeGrow (MV_FTI mv) n = MV_FTI <$> VGM.basicUnsafeGrow mv n

instance VG.Vector VU.Vector LinkedTableIndex where
  basicUnsafeFreeze (MV_FTI mv) = V_FTI <$> VG.basicUnsafeFreeze mv
  basicUnsafeThaw (V_FTI v) = MV_FTI <$> VG.basicUnsafeThaw v
  basicLength (V_FTI v) = VG.basicLength v
  basicUnsafeSlice i l (V_FTI v) = V_FTI (VG.basicUnsafeSlice i l v)
  basicUnsafeIndexM (V_FTI v) i = coerce <$> VG.basicUnsafeIndexM v i
  basicUnsafeCopy (MV_FTI mv) (V_FTI v) = VG.basicUnsafeCopy mv v
  elemseq (V_FTI v) x y = VG.elemseq v (coerce x) y

-- | This type represents the payload of the initial linked parse tables (in the
-- state of the monad).
--
-- In this representation, negative values name a payload in 'tsPatterns', while
-- non-negative values are table identifiers in 'tsTables'.
newtype LinkedTableIndex = LTI Int32
  deriving (Eq, Ord, Show, VU.Unbox, DH.Hashable)

newtype HashableUVec = HashableUVec (VU.Vector LinkedTableIndex)
  deriving (Eq)

instance DH.Hashable HashableUVec where
  hashWithSalt slt (HashableUVec v) = DH.hashWithSalt slt (VU.toList v)


-- | This type represents payloads in the 'ByteTrie' type.
--
-- Again, negative values are indices into 'btPayloads'.  Other values are
-- indices into 'btParseTables'.
newtype FlatTableIndex = FTI { unFTI :: Int32 }
  deriving (Eq, Ord, Show)


defaultElementIndex :: LinkedTableIndex
defaultElementIndex = LTI (-1)

-- | The element indexes start at -2 since the default is reserved for -1
firstElementIndex :: LinkedTableIndex
firstElementIndex = LTI (-2)

firstTableIndex :: LinkedTableIndex
firstTableIndex = LTI 0

nextTableIndex :: LinkedTableIndex -> LinkedTableIndex
nextTableIndex (LTI i) = LTI (i + 1)

nextElementIndex :: LinkedTableIndex -> LinkedTableIndex
nextElementIndex (LTI i) = LTI (i - 1)
