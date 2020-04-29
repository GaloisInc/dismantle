module Dismantle.Tablegen.BitTrie (
  bitTrie,
  lookupByte
  ) where

import qualified Control.Monad.Except as ME
import qualified Control.Monad.State.Strict as St
import           Data.Bits ( (.|.), (.&.), popCount, testBit )
import qualified Data.Bits as DB
import qualified Data.ByteString as BS
import qualified Data.Foldable as F
import qualified Data.HashMap.Strict as HM
import qualified Data.List as L
import qualified Data.Map as M
import           Data.Maybe ( catMaybes )
import qualified Data.Traversable as T
import qualified Data.Vector as V
import qualified Data.Vector.Storable as SV
import qualified Data.Vector.Unboxed as VU
import           Data.Word ( Word8 )

import qualified Dismantle.Tablegen.LinearizedTrie as LT
import qualified Dismantle.Tablegen.Patterns as DTP

lookupByte :: LT.LinearizedTrie a -> Word8 -> Either (LT.LinearizedTrie a) a
lookupByte lt byte = lookupByteRec lt byte 7

lookupByteRec :: LT.LinearizedTrie a
              -> Word8
              -> Int
              -> Either (LT.LinearizedTrie a) a
lookupByteRec lt byte n
  -- We found a terminal node in the trie, return the payload
  | tableVal < 0 = Right (LT.ltPayloads lt `V.unsafeIndex` fromIntegral (negate tableVal))
  -- We reached the end of the byte and the next node is another trie
  | n == 0 = Left $ lt { LT.ltStartIndex = fromIntegral tableVal }
  -- Otherwise, just keep traversing bits
  | otherwise =
    let lt' = lt { LT.ltStartIndex = fromIntegral tableVal }
    in lookupByteRec lt' byte (n - 1)
  where
    tableVal = LT.ltParseTables lt `SV.unsafeIndex` (bitValue byte n + LT.ltStartIndex lt)

-- | Given
--
-- > bitValue byte idx
--
-- Return the bit value at the given index (@idx@) of the @byte@.  The resulting
-- 'Int' has the value 0 or 1.
bitValue :: Word8 -> Int -> Int
bitValue byte idx =
  if DB.testBit byte idx then 1 else 0


bitTrie :: e
        -> [(String, BS.ByteString, BS.ByteString, [(BS.ByteString, BS.ByteString)], e)]
        -> Either DTP.TrieError (LT.LinearizedTrie e)
bitTrie defElt mappings =
  mkTrie defElt (mapM_ (\(n, r, t, nps, e) -> DTP.assertMapping n r t nps e) mappings)

mkTrie :: e
       -> DTP.TrieM e ()
       -> Either DTP.TrieError (LT.LinearizedTrie e)
mkTrie defElt act =
  ME.runExcept (St.evalStateT (DTP.unM (act >> trieFromState defElt)) s0)
  where
    s0 = DTP.emptyTrieState

trieFromState :: e -> DTP.TrieM e (LT.LinearizedTrie e)
trieFromState defElt = do
  pats <- St.gets DTP.tsPatterns
  t0 <- buildTableLevel pats 0 bitEmpty
  st <- St.get
  return (flattenTables t0 defElt st)

-- | Build the dispatch table on the current bit (given as an 'Int' offset into
-- the bitstream of the instructions.
buildTableLevel :: M.Map DTP.Pattern (DTP.LinkedTableIndex, e)
                -- ^ Remaining valid patterns in this branch
                -> Int
                -- ^ The *bit* index we are computing
                -> BitString
                -- ^ The string of bits followed thus far
                -> DTP.TrieM e DTP.LinkedTableIndex
buildTableLevel patterns bitIndex bitsSoFar = do
  cache <- St.gets DTP.tsCache
  psets <- St.gets DTP.tsPatternSets
  let addPatternToSet ps p =
        case HM.lookup p psets of
          Nothing -> error ("Missing pattern set for pattern: " ++ show p)
          Just s -> s .|. ps
  let pset = F.foldl' addPatternToSet (DTP.PatternSet 0) (M.keys patterns)
  let key = (bitIndex, pset)
  case HM.lookup key cache of
    Just tix -> return tix
    Nothing -> do
      payloads <- T.traverse (makePayload patterns bitIndex bitsSoFar) [False, True]
      tix <- newTable payloads
      St.modify' $ \s -> s { DTP.tsCache = HM.insert key tix (DTP.tsCache s) }
      return tix

-- | Allocate a new dispatch table that can dispatch to the given payloads
--
-- There are just two entries
newTable :: [(Bool, DTP.LinkedTableIndex)] -> DTP.TrieM e DTP.LinkedTableIndex
newTable payloads = do
  tix <- St.gets DTP.tsTblIdSrc
  let a = VU.fromList (fmap snd (L.sortOn fst payloads))
  St.modify' $ \s -> s { DTP.tsTables = M.insert tix a (DTP.tsTables s)
                       , DTP.tsTblIdSrc = DTP.nextTableIndex tix
                       }
  return tix

makePayload :: M.Map DTP.Pattern (DTP.LinkedTableIndex, e)
            -> Int
            -- ^ The bit index
            -> BitString
            -> Bool
            -- ^ The bit we are matching against
            -> DTP.TrieM e (Bool, DTP.LinkedTableIndex)
makePayload patterns bitIndex bitsSoFar bit =
  case M.toList matchingPatterns of
    [] -> return (bit, DTP.defaultElementIndex)
    [(_, (eltIdx, _elt))] -> return (bit, eltIdx)
    _ | all ((> (bitIndex + 1)) . DTP.patternBytes) (M.keys matchingPatterns) -> do
          -- This case makes sure that we have covered all of the patterns
          -- completely; we can't stop early, since there could be a required
          -- fixed bit in the last position that might make a pattern fail even
          -- if the entire prefix matches.
          tix <- buildTableLevel matchingPatterns (bitIndex + 1) bitsSoFar'
          return (bit, tix)
      | M.null negativeMatchingPatterns -> return (bit, DTP.defaultElementIndex)
      | Just (mostSpecificEltIdx, _) <- findMostSpecificPatternElt negativeMatchingPatterns -> do
          return (bit, mostSpecificEltIdx)
      | otherwise -> do
          mapping <- St.gets DTP.tsPatternMnemonics

          let pats = map fst (M.toList negativeMatchingPatterns)
          let mnemonics = catMaybes (flip M.lookup mapping <$> pats)

          error ("Overlapping bit pattern " ++ show (bitIndex, bitsSoFar, mapping, pats, mnemonics))
  where
    bitsSoFar' = bitSnoc bitsSoFar bit
    matchingPatterns' = M.filterWithKey (patternMatches bitIndex bit) patterns
    matchLength = minimum (DTP.patternBytes <$> M.keys matchingPatterns')
    matchingPatterns = M.filterWithKey (\p _ -> DTP.patternBytes p == matchLength) matchingPatterns'
    -- Negative matching patterns are used to disambiguate when we are out of
    -- bits
    negativeMatchingPatterns = M.filterWithKey (negativePatternMatches bitsSoFar') matchingPatterns

flattenTables :: DTP.LinkedTableIndex -> e -> DTP.TrieState e -> LT.LinearizedTrie e
flattenTables t0 defElt finSt =
  LT.LinearizedTrie { LT.ltStartIndex = ix0
                    , LT.ltPayloads = V.fromList (undefined : defElt : sortedElts)
                    , LT.ltParseTables = SV.fromList parseTables
                    }
  where
    ix0 = fromIntegral (DTP.unFTI (linkedToFlatIndex t0))
    -- We reverse the elements here because their IDs are negated
    sortedElts = [ e | (_ix, e) <- reverse (L.sortOn fst (M.elems (DTP.tsPatterns finSt))) ]
    parseTables = [ DTP.unFTI (linkedToFlatIndex e)
                  | (_tblIdx, tbl) <- L.sortOn fst (M.toList (DTP.tsTables finSt))
                  , e <- VU.toList tbl
                  ]

patternMatches :: Int -> Bool -> DTP.Pattern -> e -> Bool
patternMatches bitIndex bit p _ =
  (bit .&. patRequiredBit) == patTrueBit
  where
    patRequiredBit = DTP.requiredMask p `bitIndexBytestring` bitIndex
    patTrueBit = DTP.trueMask p `bitIndexBytestring` bitIndex

negativePatternMatches :: BitString -> DTP.Pattern -> e -> Bool
negativePatternMatches bs p _ =
  all (uncurry negativeMatch) (DTP.negativePairs p)
  where
    negativeMatch negMask negBits =
      case all (==0) (BS.unpack negMask) of
        True -> True
        False -> not (and (zipWith3 negativeBitMatches (asBitList negMask) (asBitList negBits) (asBitList bs)))
    negativeBitMatches :: Bool -> Bool -> Bool -> Bool
    negativeBitMatches negBitMask negBitValue bit =
      (bit .&. negBitMask) == negBitValue

findMostSpecificPatternElt :: M.Map DTP.Pattern (DTP.LinkedTableIndex, e) -> Maybe (DTP.LinkedTableIndex, e)
findMostSpecificPatternElt = findMostSpecific [] . M.toList
  where
    findMostSpecific checked pats =
      case pats of
        [] -> Nothing
        e@(pat, elt) : rest
          | checkPatterns pat checked rest -> Just elt
          | otherwise -> findMostSpecific (e:checked) rest
    -- Return 'True' if the pattern @p@ is more specific than all of the
    -- patterns in @checked@ and @rest@.
    checkPatterns p checked rest =
      all (isMoreSpecific p) checked && all (isMoreSpecific p) rest
    -- Return 'True' if @target@ is more specific than @p@.
    isMoreSpecific target (p, _) = requiredBitCount target > requiredBitCount p
    requiredBitCount bs = sum [ popCount w
                              | w <- BS.unpack (DTP.requiredMask bs)
                              ]

-- | Convert from indexes in linked tables to indexes suitable for the flat
-- tables we are constructing for the ultimate BitTrie
--
-- Unlike the 'ByteTrie' (which has 256 entries per table), this one only has 2.
linkedToFlatIndex :: DTP.LinkedTableIndex -> DTP.FlatTableIndex
linkedToFlatIndex (DTP.LTI i)
  | i < 0 = DTP.FTI i
  | otherwise = DTP.FTI (i * 2)

-- | A helper type to track accumulated concrete bits
--
-- The ByteTrie uses a ByteString since that is fairly natural.  We could do the
-- same here, but a more specific bitstring type is a bit more explicit and
-- easier to follow
newtype BitString = BitString (VU.Vector Bool)
  deriving (Eq, Ord, Show)

bitSnoc :: BitString -> Bool -> BitString
bitSnoc (BitString v) a = BitString (VU.snoc v a)

bitEmpty :: BitString
bitEmpty = BitString VU.empty


-- | Extract a bit (indexed from zero) from a bytestring
--
-- Indexing starts from byte 0:
--
-- > |  Byte 0               |  Byte 1               |
-- > | 0| 1| 2| 3| 4| 5| 6| 7| 8| 9|10|11|12|13|14|15|
--
-- Note that this order of bits in each byte is opposite from how we would
-- normally think about it, so this function handles the correction.  i.e., bit
-- number 7 in byte 0 is actually bit 0 for the purposes of indexing into the
-- pattern.
bitIndexBytestring :: BS.ByteString -> Int -> Bool
bitIndexBytestring bs i = testBit byte (7 - bitIndex)
  where
    (byteIndex, bitIndex) = i `divMod` 8
    byte = bs `BS.index` byteIndex

-- | An ad-hoc overload to turn sequences of bits into lists of bits
class AsBitList a where
  asBitList :: a -> [Bool]

-- | This instance applies the same bit order swapping logic as 'bitIndexBytestring'
instance AsBitList BS.ByteString where
  asBitList = concatMap flipBits . BS.unpack

flipBits :: Word8 -> [Bool]
flipBits w = reverse [ testBit w i | i <- [0..7] ]

instance AsBitList BitString where
  asBitList (BitString v) = VU.toList v
