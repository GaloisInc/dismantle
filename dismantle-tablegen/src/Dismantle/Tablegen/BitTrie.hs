{-# LANGUAGE MultiWayIf #-}
module Dismantle.Tablegen.BitTrie (
  bitTrie,
  lookupByte
  ) where

import qualified Control.Exception as X
import qualified Control.Monad.Except as ME
import qualified Control.Monad.State.Strict as St
import           Data.Bits ( (.|.), (.&.), popCount, testBit, countLeadingZeros )
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
lookupByte lt byte = lookupByteRec lt byte 0

lookupByteRec :: LT.LinearizedTrie a
              -> Word8
              -> Int
              -> Either (LT.LinearizedTrie a) a
lookupByteRec lt byte n
  -- We found a terminal node in the trie, return the payload
  | tableVal < 0 = Right (LT.ltPayloads lt `V.unsafeIndex` fromIntegral (negate tableVal - 1))
  -- We reached the end of the byte and the next node is another trie
  | n == 7 = Left $ lt { LT.ltStartIndex = fromIntegral tableVal }
  -- Otherwise, just keep traversing bits
  | otherwise =
    let lt' = lt { LT.ltStartIndex = fromIntegral tableVal }
    in lookupByteRec lt' byte (n + 1)
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
  payloads <- T.traverse (makePayloadCached patterns bitIndex bitsSoFar) [False, True]
  newTable payloads

-- | Allocate a new dispatch table that can dispatch to the given payloads
--
-- There are just two entries
newTable :: [(Bool, DTP.LinkedTableIndex)] -> DTP.TrieM e DTP.LinkedTableIndex
newTable payloads = do
  let a = VU.fromList (fmap snd (L.sortOn fst payloads))
  tcache <- St.gets DTP.tsTableCache
  case HM.lookup (DTP.HashableUVec a) tcache of
    Just tix -> return tix
    Nothing -> do
      tix <- St.gets DTP.tsTblIdSrc
      -- let a = VU.fromList (fmap snd (L.sortOn fst payloads))
      St.modify' $ \s -> s { DTP.tsTables = M.insert tix a (DTP.tsTables s)
                           , DTP.tsTblIdSrc = DTP.nextTableIndex tix
                           , DTP.tsTableCache = HM.insert (DTP.HashableUVec a) tix (DTP.tsTableCache s)
                           }
      return tix

patternBits :: DTP.Pattern -> Int
patternBits = (8 *) . DTP.patternBytes

-- | This is a wrapper around 'makePayload' that handles all of the necessary
-- caching heuristics to make the parse table construction tractable.
makePayloadCached :: M.Map DTP.Pattern (DTP.LinkedTableIndex, e)
                  -> Int
                  -- ^ The bit index
                  -> BitString
                  -> Bool
                  -- ^ The bit we are matching against
                  -> DTP.TrieM e (Bool, DTP.LinkedTableIndex)
makePayloadCached patterns bitIndex bitsSoFar bit = do
  cache <- St.gets DTP.tsCache
  psets <- St.gets DTP.tsPatternSets
  let addPatternToSet ps p =
        case HM.lookup p psets of
          Nothing -> error ("Missing pattern set for pattern: " ++ show p)
          Just s -> s .|. ps
  let pats = M.keys matchingPatterns
  let pset = F.foldl' addPatternToSet DTP.emptyPatternSet pats
  let key = (bitIndex, pset)
  case HM.lookup key cache of
    Just tix
      | not (inNegativePattern bitIndex bit pats) -> do
          return (bit, tix)
    _ -> do
      payload@(_, tix) <- makePayload matchingPatterns bitIndex bitsSoFar' bit
      if | not (inNegativePattern bitIndex bit pats) ->
           St.modify' $ \s -> s { DTP.tsCache = HM.insert key tix (DTP.tsCache s) }
         | otherwise -> return ()
      return payload
  where
    -- The current bit string with the current bit under analysis appended
    bitsSoFar' = bitSnoc bitsSoFar bit
    -- All of the patterns that match the current bit prefix, purely considering
    -- the positive patterns
    --
    -- Note that we don't pass in the entire bit prefix here: since we are in
    -- this state, it means that the prefix matches all of the patterns we have
    -- in @patterns@, so we don't need to re-examine those bits.  We only need
    -- to look at the current bit.
    matchingPositivePatterns = M.filterWithKey (patternMatches bitIndex bit) patterns
    -- The remaining patterns that also satisfy any negative patterns that we
    -- can apply at this point
    --
    -- We apply negative patterns as soon as possible: once we have enough bits
    -- in the prefix to cover all of the required bits for the negative pattern
    -- mask.
    matchingPatterns = M.filterWithKey (negativePatternMatches bitsSoFar') matchingPositivePatterns

{- Note [Caching With Negation]

The underlying problem with the cache is that we are keying it on the set of
patterns that match up to this point.  If we reuse cached results on a bit
sequence that triggers a negative pattern, we will get incorrect parse tables
(i.e., they would ignore the negative patterns).  Precise caching would be
*very* difficult.  Instead, we inspect the bit pattern to see if we are on a
path that *could* trigger a negative pattern from the currently active pattern
set.  If we are, we simply do not use the cached result.

The most common use of negative patterns is to say that some N-bit operand is a
register operand, but must NOT equal a certain value in order to be a valid
parse (because the excluded value parses as another instruction).

- Under this example, we *might* trigger the negative pattern at any time until
  the bit pattern is fully resolved (i.e., parsing is past the last bit in the
  negative pattern)
- However, not caching at all up to that point is extremely expensive

Question 1: Can we use cached values if the prefix does not include any of the
bits in the negative mask?

Question 2: Do we need to worry about any negative bits except for the last one?

Observation: construction is DFS.  In a bit pattern like

> P = |1|0|?|?|1|?|?|1|
>      0 1 2 3 4 5 6 7

where the second operand is not allowed to be 0b11.  We traverse out to the end
of the bit-string depth first covering suffixes (starting from bit 5):

- 0b001   # operand is 0b00 (and fine)
- 0b011   # Operand is 0b01 (and still fine)
- 0b101   # Here we backtrack to the first bit of the operand, which is 0b10 (and fine)

These all share the same suffix of the tree, with a node indicating the
transition on the final concrete 1 bit into an accept state.  That cache entry
will look something like

(7, {P}) -> (1)->Accept
(6, {P}) ->

and all of these share it.  When we backtrack to the first operand bit, it
switches to a 1.  But the construction sees that there is a cached value for the
next bit, even though it would be invalidated by the negative pattern.  It takes
it, producing incorrect parse tables.  We need to catch it, but it is actually
*not* something we can see at the last bit because we'll never get a chance to
inspect that last bit.  We need to re-evaluate our cache usage as we unwind the
recursion.  It seems like the best place to take corrective action is at the
*first* bit of a negative pattern where we could maintain localized state on
recursive calls until the end of the negated patterns.  If we are making
"progress" towards satisfying a negated pattern, we cannot use the cache.

Some previous (unsuccessful) attempts to solve this problem include:
- Cache invalidation
  - The recursion pattern we are using meant that this basically eliminated all caching
- Only caching once all negated bits have been resolved
  - This was too expensive in practice

-}

{- Note [Cache Invalidation]

The underlying problem with the cache is that we are keying it on the set of
patterns that match up to this point.  If we reuse cached results on a bit
sequence that triggers a negative pattern, we will get incorrect parse tables
(i.e., they would ignore the negative patterns).  Precise caching would be
*very* difficult, so we instead invalidate the cache when we encounter that
situation.

The cache invalidation mechanism simply piggybacks on the cache: every cached
entry includes a 'DTP.InvalidationPredicate' that returns True if the entry
needs to be invalidated.  This way we only have to pay the cost of constructing
the predicate when we make cache entries; checking it is cheaper.

The cache must be invalidated if the 'BitString' input to the predicate
satisfies the negative pattern of *any* of the patterns.


FIXME: The current cache invalidation strategy is not good.  The logic is subtly
wrong, probably, but it is just inherently bad in that it clears the cache too
often.  A more efficient model might be to look at the full concrete BitString
in 'makePayloadCached' and compute all of the matching patterns *accounting for
negative patterns*.  Then the cache entries can be in terms of the matching
patterns and depth again, as is proper.  That would obviate the need for any
cache invalidation.

Question: Can you only cache if there are no more negative patterns left?

-}

makePayload :: M.Map DTP.Pattern (DTP.LinkedTableIndex, e)
            -- ^ The set of patterns that match the current bit-string,
            -- accounting for both negative and positive patterns.
            --
            -- Note that specialization and length matching rules are not yet
            -- accounted for.
            -> Int
            -- ^ The bit index
            -> BitString
            -- ^ This bitstring has the current bit (@bit@ in the argument list)
            -- already appended
            -> Bool
            -- ^ The bit we are matching against
            -> DTP.TrieM e (Bool, DTP.LinkedTableIndex)
makePayload matchingPatterns bitIndex bitsSoFar bit =
  if | M.null matchingPatterns -> do
         -- We failed to match any patterns (and eliminated all of the possible
         -- patterns)
         return (bit, DTP.defaultElementIndex)
     | [(pat, (eltIdx, _elt))] <- M.toList minLengthPatterns
     , patternBits pat - 1 == bitIndex -> do
         -- In the above case we had a single match on the last bit.  This case
         -- asks the question: if there is a collection of patterns where some
         -- are shorter than the rest, do any of the shorter patterns match
         -- conclusively at this bit pattern?  If so, that is also an answer.
         return (bit, eltIdx)
     | Just (mostSpecificEltIdx, _) <- findMostSpecificPatternElt minLengthPatterns -> do
         -- It could be the case that multiple patterns happen to match.  In
         -- those cases, we choose the unique pattern with the most required
         -- bits.
         return (bit, mostSpecificEltIdx)
     | not (M.null longerPatterns) -> do
         -- If we have no match and there are some patterns remaining that are
         -- longer than the current bit string, extend the parse tree to cover
         -- those patterns.
         tix <- buildTableLevel longerPatterns (bitIndex + 1) bitsSoFar
         return (bit, tix)
     | otherwise -> do
         mapping <- St.gets DTP.tsPatternMnemonics

         let pats = map fst (M.toList matchingPatterns)
         let mnemonics = catMaybes (flip M.lookup mapping <$> pats)

         error ("Overlapping bit pattern " ++ show (bitIndex, bitsSoFar, mapping, pats, mnemonics))
  where
    -- Now find the length of the smallest matching pattern
    --
    -- If there are multiple matches and one is shorter than the rest, we can
    -- use this to say that the unique shortest pattern supersedes the others.
    -- minPatLen = minimum (patternBits <$> M.keys matchingNegativePatterns)

    -- It could be the case that some patterns are shorter than others, and that
    -- we hit a match to the shorter patterns early. We want to take those
    -- matches first.
    --
    -- However, if the shorter set of patterns do not match, we want to continue
    -- the search with the full set of possible patterns.  If we continue a
    -- search past the size of a shortened pattern, we should remove the short
    -- patterns entirely because they did not match (so they will not match)
    minLengthPatterns = M.filterWithKey (\p _ -> patternBits p - 1 == bitIndex) matchingPatterns

    -- Remaining matching patterns longer than the current bit length
    longerPatterns = M.filterWithKey (\p _ -> patternBits p >= bitIndex) matchingPatterns

    -- matchingPatterns = matchingNegativePatterns

flattenTables :: DTP.LinkedTableIndex -> e -> DTP.TrieState e -> LT.LinearizedTrie e
flattenTables t0 defElt finSt =
  LT.LinearizedTrie { LT.ltStartIndex = ix0
                    , LT.ltPayloads = V.fromList (defElt : sortedElts)
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

-- | Return 'True' if the concrete bit value (at the specified index) satisfies
-- the bit at that location in the 'DTP.Pattern'
--
-- If the pattern is not long enough to provide that bit, return False
patternMatches :: Int -> Bool -> DTP.Pattern -> e -> Bool
patternMatches bitIndex bit p _
  | bitIndex >= patternBits p = False
  | otherwise =
    (bit .&. patRequiredBit) == patTrueBit
  where
    patRequiredBit = DTP.requiredMask p `bitIndexBytestring` bitIndex
    patTrueBit = DTP.trueMask p `bitIndexBytestring` bitIndex

-- | Return 'True' if the current value matches any of the negative patterns; we
-- will avoid the cache if it does
inNegativePattern :: Int
                  -- ^ Bit index
                  -> Bool
                  -- ^ Bit value
                  -> [DTP.Pattern]
                  -- ^ Currently matching patterns
                  -> Bool
inNegativePattern bitIndex bVal = any (any negativeMatch . DTP.negativePairs)
  where
    negativeMatch (negMask, negBits) =
      or [ bitIndexBytestring negMask bitIndex && bVal == bitIndexBytestring negBits bitIndex
         , bitIndex + 1 < BS.length negMask * 8 && bitIndexBytestring negMask (bitIndex + 1)
         ]

-- | Return True if the 'BitString' does not violate any negative patterns in the 'DTP.Pattern'
--
-- NOTE: Negative patterns can only reject a 'BitString' once all of the
-- required bits in the negative bitmasks are present in the 'BitString'.  If
-- the 'BitString' does not contain some of the required negative bit positions,
-- this function simply returns True.
negativePatternMatches :: BitString -> DTP.Pattern -> e -> Bool
negativePatternMatches bs p _ =
  all (uncurry negativeMatch) (DTP.negativePairs p)
  where
    negativeMatch negMask negBits =
      let mHighestBit = highestRequiredBit negMask
      in if | mHighestBit == Nothing ->
              -- If there are actually no required bits in the negative mask, we
              -- know it won't reject anything
              True
            | Just highestBit <- mHighestBit
            , not (bitStringLength bs > highestBit) ->
              -- If we don't have enough bits for the highest bit number in the
              -- negative mask, it won't match yet
              --
              --                 abc
              -- Bit String: ????????
              -- Mask:       00001100
              --
              -- - In @a@, length(bs)=5, highestBit(mask)=5
              -- - In @b@, length(bs)=6, highestBit(mask)=5
              -- - In @c@, length(bs)=7, highestBit(mask)=5
              --
              -- So we cannot apply the mask if until
              --
              -- > length(bs) > highestBit(mask)
              True
            | otherwise ->
              -- NOTE: the 'asBitList' function unrolls all of the bits in the
              -- ByteString values; the zipWith just takes the necessary set
              -- given the length of the bitstring.
              not (and (zipWith3 negativeBitMatches (asBitList negMask) (asBitList negBits) (asBitList bs)))

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
  deriving (Eq, Ord)

instance Show BitString where
  show (BitString v) = "0b" ++ map asBitChar (VU.toList v)

asBitChar :: Bool -> Char
asBitChar b =
  case b of
    True -> '1'
    False -> '0'

bitSnoc :: BitString -> Bool -> BitString
bitSnoc (BitString v) a = BitString (VU.snoc v a)

bitEmpty :: BitString
bitEmpty = BitString VU.empty

bitStringLength :: BitString -> Int
bitStringLength (BitString v) = VU.length v

-- | Return the index of the highest 1 bit in the 'BS.ByteString'
--
-- If there are no 1 bits, returns Nothing
highestRequiredBit :: BS.ByteString -> Maybe Int
highestRequiredBit = fst . BS.foldl' highestSet (Nothing, 0)
  where
    highestSet (mIdx, byteNum) b =
      let ix = 7 - countLeadingZeros b
      in if | ix < 0 -> (mIdx, byteNum + 1)
            | otherwise -> (Just (byteNum * 8 + ix), byteNum + 1)

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
bitIndexBytestring bs i =
  X.assert (i >= 0) $ X.assert (BS.length bs > byteIndex) $ testBit byte bitIndex
  where
    (byteIndex, bitIndex) = i `divMod` 8
    byte = bs `BS.index` byteIndex

-- | An ad-hoc overload to turn sequences of bits into lists of bits
class AsBitList a where
  asBitList :: a -> [Bool]

-- | This instance applies the same bit order swapping logic as 'bitIndexBytestring'
--
-- Note that it achieves the flipping by virtue of the order it constructs the
-- list in: bit 0-7 (so the Bool corresponding to bit 0 is at idx 0 of the
-- generated sub-lists).
instance AsBitList BS.ByteString where
  asBitList = concatMap flipBits . BS.unpack

flipBits :: Word8 -> [Bool]
flipBits w = [ testBit w i | i <- [0..7] ]

instance AsBitList BitString where
  asBitList (BitString v) = VU.toList v

{- [BitTrie Design]

This implementation is inspired by the ByteTrie, but instead of dispatching
based on byte values at each level of the tree, it dispatches one bit at a time.

At each level of the trie construction, we have a set of patterns that matched
up to the current bit.  For each concrete bit (True or False) at the current
position, we filter the set of patterns down to those that match accounting for
the current concrete bit.  We apply negative patterns as soon as possible.  Once
we reach the end of the bit stream, we produce a leaf that parses using the
single remaining matching pattern.

There is a special case to handle some overlapping patterns where we take the
'most specific pattern'; see makePayload for details.

Some important semantic notes for the table construction process:

- Negative patterns can only be matched once all of their required bits are
  present.  It would probably be best to match them as soon as possible to
  simplify the logic and reduce the size of matched pattern sets.

- If table results are cached, the cache must respect negative patterns.  If we
  don't respect negative patterns, there will be invalid patterns in the suffix
  returned by the cache lookup

  Instead of invalidating the cache at each lookup, it might be more efficient
  to compute a predicate for cached entries that returns 'True' if the entry is
  not valid for the "current"

- We can match a negative pattern once we have reached the last set bit in the
  negative pattern mask

-}
