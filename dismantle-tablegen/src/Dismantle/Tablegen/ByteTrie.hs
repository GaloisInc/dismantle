{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE StandaloneDeriving #-}

module Dismantle.Tablegen.ByteTrie (
  byteTrie,
  lookupByte
  ) where
import           Debug.Trace

import           Control.Applicative
import qualified Control.Monad.Except as E
import qualified Control.Monad.State.Strict as St
import           Data.Bits ( (.&.), (.|.), popCount )
import qualified Data.ByteString as BS
import qualified Data.Foldable as F
import qualified Data.HashMap.Strict as HM
import qualified Data.List as L
import qualified Data.Map.Strict as M
import           Data.Maybe (catMaybes)
import qualified Data.Traversable as T
import qualified Data.Vector as V
import qualified Data.Vector.Storable as SV
import qualified Data.Vector.Unboxed as VU
import           Data.Word ( Word8 )

import qualified Dismantle.Tablegen.LinearizedTrie as LT
import qualified Dismantle.Tablegen.Patterns as DTP

import           Prelude


-- | Look up a byte in the trie.
--
-- The result could either be a terminal element or another trie that
-- must be fed another byte to continue the lookup process.
--
-- There is no explicit return value for invalid encodings.  The trie
-- is constructed such that no invalid encodings are possible (i.e.,
-- the constructor is required to explicitly represent those cases
-- itself).
lookupByte :: LT.LinearizedTrie a -> Word8 -> Either (LT.LinearizedTrie a) a
lookupByte lt byte
  | tableVal < 0 = Right (LT.ltPayloads lt `V.unsafeIndex` fromIntegral (negate tableVal))
  | otherwise = Left $ lt { LT.ltStartIndex = fromIntegral tableVal }
  where
    tableVal = LT.ltParseTables lt `SV.unsafeIndex` (fromIntegral byte + LT.ltStartIndex lt)

-- | Construct a 'ByteTrie' from a list of mappings and a default element
byteTrie :: e -> [(String, BS.ByteString, BS.ByteString, [(BS.ByteString, BS.ByteString)], e)] -> Either DTP.TrieError (LT.LinearizedTrie e)
byteTrie defElt mappings = mkTrie defElt (mapM_ (\(n, r, t, nps, e) -> DTP.assertMapping n r t nps e) mappings)
  where _showMapping (s, bs0, bs1, bs2, bs3, _) = show (s, BS.unpack bs0, BS.unpack bs1, BS.unpack bs2, BS.unpack bs3)

-- | Construct a 'ByteTrie' through a monadic assertion-oriented interface.
--
-- Any bit pattern not covered by an explicit assertion will default
-- to the undefined parse value.
mkTrie :: e
       -- ^ The value of an undefined parse
       -> DTP.TrieM e ()
       -- ^ The assertions composing the trie
       -> Either DTP.TrieError (LT.LinearizedTrie e)
mkTrie defElt act =
  E.runExcept (St.evalStateT (DTP.unM (act >> trieFromState defElt)) s0)
  where
    s0 = DTP.emptyTrieState

trieFromState :: e -> DTP.TrieM e (LT.LinearizedTrie e)
trieFromState defElt = do
  pats <- St.gets DTP.tsPatterns
  t0 <- buildTableLevel pats 0 BS.empty
  st <- St.get
  return (flattenTables t0 defElt st)

-- | Flatten the maps of parse tables that reference each other into a single
-- large array with indices into itself (positive indices) and the payloads
-- table (negative indices).
--
-- The 'LinkedTableIndex' parameter is the index of the table to start from.
-- Note that it almost certainly *won't* be zero, as table IDs are allocated
-- depth-first.
flattenTables :: DTP.LinkedTableIndex -> e -> DTP.TrieState e -> LT.LinearizedTrie e
flattenTables t0 defElt st =
  LT.LinearizedTrie { LT.ltStartIndex = ix0
                    , LT.ltPayloads = V.fromList (undefined : defElt : sortedElts)
                    , LT.ltParseTables = SV.fromList parseTables
                    }
  where
    ix0 = fromIntegral (DTP.unFTI (linkedToFlatIndex t0))
    -- We have to reverse the elements because their IDs are negated
    sortedElts = [ e | (_ix, e) <- reverse (L.sortOn fst (M.elems (DTP.tsPatterns st))) ]
    -- The table index doesn't really matter - it is just important to keep the
    -- tables in relative order
    parseTables = [ DTP.unFTI (linkedToFlatIndex e)
                  | (_tblIx, tbl) <- L.sortOn fst (M.toList (DTP.tsTables st))
                  , e <- VU.toList tbl
                  ]

-- | Convert between table index types.
--
-- The conversion assumes that tables will be laid out in order.  Each table is
-- 256 entries, so that is the conversion factor between table number and index
-- into the 'btParseTables' array.
linkedToFlatIndex :: DTP.LinkedTableIndex -> DTP.FlatTableIndex
linkedToFlatIndex (DTP.LTI i)
  | i < 0 = DTP.FTI i
  | otherwise = DTP.FTI (i * 256)

-- | Builds a table for a given byte in the sequence (or looks up a matching
-- table in the 'tsCache')
buildTableLevel :: M.Map DTP.Pattern (DTP.LinkedTableIndex, e)
                -- ^ Remaining valid patterns
                -> Int
                -- ^ Byte index we are computing
                -> BS.ByteString
                -- ^ string of bytes we have followed thus far
                -> DTP.TrieM e DTP.LinkedTableIndex
buildTableLevel patterns byteIndex bytesSoFar = do
  cache <- St.gets DTP.tsCache
  psets <- St.gets DTP.tsPatternSets
  let addPatternToSet ps p =
        case HM.lookup p psets of
          Nothing -> error ("Missing pattern set for pattern: " ++ show p)
          Just s -> s .|. ps
  let pset = F.foldl' addPatternToSet (DTP.PatternSet 0) (M.keys patterns)
  let key = (byteIndex, pset)
  case HM.lookup key cache of
    Just tix -> return tix
    Nothing -> do
      payloads <- T.traverse (makePayload patterns byteIndex bytesSoFar) byteValues
      tix <- newTable payloads
      St.modify' $ \s -> s { DTP.tsCache = HM.insert key tix (DTP.tsCache s) }
      return tix
  where
    maxWord :: Word8
    maxWord = maxBound
    byteValues = [0 .. maxWord]

-- | Allocate a new chunk of the table with the given payload
--
-- The table is assigned a unique ID and added to the table list
newTable :: [(Word8, DTP.LinkedTableIndex)] -> DTP.TrieM e DTP.LinkedTableIndex
newTable payloads = do
  tix <- St.gets DTP.tsTblIdSrc
  let a = VU.fromList (fmap snd (L.sortOn fst payloads))
  St.modify' $ \s -> s { DTP.tsTables = M.insert tix a (DTP.tsTables s)
                       , DTP.tsTblIdSrc = DTP.nextTableIndex tix
                       }
  return tix

makePayload :: M.Map DTP.Pattern (DTP.LinkedTableIndex, e)
            -- ^ Valid patterns at this point in the trie
            -> Int
            -- ^ Byte index we are computing
            -> BS.ByteString
            -- ^ bytes we have used to traverse thus far
            -> Word8
            -- ^ The byte we are matching patterns against
            -> DTP.TrieM e (Word8, DTP.LinkedTableIndex)
makePayload patterns byteIndex bytesSoFar byte =
  case M.toList matchingPatterns of
    [] -> return (byte, DTP.defaultElementIndex)
    [(_, (eltIdx, _elt))] -> return (byte, eltIdx)
    _ | all ((> (byteIndex + 1)) . DTP.patternBytes) (M.keys matchingPatterns) -> do
          -- If there are more bytes available in the overlapping patterns, extend
          -- the trie to inspect one more byte
          {-

            Instead of this, we should probably choose the pattern that has the
            most required bits in the *current byte*.  We know that the pattern
            already matches due to the computation of 'matchingPatterns'.  If
            there are an equal number of matching bits in the current byte,
            *then* extend the trie to the next level.

          -}
          tix <- buildTableLevel matchingPatterns (byteIndex + 1) bytesSoFar'
          return (byte, tix)
      | M.null negativeMatchingPatterns -> return (byte, DTP.defaultElementIndex)
      | Just (mostSpecificEltIdx, _) <- findMostSpecificPatternElt negativeMatchingPatterns -> do
          -- If there are no more bytes *and* one of the patterns is more specific
          -- than all of the others, take the most specific pattern
          return (byte, mostSpecificEltIdx)
      | otherwise -> do
          -- Otherwise, the patterns overlap and we have no way to
          -- choose a winner, so fail
          mapping <- St.gets DTP.tsPatternMnemonics

          let pats = map fst (M.toList negativeMatchingPatterns)
              mnemonics = catMaybes $ (flip M.lookup mapping) <$> pats

          traceM $ show (DTP.patternBytes <$> M.keys negativeMatchingPatterns)

          E.throwError (DTP.OverlappingBitPatternAt byteIndex (BS.unpack bytesSoFar') $ zip3 pats ((:[]) <$> mnemonics) (DTP.patternBytes <$> pats))
  where
    bytesSoFar' = BS.snoc bytesSoFar byte
    -- First, filter out the patterns that don't match the current byte at the given
    -- byte index.
    matchingPatterns' = M.filterWithKey (patternMatches byteIndex byte) patterns
    -- FIXME: Next, reduce the matching patterns to only those with shortest
    -- length. This should probably be done at the top level rather than here.
    matchLength = minimum (DTP.patternBytes <$> M.keys matchingPatterns')
    matchingPatterns = M.filterWithKey (\p _ -> DTP.patternBytes p == matchLength) matchingPatterns'

    -- This should only be used to disambiguate when we no longer have any bytes left.
    negativeMatchingPatterns = M.filterWithKey (negativePatternMatches bytesSoFar') matchingPatterns

-- | Return the element associated with the most specific pattern in the given
-- collection, if any.
--
-- A pattern is the most specific if its required bits are a strict superset of
-- the required bits of the other patterns in the initial collection.
--
-- The required invariant for this function is that all of the patterns in the
-- collection are actually congruent (i.e., *could* have all of their bits
-- matching).
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

-- | Return 'True' if the 'Pattern' *could* match the given byte at the 'Int' byte index
patternMatches :: Int -> Word8 -> DTP.Pattern -> e -> Bool
patternMatches byteIndex byte p _ = -- (Pattern { requiredMask = req, trueMask = true }) _ =
  (byte .&. patRequireByte) == patTrueByte
  where
    patRequireByte = DTP.requiredMask p `BS.index` byteIndex
    patTrueByte = DTP.trueMask p `BS.index` byteIndex

-- | Return 'True' if a 'BS.ByteString' does not match with any of the negative bit
-- masks in a pattern.
-- FIXME: We do not check that the bytestrings have the same length
negativePatternMatches :: BS.ByteString -> DTP.Pattern -> e -> Bool
negativePatternMatches bs p _ = all (uncurry (negativeMatch bs)) (DTP.negativePairs p)
  where negativeMatch bs' negMask negBits =
          case (all (==0) (BS.unpack negMask)) of
            True -> True
            False -> not (and (zipWith3 negativeByteMatches (BS.unpack negMask) (BS.unpack negBits) (BS.unpack bs')))
        negativeByteMatches :: Word8 -> Word8 -> Word8 -> Bool
        negativeByteMatches negByteMask negByteBits byte = (byte .&. negByteMask) == negByteBits




{- Note [Trie Structure]

The 'ByteTrie' maps sequences of bytes to payloads of type 'a'.

The 'ByteTrie' is conceptually a DAG of parsing tables linked together, where
each table is 256 elements and intended to be indexed by a byte.  Each lookup
either yields a payload or another parse table (which requires more bytes to
yield a payload).

The actual implementation of the 'ByteTrie' flattens the DAG structure of the
tables into two arrays: an array with indexes into itself ('btParseTables') and
a separate array of payload values ('btPayloads').  A negative value in
'btParseTables' indicates that the value should be negated and used as an index
into 'btPayloads'.  A non-negative value is an index into 'btParseTables'.

When asserting mappings into the trie, we assign a unique numeric identifier to
each payload.  This lets us reference payloads without putting Ord or Hashable
constraints on them.

To construct the trie, pass in the list of all possible patterns and
the byte index to compute (i.e., start with 0).

Enumerate the values of the byte, for each value filtering down a list
of possibly-matching patterns.

* If there are multiple patterns and bits remaining, generate a
  'NextTable' reference and recursively build that table by passing
  the remaining patterns and byte index + 1 to the procedure.  If
  there is overlap, *all* of the patterns must have bytes remaining.

* If there are multiple patterns and no bits remaining in them, raise
  an overlapping pattern error

* If there is a single pattern left, generate an 'Element' node

* If there are no patterns, generate an 'Element' node with the default element

-}
