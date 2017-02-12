{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
module Dismantle.Tablegen.ByteTrie (
  ByteTrie,
  byteTrie,
  mkTrie,
  lookupByte,
  assertMapping,
  Bit(..),
  TrieM,
  -- * Errors
  TrieError(..)
  ) where

import Control.Applicative
import Control.DeepSeq
import qualified Control.Monad.State.Strict as St
import qualified Control.Monad.Except as E
import qualified Data.Array as A
import Data.Bits ( (.&.) )
import qualified Data.ByteString as BS
import qualified Data.Map.Strict as M
import qualified Data.Set as S
import qualified Data.Traversable as T
import Data.Word ( Word8 )
import Language.Haskell.TH
import Language.Haskell.TH.Syntax ( Lift(..) )

import Prelude

data Payload a = NextTable (ByteTrie a)
               | Element a
               deriving (Show)

-- | A data type mapping sequences of bytes to elements of type @a@
newtype ByteTrie a = ByteTrie (A.Array Word8 (Payload a))
  deriving (Show)

-- | A bit with either an expected value ('ExpectedBit') or an
-- unconstrained value ('Any')
data Bit = ExpectedBit !Bool
         | Any
         deriving (Eq, Ord, Show)

instance Lift Bit where
  lift b =
    case b of
      Any -> conE 'Any
      ExpectedBit bit -> conE 'ExpectedBit `appE` lift bit

instance NFData Bit where
  rnf _ = ()

-- | A wrapper around a sequence of 'Bit's
data Pattern = Pattern { requiredMask :: BS.ByteString
                       , trueMask :: BS.ByteString
                       }
               deriving (Eq, Ord, Show)

-- | Return the number of bytes occupied by a 'Pattern'
patternBytes :: Pattern -> Int
patternBytes = BS.length . requiredMask

-- | Look up a byte in the trie.
--
-- The result could either be a terminal element or another trie that
-- must be fed another byte to continue the lookup process.
--
-- There is no explicit return value for invalid encodings.  The trie
-- is constructed such that no invalid encodings are possible (i.e.,
-- the constructor is required to explicitly represent those cases
-- itself).
lookupByte :: ByteTrie a -> Word8 -> Either (ByteTrie a) a
lookupByte (ByteTrie a) b =
  case a A.! b of
    Element e -> Right e
    NextTable t -> Left t

data TrieError = OverlappingBitPattern [Pattern]
               | InvalidPatternLength Pattern
  deriving (Eq, Show)

data TrieState e = TrieState { tsPatterns :: !(M.Map Pattern e)
                             , tsCache :: !(M.Map (Int, S.Set Pattern) (ByteTrie e))
                             }

newtype TrieM e a = TrieM { unM :: St.StateT (TrieState e) (E.Except TrieError) a }
  deriving (Functor,
            Applicative,
            Monad,
            E.MonadError TrieError,
            St.MonadState (TrieState e))

-- | Construct a 'ByteTrie' from a list of mappings and a default element
byteTrie :: e -> [(BS.ByteString, BS.ByteString, e)] -> Either TrieError (ByteTrie e)
byteTrie defElt mappings =
  mkTrie defElt (mapM_ (\(r, t, e) -> assertMapping r t e) mappings)

-- | Construct a 'ByteTrie' through a monadic assertion-oriented interface.
--
-- Any bit pattern not covered by an explicit assertion will default
-- to the undefined parse value.
mkTrie :: e
       -- ^ The value of an undefined parse
       -> TrieM e ()
       -- ^ The assertions composing the trie
       -> Either TrieError (ByteTrie e)
mkTrie defElt act =
  E.runExcept (St.evalStateT (unM (act >> trieFromState defElt)) s0)
  where
    s0 = TrieState { tsPatterns = M.empty
                   , tsCache = M.empty
                   }

trieFromState :: e -> TrieM e (ByteTrie e)
trieFromState defElt = do
  pats <- St.gets tsPatterns
  buildTableLevel defElt pats 0

buildTableLevel :: e
                -- ^ Default element
                -> M.Map Pattern e
                -- ^ Remaining valid patterns
                -> Int
                -- ^ Byte index we are computing
                -> TrieM e (ByteTrie e)
buildTableLevel defElt patterns byteIndex = do
  cache <- St.gets tsCache
  let key = (byteIndex, S.fromList (M.keys patterns))
  case M.lookup key cache of
    Just bt -> return bt
    Nothing -> do
      payloads <- T.traverse (makePayload defElt patterns byteIndex) byteValues
      let bt = ByteTrie $ A.array (0, maxWord) payloads
      St.modify' $ \s -> s { tsCache = M.insert key bt (tsCache s) }
      return bt
  where
    maxWord :: Word8
    maxWord = maxBound
    byteValues = [0 .. maxWord]

makePayload :: e
            -- ^ Default element
            -> M.Map Pattern e
            -- ^ Valid patterns at this point in the trie
            -> Int
            -- ^ Byte index we are computing
            -> Word8
            -- ^ The byte we are matching patterns against
            -> TrieM e (Word8, Payload e)
makePayload defElt patterns byteIndex byte =
  case M.toList matchingPatterns of
    [] -> return (byte, Element defElt)
    [(_, elt)] -> return (byte, Element elt)
    _ | all ((> (byteIndex + 1)) . patternBytes) (M.keys matchingPatterns) -> do
        t <- buildTableLevel defElt matchingPatterns (byteIndex + 1)
        return (byte, NextTable t)
      | otherwise ->
        E.throwError (OverlappingBitPattern (map fst (M.toList matchingPatterns)))
  where
    matchingPatterns = M.filterWithKey (patternMatches byteIndex byte) patterns

patternMatches :: Int -> Word8 -> Pattern -> e -> Bool
patternMatches byteIndex byte (Pattern { requiredMask = req, trueMask = true }) _ =
  (byte .&. patRequireByte) == patTrueByte
  where
    patRequireByte = req `BS.index` byteIndex
    patTrueByte = true `BS.index` byteIndex

{-

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

-- | Assert a mapping from a bit pattern to a value.
--
-- The bit pattern must have a length that is a multiple of 8.  This
-- function can error out with a pure error ('TrieError') if an
-- overlapping bit pattern is asserted.
assertMapping :: BS.ByteString -> BS.ByteString -> a -> TrieM a ()
assertMapping patReq patTrue val
  | BS.length patReq /= BS.length patTrue || BS.null patReq =
    E.throwError (InvalidPatternLength pat)
  | otherwise = do
      pats <- St.gets tsPatterns
      case M.lookup pat pats of
        Just xist -> E.throwError (OverlappingBitPattern [pat])
        Nothing ->
          St.modify' $ \s -> s { tsPatterns = M.insert pat val (tsPatterns s) }
  where
    pat = Pattern patReq patTrue
