module Dismantle.Tablegen.ByteTrie (
  ByteTrie,
  mkTrie,
  lookupByte,
  TrieError(..)
  ) where

import qualified Control.Monad.State.Strict as St
import qualified Control.Monad.Except as E
import qualified Data.Array as A
import Data.Word ( Word8 )

data Payload a = NextTable (ByteTrie a)
               | Element a

-- | A data type mapping sequences of bytes to elements of type @a@
data ByteTrie a = ByteTrie (A.Array Word8 (Payload a))

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

data TrieError = TrieError
data TrieState e = TrieState

type TrieM e a = St.StateT (TrieState e) (E.Except TrieError) a

-- | Construct a 'ByteTrie' through a monadic assertion-oriented interface.
--
-- Any bit pattern not covered by an explicit assertion will default
-- to the undefined parse value.
mkTrie :: a
       -- ^ The value of an undefined parse
       -> TrieM a ()
       -- ^ The assertions composing the trie
       -> Either TrieError (ByteTrie a)
mkTrie defElt act =
  E.runExcept (St.execStateT act TrieState >>= trieFromState defElt)

trieFromState :: a -> TrieState a -> E.Except TrieError (ByteTrie a)
trieFromState = undefined
