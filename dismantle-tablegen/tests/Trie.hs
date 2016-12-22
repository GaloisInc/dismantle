module Trie ( trieTests ) where

import qualified Test.Tasty as T
import qualified Test.Tasty.HUnit as T

import qualified Dismantle.Tablegen.ByteTrie as BT

trieTests :: T.TestTree
trieTests = T.testGroup "Trie" []
