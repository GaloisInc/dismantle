module Main ( main ) where

import qualified Test.Tasty as T

import Trie

main :: IO ()
main = T.defaultMain tests
  where
    tests = T.testGroup "Dismantle" [ trieTests ]
