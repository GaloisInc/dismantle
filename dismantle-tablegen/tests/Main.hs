module Main ( main ) where

import qualified Test.Tasty as T

import Parser
import Trie

main :: IO ()
main = do
  tests <- sequence [ parserTests, trieTests ]
  T.defaultMain $ T.testGroup "Dismantle" tests

