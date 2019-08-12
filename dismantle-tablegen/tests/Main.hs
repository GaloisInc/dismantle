module Main ( main ) where

import qualified Test.Tasty as T

import Dismantle.Testing.ParserTests ( parserTests )
import Operands ( operandTests )
import Trie ( trieTests )
import WordIndexed ( wordIndexedTests )

main :: IO ()
main = do
  tests <- sequence [ parserTests, trieTests, wordIndexedTests ]
  T.defaultMain $ T.testGroup "Dismantle" (operandTests:tests)

