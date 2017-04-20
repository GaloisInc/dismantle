module Main ( main ) where

import qualified Test.Tasty as T

import Operands ( operandTests )
import Parser ( parserTests )
import Trie ( trieTests )

main :: IO ()
main = do
  tests <- sequence [ parserTests, trieTests ]
  T.defaultMain $ T.testGroup "Dismantle" (operandTests:tests)

