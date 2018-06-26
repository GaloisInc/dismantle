module Main ( main ) where

import qualified Test.Tasty as T

import Dismantle.Testing.ParserTests ( parserTests )
import Operands ( operandTests )
import Trie ( trieTests )

main :: IO ()
main = do
  tests <- sequence [ parserTests, trieTests ]
  T.defaultMain $ T.testGroup "Dismantle" (operandTests:tests)

