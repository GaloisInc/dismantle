module Parser ( parserTests ) where

import qualified Data.Text.Lazy.IO as TL
import qualified Test.Tasty as T
import qualified Test.Tasty.HUnit as T

import qualified System.FilePath.Glob as G

import qualified Dismantle.Tablegen as D

parserTests :: IO T.TestTree
parserTests = do
  paths <- G.namesMatching "data/*.tgen"
  return (T.testGroup "Parser" (map mkTest paths))

mkTest :: FilePath -> T.TestTree
mkTest p = T.testCase p $ do
  t <- TL.readFile p
  case D.parseTablegen p t of
    Left err -> T.assertFailure (show err)
    Right _r -> do
      return ()
