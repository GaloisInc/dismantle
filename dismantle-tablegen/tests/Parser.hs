{-# LANGUAGE TupleSections #-}
module Parser ( parserTests ) where

import Control.DeepSeq ( deepseq )
import Control.Monad (when, forM)
import Data.List (delete)
import Data.Monoid ((<>))
import qualified Data.Text.IO as TS
import qualified Data.Text.Lazy as TL
import qualified Test.Tasty as T
import qualified Test.Tasty.HUnit as T
import qualified Dismantle.Testing.Regex as RE
import qualified System.FilePath.Glob as G
import System.Directory (canonicalizePath)
import System.Exit (die)
import System.FilePath (takeFileName)

import qualified Dismantle.Tablegen as D
import qualified Dismantle.Tablegen.Parser.Types as D

parserTests :: IO T.TestTree
parserTests = do
  tgenFiles <- findTgenFiles
  (T.testGroup "Parser") <$> mapM mkParserTestGroup tgenFiles

mkParserTestGroup :: (String, [FilePath]) -> IO T.TestTree
mkParserTestGroup (pkg, files) = do
  return (T.testGroup pkg (map mkTest files))

requireGlob :: String -> String -> IO [FilePath]
requireGlob ty pat = do
    names <- G.namesMatching pat
    paths <- mapM canonicalizePath (delete "../dismantle-coverage" names)
    when (null paths) $ do
        die $ "Error: could not find any " <> ty <> " matching " <> show pat
    return paths

-- Find all .tgen files in this package and sibling packages, assuming
-- these tests are being run from a dismantle-tablegen build directory
-- where sibling dismantle-* projects are in the same directory as
-- dismantle-tablegen.
--
-- Returns the tablegen files it found, paired with the path to the
-- project that provides them.
--
-- Dies if no tablegen source files could be found.
findTgenFiles :: IO [(FilePath, [FilePath])]
findTgenFiles = do
    projectPaths <- requireGlob "dismantle projects" "../dismantle-*"

    forM projectPaths $ \project -> do
        let tgenPattern = project <> "/data/*.tgen"
        (project,) <$> requireGlob "tgen files" tgenPattern

mkTest :: FilePath -> T.TestTree
mkTest p = T.testCase (takeFileName p) $ do
  t <- TS.readFile p
  let (Right re) = RE.mkRegex "^def "
  let expectedDefCount = RE.countMatches t re
  case D.parseTablegen p (TL.fromStrict t) of
    Left err ->
        let msg = "Error parsing " <> show p <> ": " <> err
        in T.assertFailure msg
    Right rs -> do
        rs `deepseq` return ()
        T.assertEqual ("Number of defs in " <> p) expectedDefCount (length (D.tblDefs rs))
