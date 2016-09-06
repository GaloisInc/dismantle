module Main ( main ) where

import Control.Applicative
import Data.Monoid
import qualified Options.Applicative as O
import qualified Data.Text.Lazy.IO as TL
import qualified System.Exit as E

import Prelude

import qualified Dismantle.Tablegen as D

data Options = Options { inputFile :: FilePath
                       }
               deriving (Show)

options :: O.Parser Options
options = Options <$> O.strArgument ( O.metavar "FILE"
                                      <> O.help "The tablegen output to parse"
                                      )

main :: IO ()
main = O.execParser opts >>= dump
  where
    opts = O.info (O.helper <*> options) O.fullDesc

dump :: Options -> IO ()
dump o = do
  t <- TL.readFile (inputFile o)
  case D.parseTablegen (inputFile o) t of
    Left err -> do
      print err
      E.exitFailure
    Right r -> print r
