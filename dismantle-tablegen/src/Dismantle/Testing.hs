{-# LANGUAGE TupleSections #-}
-- | Provide some tools for testing disassemblers
module Dismantle.Testing (
  Disassembly(..),
  Section(..),
  Instruction(..),
  Parser,
  withInstructions,
  withDisassembledFile
  ) where

import Data.Word (Word64)
import qualified Data.ByteString.Lazy as LBS
import qualified Data.Text.Lazy as T
import qualified Data.Text.Lazy.IO as T
import qualified Data.Traversable as T
import System.FilePath.Glob ( namesMatching )
import System.FilePath ( (</>) )
import qualified System.Process as Proc
import qualified Text.Megaparsec as P
import System.IO (hClose)

import Prelude

type Parser = P.Parsec P.Dec T.Text

data Disassembly = Disassembly { sections :: [Section] }
  deriving (Show)

data Section = Section { sectionName :: T.Text
                       , instructions :: [Instruction]
                       }
             deriving (Show)

data Instruction = Instruction { insnAddress :: Word64
                               , insnBytes :: LBS.ByteString
                               , insnText :: T.Text
                               }
                 deriving (Show)

-- | Convert a directory of executables into a list of data, where
-- each data item is constructed by a callback called on one
-- instruction disassembled by objdump.
withInstructions :: Parser Disassembly
                 -- ^ The parser to use to parse the objdump output
                 -> FilePath
                 -- ^ A directory containing executables (that can be objdumped)
                 -> (Word64 -> LBS.ByteString -> T.Text -> a)
                 -- ^ Turn a disassembled instruction into data
                 -> IO [(FilePath, [a])]
withInstructions parser dir con = do
  files <- namesMatching (dir </> "*")
  mapM disassembleFile files
  where
    disassembleFile f = do
      insns <- withDisassembledFile parser f $ \d -> do
        T.forM (concatMap instructions (sections d)) $ \i ->
            return (con (insnAddress i) (insnBytes i) (insnText i))
      return (f, insns)

withDisassembledFile :: Parser Disassembly -> FilePath -> (Disassembly -> IO a) -> IO a
withDisassembledFile parser f k = do
  (_, Just hout, _, ph) <- Proc.createProcess p1
  t <- T.hGetContents hout
  case P.runParser parser f t of
    Left err -> do
      hClose hout
      _ <- Proc.waitForProcess ph
      error $ P.parseErrorPretty err
    Right d -> do
      res <- k d
      hClose hout
      _ <- Proc.waitForProcess ph
      return res
  where
    p0 = Proc.proc "objdump" ["-d", f]
    p1 = p0 { Proc.std_out = Proc.CreatePipe
            }
