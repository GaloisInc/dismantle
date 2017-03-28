{-# LANGUAGE TupleSections #-}
-- | Provide some tools for testing disassemblers
module Dismantle.Testing (
  Disassembly(..),
  Section(..),
  Instruction(..),
  parseObjdump,
  withInstructions,
  withDisassembledFile
  ) where

import Control.Applicative
import Control.Monad ( replicateM_, void )
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as LBS
import qualified Data.List.NonEmpty as NL
import Data.Maybe ( catMaybes )
import qualified Data.Text.Lazy as T
import qualified Data.Text.Lazy.IO as T
import qualified Data.Traversable as T
import Data.Word ( Word8, Word64 )
import System.FilePath.Glob ( namesMatching )
import System.FilePath ( (</>) )
import qualified System.Process as Proc
import qualified Text.Megaparsec as P
import Text.Read ( readMaybe )

import Prelude

-- | Convert a directory of executables into a list of data, where
-- each data item is constructed by a callback called on one
-- instruction disassembled by objdump.
withInstructions :: FilePath
                 -- ^ A directory containing executables (that can be objdumped)
                 -> (Word64 -> LBS.ByteString -> T.Text -> a)
                 -- ^ Turn a disassembled instruction into data
                 -> IO [(FilePath, [a])]
withInstructions dir con = do
  files <- namesMatching (dir </> "*")
  mapM disassembleFile files
  where
    disassembleFile f = do
      insns <- withDisassembledFile f $ \d -> do
        T.forM (concatMap instructions (sections d)) $ \i -> return (con (insnAddress i) (insnBytes i) (insnText i))
      return (f, insns)

withDisassembledFile :: FilePath -> (Disassembly -> IO a) -> IO a
withDisassembledFile f k = do
  (_, Just hout, _, ph) <- Proc.createProcess p1
  t <- T.hGetContents hout
  case parseObjdump f t of
    Left err -> do
      _ <- Proc.waitForProcess ph
      error (show err)
    Right d -> do
      res <- k d
      _ <- Proc.waitForProcess ph
      return res
  where
    p0 = Proc.proc "objdump" ["-d", f]
    p1 = p0 { Proc.std_out = Proc.CreatePipe
            }

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

parseObjdump :: String
             -> T.Text
             -> Either (P.ParseError Char P.Dec) Disassembly
parseObjdump fname t = P.runParser p fname t

type Parser = P.Parsec P.Dec T.Text

p :: Parser Disassembly
p = do
  consumeHeader
  Disassembly <$> P.some parseSection

consumeLine :: Parser ()
consumeLine = void (P.manyTill P.anyChar P.eol)

consumeHeader :: Parser ()
consumeHeader = replicateM_ 3 consumeLine

parseSectionName :: Parser T.Text
parseSectionName = T.pack <$> P.some sectionNameChar

tryOne :: [Parser a] -> Parser a
tryOne = P.try . P.choice

sectionNameChar :: Parser Char
sectionNameChar = tryOne [ P.alphaNumChar
                         , P.oneOf ['.', '_']
                         ]

-- | Characters that can appear in symbol names, including symbol
-- names that are an offset from another symbol.
symbolNameChar :: Parser Char
symbolNameChar = tryOne [ P.alphaNumChar
                        , P.oneOf ['@', '-', '_']
                        ]

parseSection :: Parser Section
parseSection = do
  consumeLine
  _ <- P.string "Disassembly of section "
  sn <- parseSectionName
  _ <- P.char ':'
  _ <- P.eol
  consumeLine
  -- FIXME: insns has to be prepared for a line beginning with an
  -- address, but that actually serves as a label
  insns <- catMaybes <$> P.some tryParseInstruction
  return Section { sectionName = sn
                 , instructions = insns
                 }

tryParseInstruction :: Parser (Maybe Instruction)
tryParseInstruction =
  tryOne [ parseInstruction
         , parseEllipses
         , parseFunctionHeading
         ]

parseEllipses :: Parser (Maybe a)
parseEllipses = P.some (P.char ' ') >> P.string "..." >> P.eol >> return Nothing

parseInstruction :: Parser (Maybe Instruction)
parseInstruction = do
  addr <- parseAddress
  _ <- P.char ':'
  _ <- P.some (P.char ' ')
  bytes <- P.sepBy1 parseByte (P.char ' ')
  txt <- T.pack <$> P.manyTill P.anyChar P.eol
  return $ Just Instruction { insnAddress = addr
                            , insnBytes = LBS.pack bytes
                            , insnText = txt
                            }

-- | These are the markers for where symbols point in the decoded
-- stream.  Even stripped binaries tend to have at least a few (at the
-- start of each section).
parseFunctionHeading :: Parser (Maybe a)
parseFunctionHeading = do
  _ <- parseAddress
  _ <- P.string " <"
  _ <- P.some symbolNameChar
  _ <- P.string ">:"
  return Nothing

parseAddress :: Parser Word64
parseAddress = do
  digits@(d:rest) <- P.some P.hexDigitChar
  case readMaybe digits of
    Just w -> return w
    Nothing -> P.unexpected (P.Tokens (d NL.:| rest))

parseByte :: Parser Word8
parseByte = do
  d1 <- P.hexDigitChar
  d2 <- P.hexDigitChar
  case readMaybe (d1 : d2 : []) of
    Just b -> return b
    Nothing -> P.unexpected (P.Tokens (d1 NL.:| [d2]))
