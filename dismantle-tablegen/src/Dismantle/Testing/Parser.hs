module Dismantle.Testing.Parser (
  objdumpParser,
  Disassembly(..),
  Section(..),
  Instruction(..),
  Parser
  ) where

import Control.Applicative
import Control.Monad ( replicateM, replicateM_, void )
import qualified Data.ByteString.Lazy as LBS
import qualified Data.List.NonEmpty as NL
import Data.Maybe ( catMaybes )
import qualified Data.Text.Lazy as TL
import Data.Word ( Word8, Word64 )
import qualified Text.Megaparsec as P
import qualified Text.Megaparsec.Char as P
import Text.Read ( readMaybe )

import Prelude

type Parser = P.Parsec (P.ErrorItem Char) TL.Text

data Disassembly = Disassembly { sections :: [Section] }
  deriving (Show)

data Section = Section { sectionName :: TL.Text
                       , instructions :: [Instruction]
                       }
             deriving (Show)

data Instruction = Instruction { insnAddress :: Word64
                               , insnBytes :: LBS.ByteString
                               , insnText :: TL.Text
                               }
                 deriving (Show)

objdumpParser :: Parser Disassembly
objdumpParser =
  consumeHeader *> (Disassembly <$> P.sepEndBy parseSection P.space) <* P.eof

consumeLine :: Parser ()
consumeLine = void (P.manyTill P.anyChar P.eol)

consumeHeader :: Parser ()
consumeHeader = replicateM_ 2 consumeLine >> P.space

parseSectionName :: Parser TL.Text
parseSectionName = TL.pack <$> P.some sectionNameChar

tryOne :: [Parser a] -> Parser a
tryOne = P.choice . map P.try

sectionNameChar :: Parser Char
sectionNameChar = tryOne [ P.alphaNumChar
                         , P.oneOf ['.', '_']
                         ]

-- | Characters that can appear in symbol names, including symbol
-- names that are an offset from another symbol.
symbolNameChar :: Parser Char
symbolNameChar = tryOne [ P.alphaNumChar
                        , P.oneOf ['@', '-', '_', '.']
                        ]

parseSection :: Parser Section
parseSection = do
  _ <- P.string (TL.pack "Disassembly of section ")
  sn <- parseSectionName
  _ <- P.char ':'
  _ <- P.eol
  consumeLine
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
parseEllipses = tryOne [ P.space >> P.string (TL.pack "...") >> P.eol >> P.eol >> return Nothing
                       , P.space >> P.string (TL.pack "...") >> P.eol >> return Nothing
                       ]

parseInstruction :: Parser (Maybe Instruction)
parseInstruction = do
  P.skipMany (P.char ' ')
  addr <- parseAddress
  _ <- P.char ':'
  P.space
  bytes <- P.try (P.endBy1 parseByte (P.char ' ')) <|> replicateM 4 parseByte
  P.space
  txt <- TL.pack <$> P.manyTill P.anyChar P.eol
  _ <- P.optional P.eol
  case isDataDirective txt || isUndefinedInstruction txt of
    True -> return Nothing
    False ->
      return $ Just Instruction { insnAddress = addr
                                , insnBytes = LBS.pack bytes
                                , insnText = txt
                                }

isDataDirective :: TL.Text -> Bool
isDataDirective t =  or [ TL.pack ".long" `TL.isInfixOf` t
                        , TL.pack ".word" `TL.isInfixOf` t
                        ]

isUndefinedInstruction :: TL.Text -> Bool
isUndefinedInstruction t =  TL.pack "UNDEFINED" `TL.isInfixOf` t

-- | These are the markers for where symbols point in the decoded
-- stream.  Even stripped binaries tend to have at least a few (at the
-- start of each section).
parseFunctionHeading :: Parser (Maybe a)
parseFunctionHeading = do
  _ <- parseAddress
  _ <- P.string (TL.pack " <")
  _ <- P.some symbolNameChar
  _ <- P.string (TL.pack ">:")
  _ <- P.eol
  return Nothing

parseAddress :: Parser Word64
parseAddress = do
  digits@(d:rest) <- P.some P.hexDigitChar
  case readMaybe ('0' : 'x' : digits) of
    Just w -> return w
    Nothing -> P.unexpected (P.Tokens (d NL.:| rest))

parseByte :: Parser Word8
parseByte = do
  d1 <- P.hexDigitChar
  d2 <- P.hexDigitChar
  case readMaybe ('0' : 'x' : d1 : d2 : []) of
    Just b -> return b
    Nothing -> P.unexpected (P.Tokens (d1 NL.:| [d2]))
