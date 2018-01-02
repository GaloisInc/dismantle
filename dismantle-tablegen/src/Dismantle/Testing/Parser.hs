{-# LANGUAGE TupleSections #-}
module Dismantle.Testing.Parser (
  objdumpParser,
  Disassembly(..),
  Section(..),
  Instruction(..),
  InstructionLayout(..),
  Parser
  ) where

import Control.Applicative
import Control.Monad ( replicateM, replicateM_, void )
import qualified Data.ByteString.Lazy as LBS
import qualified Data.List.NonEmpty as NL
import Data.Monoid ((<>))
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

data Instruction =
    Instruction { insnAddress :: Word64
                , insnBytes :: LBS.ByteString
                , insnText :: TL.Text
                , insnLayout :: InstructionLayout
                }
                deriving (Show)

data InstructionLayout =
    FullWord
    -- ^ The instruction bytes are to be treated as a contiguous
    -- four-byte word.
    | HalfWord
    -- ^ The instruction bytes are to be treated as a single two-byte
    -- half word.
    | HalfWordPair
    -- ^ The instruction bytes are to be treated as a pair of two-byte
    -- half words.
    | SingleByte
    -- ^ Used only by .byte entries.
    deriving (Eq, Read, Show)

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
  tryOne [ parseAddressOutOfBounds
         , parseEllipses
         , parseFunctionHeading
         , parseJunkData
         , parseInstruction
         ]

parseJunkData :: Parser (Maybe Instruction)
parseJunkData = do
  P.skipMany (P.char ' ')
  void parseAddress
  void $ P.char ':'
  P.space

  -- We have seen character data lines like these and want to ignore
  -- them:
  --
  -- 80800000:       b8 00 00 ea 14 f0 9f e5 14 f0 9f e5 14 f0 9f e5     <char data>
  -- 8084f118:       20494249 54535953 00020005 00000018     <char data>
  let parseWord = void parseHalfWord >> void parseHalfWord
  tryOne [ replicateM_ 15 (parseByte >> void (P.char ' ')) >> void parseByte
         , replicateM_ 3 (parseWord >> void (P.char ' ')) >> void parseWord
         , parseWord >> void (P.char ' ') >> void parseWord
         ]

  void $ P.manyTill P.anyChar P.eol
  void $ P.optional P.eol

  return Nothing

parseEllipses :: Parser (Maybe a)
parseEllipses = tryOne [ P.space >> P.string (TL.pack "...") >> P.eol >> P.eol >> return Nothing
                       , P.space >> P.string (TL.pack "...") >> P.eol >> return Nothing
                       ]

parseInstructionBytes :: Parser ([Word8], InstructionLayout)
parseInstructionBytes =
  tryOne [ parseThumbFull
         , parsePowerPCFull
         , parseARMFull
         , parseThumbHalf
         , (, SingleByte) <$> pure <$> parseByte
         ]

parseHalfWord :: Parser [Word8]
parseHalfWord = replicateM 2 parseByte

parseThumbFull :: Parser ([Word8], InstructionLayout)
parseThumbFull = do
    w1 <- parseHalfWord
    void $ P.char ' '
    w2 <- parseHalfWord
    return (w1 <> w2, HalfWordPair)

parsePowerPCFull :: Parser ([Word8], InstructionLayout)
parsePowerPCFull = do
    b1 <- parseByte
    void $ P.char ' '
    b2 <- parseByte
    void $ P.char ' '
    b3 <- parseByte
    void $ P.char ' '
    b4 <- parseByte
    return ([b1, b2, b3, b4], FullWord)

parseARMFull :: Parser ([Word8], InstructionLayout)
parseARMFull =
    (, FullWord) <$> replicateM 4 parseByte

parseThumbHalf :: Parser ([Word8], InstructionLayout)
parseThumbHalf =
    (, HalfWord) <$> parseHalfWord

-- Objdump sometimes emits these lines for thumb disassemblies.
parseAddressOutOfBounds :: Parser (Maybe Instruction)
parseAddressOutOfBounds = do
  P.space
  void parseAddress
  void $ P.char ':'
  P.space
  void $ P.string $ TL.pack "Address 0x"
  void parseAddress
  void $ P.string $ TL.pack " is out of bounds."
  void $ replicateM 3 P.eol
  return Nothing

parseInstruction :: Parser (Maybe Instruction)
parseInstruction = do
  P.skipMany (P.char ' ')
  addr <- parseAddress
  _ <- P.char ':'
  P.space
  (bytes, layout) <- parseInstructionBytes
  P.space
  txt <- TL.pack <$> P.manyTill P.anyChar P.eol
  _ <- P.optional P.eol
  case isDataDirective txt || isUndefinedInstruction txt of
    True -> return Nothing
    False ->
      return $ Just Instruction { insnAddress = addr
                                , insnBytes = LBS.pack bytes
                                , insnText = txt
                                , insnLayout = layout
                                }

isDataDirective :: TL.Text -> Bool
isDataDirective t =  or [ TL.pack ".long" `TL.isInfixOf` t
                        , TL.pack ".word" `TL.isInfixOf` t
                        , TL.pack ".short" `TL.isInfixOf` t
                        , TL.pack ".byte" `TL.isInfixOf` t
                        ]

isUndefinedInstruction :: TL.Text -> Bool
isUndefinedInstruction t =
    TL.pack "UNDEFINED" `TL.isInfixOf` t ||
    TL.pack "undefined" `TL.isInfixOf` t

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
