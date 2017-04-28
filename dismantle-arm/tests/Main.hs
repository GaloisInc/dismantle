module Main ( main ) where

import qualified Data.ByteString.Lazy as LBS
import qualified Data.Text.Lazy as T
import Data.Word ( Word8, Word64 )
import Text.Read ( readMaybe )
import Control.Monad ( replicateM, replicateM_, void )
import Data.Maybe ( catMaybes )
import qualified Test.Tasty as T
import qualified Test.Tasty.HUnit as T
import qualified Text.Megaparsec as P
import qualified Data.List.NonEmpty as NL

import Dismantle.Testing

import qualified Dismantle.ARM as ARM

main :: IO ()
main = do
  testsByFile <- withInstructions p "tests/bin" mkTest
  let testCases = map (\(f, tests) -> T.testGroup f tests) testsByFile
  T.defaultMain (T.testGroup "ARM Tests" testCases)

mkTest :: Word64 -> LBS.ByteString -> T.Text -> T.TestTree
mkTest _addr bytes txt = T.testCase (T.unpack txt) $ do
  let (_consumed, minsn) = ARM.disassembleInstruction bytes
  case minsn of
    Nothing -> T.assertFailure ("Failed to disassemble " ++ show txt)
    Just i -> T.assertEqual "Reassembly" bytes (ARM.assembleInstruction i)

p :: Parser Disassembly
p =
  consumeHeader *> (Disassembly <$> P.manyTill parseSection P.eof)

consumeLine :: Parser ()
consumeLine = void (P.manyTill P.anyChar P.eol)

consumeHeader :: Parser ()
consumeHeader = replicateM_ 2 consumeLine >> P.space

parseSectionName :: Parser T.Text
parseSectionName = T.pack <$> P.some sectionNameChar

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
  _ <- P.string "Disassembly of section "
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
         , P.eol >> return Nothing
         ]

parseEllipses :: Parser (Maybe a)
parseEllipses = tryOne [ P.space >> P.string "..." >> P.eol >> return Nothing
                       ]

parseInstruction :: Parser (Maybe Instruction)
parseInstruction = do
  P.skipSome (P.char ' ')
  addr <- parseAddress
  _ <- P.char ':'
  P.space
  bytes <- replicateM 4 parseByte
  P.space
  txt <- T.pack <$> P.manyTill P.anyChar P.eol
  case isDataDirective txt of
    True -> return Nothing
    False ->
      return $ Just Instruction { insnAddress = addr
                                , insnBytes = LBS.pack bytes
                                , insnText = txt
                                }

isDataDirective :: T.Text -> Bool
isDataDirective t =  or [ T.pack ".word" `T.isInfixOf` t
                        ]

-- | These are the markers for where symbols point in the decoded
-- stream.  Even stripped binaries tend to have at least a few (at the
-- start of each section).
parseFunctionHeading :: Parser (Maybe a)
parseFunctionHeading = do
  _ <- parseAddress
  _ <- P.string " <"
  _ <- P.some symbolNameChar
  _ <- P.string ">:"
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
