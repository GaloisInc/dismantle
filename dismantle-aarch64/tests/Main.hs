{-# LANGUAGE OverloadedStrings #-}
module Main ( main ) where

import Data.Char (isSpace)
import qualified Data.List as L
import qualified Test.Tasty as T
import qualified Data.Text.Lazy as TL
import qualified Text.RE.TDFA as RE
import Data.Monoid ((<>))
import qualified Text.PrettyPrint.HughesPJClass as PP
import Data.Word (Word64)

import Dismantle.Testing

import qualified Dismantle.AArch64 as AArch64

ignored :: [(FilePath, [Word64])]
ignored =
    [
    ]

aarch64 :: ArchTestConfig
aarch64 = ATC { archName = "aarch64"
              , disassemble = AArch64.disassembleInstruction
              , assemble = AArch64.assembleInstruction
              , prettyPrint = AArch64.ppInstruction
              , expectFailure = Just expectedFailures
              , skipPrettyCheck = Just skipPretty
              , ignoreAddresses = ignored
              , normalizePretty = normalize
              , instructionFilter = const True
              }

main :: IO ()
main = do
  tg <- binaryTestSuite aarch64 "tests/bin"
  T.defaultMain tg

normalize :: TL.Text -> TL.Text
normalize =
    -- Then remove whitespace
    TL.filter (not . isSpace) .
    -- Remove square brackets and "#"
    TL.filter (flip notElem ("[]#"::String)) .
    -- First, trim any trailing comments
    (fst . TL.breakOn ";")

rx :: String -> RE.RE
rx s =
  case RE.compileRegex s of
    Nothing -> error ("Invalid regex: " ++ s)
    Just r -> r

skipPretty :: RE.RE
skipPretty = rx (L.intercalate "|" rxes)
  where
    rxes = others

    others = [ "add[[:space:]]..,[[:space:]]pc"
             , "sub[[:space:]]..,[[:space:]]pc"
             ]

expectedFailures :: RE.RE
expectedFailures = rx (L.intercalate "|" rxes)
  where
    rxes = [ "^[[:space:]]*mvnpl"
           ]
