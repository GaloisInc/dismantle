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
    -- Remove zero offsets that we can't properly render
    TL.replace ", #0" "" .
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

             -- We reassemble "mov rN, sp" as "add rN, sp, #0x0". That's
             -- equivalent but we fail the pretty print check as a result.
             , "mov.*, sp"

             -- mov <reg>, <imm> gets disasssembled as "movz <reg>,
             -- <imm>[, shift]" (or "movn") but we lack sufficient
             -- information in the pretty printer to remove the "z" when
             -- there is no shift.
             , "mov[[:space:]]+[wx][[:digit:]]+,[[:space:]]#0x[0-9a-f]+[^,]"

             -- Instructions with a PC-relative offset / label that we
             -- can't resolve
             , "ldr.*<"
             , "b.*<"

             -- We decode RET as RET x30. That's technically accurate
             -- since an absent RET argument defaults to x30 (see
             -- C5.6.148) but objdump omits the x30 argument and we
             -- can't because our operand pretty printer doesn't know
             -- we're rendering a RET.
             , "ret"
             ]

expectedFailures :: RE.RE
expectedFailures = rx (L.intercalate "|" rxes)
  where
    rxes = [ "^[[:space:]]*mvnpl"
           ]
