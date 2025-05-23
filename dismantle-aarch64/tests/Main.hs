{-# LANGUAGE OverloadedStrings #-}
module Main ( main ) where

import Data.Char (isSpace)
import qualified Data.List as L
import qualified Test.Tasty as T
import qualified Data.Text.Lazy as TL
import Data.Word (Word64)
import Data.Monoid ((<>))
import qualified Text.PrettyPrint.HughesPJClass as PP

import Dismantle.Testing
import Dismantle.Testing.ParserTests (parserTests)
import qualified Dismantle.Testing.Regex as RE

import qualified Dismantle.ARM as ARM
import qualified Dismantle.AArch64 as AArch64
import qualified Dismantle.AArch64.ISA as AArch64

ignored :: [(FilePath, [Word64])]
ignored =
    [
    ]

aarch64 :: ArchTestConfig
aarch64 = ATC { testingISA = AArch64.isa
              , disassemble = AArch64.disassembleInstruction
              , assemble = AArch64.assembleInstruction
              , prettyPrint = AArch64.ppInstruction
              , expectFailure = Just expectedFailures
              , skipPrettyCheck = Just skipPretty
              , customObjdumpArgs =
                  [("tests/bin/xen-4.6-arm64", ["-b", "binary", "-m", "aarch64", "-D"])]
              , ignoreAddresses = ignored
              , normalizePretty = normalize
              , comparePretty = Nothing
              , instructionFilter = const True
              }

main :: IO ()
main = do
  tg <- binaryTestSuite aarch64 "tests/bin"
  pt <- parserTests
  T.defaultMain $ T.testGroup "dismantle-aarch64" [tg, pt]

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

rx :: String -> RE.Regex
rx s =
  case RE.mkRegex s of
    Left e -> error ("Invalid regex <<" ++ s ++ ">> because: " ++ e)
    Right r -> r

skipPretty :: RE.Regex
skipPretty = rx (L.intercalate "|" rxes)
  where
    rxes = others

    others = [ "add[[:space:]]..,[[:space:]]pc"
             , "sub[[:space:]]..,[[:space:]]pc"

             -- We reassemble "mov rN, sp" as "add rN, sp, #0x0" and
             -- similarly for "cmp", "mov", and other instructions
             -- with alternative forms. The resulting assemblies are
             -- equivalent but we have to ignore the pretty prints
             -- because we don't have enough context to represent them
             -- the way objdump does.
             , "^mov"
             , "^cmp"
             , "^mul"
             , "^lsl"
             , "^nop"
             , "^sxtw"

             -- Aliases for UBFM
             , "ubfx"
             , "uxtb"
             , "uxth"

             -- Alias for CSINC
             , "cinc"

             -- Aliases for SBFM
             , "asr"
             , "sbfx"
             , "sxtb"
             , "sxth"

             -- Alias for umaddl
             , "umull"

             -- Alias for saddl
             , "smull"

             -- Alias for ORN
             , "mvn"

             -- Alias for BFM
             , "bfi"
             , "bfxil"

             -- Alias for CSINC
             , "cset"

             -- Alias for ADDS
             , "cmn"

             -- Alias for SUB
             , "neg"

             -- LSR is represented as UBFM
             , "lsr"

             -- ROR is an alias for EXTR
             , "ror"

             -- Floating-point immediates are hard to pretty-print
             , "fmov"

             -- Alias for ANDS
             , "tst"

             -- Have you even seen how many possible values there are
             -- for system register operands?
             , "mrs"
             , "msr"
             , "dc"

             -- Some variants of STR have broken formatting strings in
             -- tgen
             , "str"

             -- Instructions with a PC-relative offset / label that we
             -- can't resolve
             , "ldr"
             , "bl?[[:space:]]"
             , "b." <> conditions <> "[[:space:]]"
             , "adrp?"
             , "cbn?z"
             , "tbn?z"

             -- We decode RET as RET x30. That's technically accurate
             -- since an absent RET argument defaults to x30 (see
             -- C5.6.148) but objdump omits the x30 argument and we
             -- can't because our operand pretty printer doesn't know
             -- we're rendering a RET.
             , "ret"

             -- Impossible to pretty print because the format strings
             -- are more than we currently know how to handle
             , "addp"
             , "dup"
             , "add.*v"
             ]

    conditions = "(" <> (concat $ L.intersperse "|"
                  (PP.render <$> PP.pPrint <$> ARM.mkPred <$> [0..13])) <> ")?"

expectedFailures :: RE.Regex
expectedFailures = rx (L.intercalate "|" rxes)
  where
    rxes = [ "^[[:space:]]*mvnpl"
           ]
