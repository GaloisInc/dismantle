{-# LANGUAGE OverloadedStrings #-}
module Main ( main ) where

import Data.Char (isSpace)
import qualified Data.List as L
import qualified Test.Tasty as T
import qualified Data.Text.Lazy as TL
import Data.Monoid ((<>))
import qualified Text.PrettyPrint.HughesPJClass as PP
import Data.Word (Word64)

import Dismantle.Testing
import Dismantle.Testing.ParserTests (parserTests)
import qualified Dismantle.Testing.Regex as RE

import qualified Dismantle.Thumb as Thumb
import qualified Dismantle.Thumb.ISA as Thumb

ignored :: [(FilePath, [Word64])]
ignored =
    [
    ]

thumb :: ArchTestConfig
thumb = ATC { testingISA = Thumb.isa
            , disassemble = Thumb.disassembleInstruction
            , assemble = Thumb.assembleInstruction
            , prettyPrint = Thumb.ppInstruction
            , expectFailure = Nothing
            , instructionFilter = ((/= FullWord) . insnLayout)
            , skipPrettyCheck = Just skipPretty
            , ignoreAddresses = ignored
            , normalizePretty = normalize
            , comparePretty = Nothing
            , customObjdumpArgs = []
            }

main :: IO ()
main = do
  tg <- binaryTestSuite thumb "tests/bin"
  pt <- parserTests
  T.defaultMain $ T.testGroup "dismantle-thumb" [tg, pt]

remove :: TL.Text -> TL.Text -> TL.Text
remove needle s =
    let (h, t) = TL.breakOn ".w" s
    in if needle `TL.isPrefixOf` t
       then h <> (TL.drop (TL.length needle) t)
       else h

normalize :: TL.Text -> TL.Text
normalize =
    -- Then, because objdump and tablegen don't always agree on when to
    -- append the ".w" instruction name suffix, we strip it. This means
    -- we at least get to test that operand pretty printing works in
    -- those cases rather than ignoring the entire instruction.
    remove ".w" .
    -- Then remove whitespace
    TL.filter (not . isSpace) .
    -- Remove square brackets and "#"
    TL.filter (flip notElem ("[]#"::String)) .
    -- First, trim any trailing comments
    (fst . TL.breakOn ";")

rx :: String -> RE.Regex
rx s =
  case RE.mkRegex s of
    Left e -> error ("Invalid regex: <<" ++ s ++ ">> because: " ++ e)
    Right r -> r

skipPretty :: RE.Regex
skipPretty = rx (L.intercalate "|" rxes)
  where
    rxes = others <> (matchInstruction <$> skipped)

    others = [
             -- We need to ignore these instructions when they mention
             -- the PC since objdump disassembles those as "add" etc.
             -- but we disassemble them as the (admittedly nicer) "adr".
               "add[[:space:]]..,[[:space:]]pc"
             , "sub[[:space:]]..,[[:space:]]pc"

             -- Any objdump output line with the word "illegal"
             -- (usually "illegal reg") is definitely a line we should
             -- ignore. It's probably a data byte sequence being
             -- parsed as an instruction.
             , "illegal"
             ]

    skipped = [
              -- We ignore branching instructions because objdump
              -- resolves the branch targets and we can't, so we can
              -- never match objdump's output.
                "bls"
              , "bl"
              , "b"
              , "b.n"
              , "cb"

              , "mrs"
              , "msr"
              , "cpsie"

              -- These get represented as RSBS (see ARM ARM, DDI
              -- 0406C.b, A8.8.118 NEG)
              , "negs?"

              -- These get represented as load/store multiple with sp
              -- mutation; push and pop are not even mentioned in the
              -- Tgen data.
              , "push"
              , "pop"

              -- Objdump knows about these but Tgen doesn't.
              , "cfldr64"
              , "cfldr32"
              , "cfstrs"

              -- These instructions are ones that the ISA specifically
              -- ignores right now.
              , "fldmdbx"
              , "vstr"

              -- These are equivalent to MOV but we don't format
              -- them that way. (See the ARM ARM, A8.8.105 MOV
              -- (shifted register), table: MOV (shifted register)
              -- equivalences). These are canonical forms, but they're
              -- marked as pseudo-instructions in the Tgen and we use
              -- the MOV representation while objdump uses the ones
              -- below.
              , "asrs?"
              , "lsls?"
              , "lsrs?"
              , "rors?"
              , "rrxs?"

              -- Ignored because the Tgen format string uses "stm" for
              -- this instruction and objdump uses "stmia".
              , "stmia"

              -- Ignored because the Tgen pretty-print format string has
              -- mistakes.
              , "pkhtb"

              -- Ignored because the Tgen format string uses "ldm" for
              -- this instruction and objdump uses "ldmfd".
              , "ldmfd"
              , "ldmia"

              -- Ignored because the Tgen format string for these
              -- reference an implied operand that isn't mentioned
              -- in the bit patterns, so it's impossible for us to
              -- pretty-print these.
              , "ldrdeq"
              , "strdeq"
              , "mrc"
              , "mcr2"
              , "mrc2"
              , "strd"
              , "strb"
              , "ldrd"

              -- Ignored because we don't have enough context to format
              -- the arguments correctly.
              , "stcl"
              , "ldrsht"
              , "ldcl"
              , "mcr"
              , "blx"
              , "cdp"
              , "stc"
              , "stc2l"

              -- We show nop as "mov r0, r0"
              , "nop"

              -- Tablegen format string has an undefined variable
              -- for this one (tMUL). We could ignore that, but it
              -- still leaves a trailing comma in the pretty-printed
              -- instruction that would still fail to match the objdump
              -- output.
              , "muls"
              , "mulcs"

              -- Ignore instructions where objdump used a preceding "it"
              -- instruction to get condition hints about subsequent
              -- instructions. Those get pretty-printed by objdump as
              -- ARM-style instructions with predication suffixes and
              -- that isn't legal, so we can safely ignore those in
              -- thiese Thumb tests.
              , "mov" <> conditions
              , "mul" <> conditions
              , "utxb" <> conditions
              , "add" <> conditions
              , "sub" <> conditions
              , "uxtb" <> conditions
              , "rsb" <> conditions
              , "cmp" <> conditions
              , "ldrb" <> conditions
              , "ldr" <> conditions
              , "orr" <> conditions
              , "str" <> conditions
              , "ubfx" <> conditions
              , "and" <> conditions
              , "strh" <> conditions
              , "mvn" <> conditions
              , "ldrsb" <> conditions
              , "ldrh" <> conditions
              , "sdiv" <> conditions
              , "umull" <> conditions
              , "eor" <> conditions
              , "udiv" <> conditions
              , "addw" <> conditions
              , "clz" <> conditions
              , "uxth" <> conditions
              , "sxth" <> conditions
              , "rev" <> conditions
              , "movw" <> conditions
              , "teq" <> conditions
              , "subs" <> conditions <> ".w"
              , "rsbs" <> conditions
              , "cmn" <> conditions <> ".w"
              , "orrs" <> conditions <> ".w"
              , "mvns" <> conditions <> ".w"
              , "ands" <> conditions <> ".w"
              , "mls" <> conditions
              , "orn" <> conditions

              -- We can't pretty-print VFP instructions
              , "vmov"
              , "vmul"
              , "vldr"
              , "vadd"
              , "vpop"
              , "vpush"
              , "vmrs"
              , "vcmpe"
              , "vfma"
              , "vsub"
              , "vfnms"
              , "vnmul"
              , "vcvt"
              , "vneg"
              , "vldmdb"
              , "vstmdb"
              , "vldmia"
              , "vstmia"
              , "vfms"
              , "vcmp"
              , "vdiv"
              , "vabs"

              -- We render this as ADR
              , "addw.*pc.*"

              -- We render this as a shift instruction
              , "mov.*lsl.*"
              , "mov.*lsr.*"
              , "mov.*asr.*"
              , "mov.*ror.*"
              , "mov.*rrx.*"

              -- We can't decode "it" branching instructions because
              -- we need information from multiple operands at once to
              -- render the instruction correctly, but the Tablegen
              -- parser only lets us pretty-print at the operand level.
              -- There are many t/e condition flags that can follow
              -- the "it" instruction name, but rather than write out
              -- a regular expression to handle them all, I decided to
              -- just write out an expression to handle just the ones
              -- we've encountered. -JTD
              , "ite?"
              ]

    matchInstruction name = "(^[[:space:]]*" <> name <> conditions <> "?" <> suffix <> "[[:space:]]?)"

    conditions = "(" <> (concat $ L.intersperse "|"
                  (PP.render <$> PP.pPrint <$> Thumb.mkPred <$> [0..13])) <> ")"
    suffix = "(.n)?"
