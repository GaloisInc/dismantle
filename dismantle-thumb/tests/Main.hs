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

import qualified Dismantle.Thumb as Thumb

ignored :: [(FilePath, [Word64])]
ignored =
    [
    ]

thumb :: ArchTestConfig
thumb = ATC { archName = "thumb"
            , disassemble = Thumb.disassembleInstruction
            , assemble = Thumb.assembleInstruction
            , prettyPrint = Thumb.ppInstruction
            , expectFailure = Just expectedFailures
            , instructionFilter = ((/= FullWord) . insnLayout)
            , skipPrettyCheck = Just skipPretty
            , ignoreAddresses = ignored
            , normalizePretty = normalize
            }

main :: IO ()
main = do
  tg <- binaryTestSuite thumb "tests/bin"
  T.defaultMain tg

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

rx :: String -> RE.RE
rx s =
  case RE.compileRegex s of
    Nothing -> error ("Invalid regex: " ++ s)
    Just r -> r

skipPretty :: RE.RE
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

              -- Ignore instructions where objdump used a preceding "it"
              -- instruction to get condition hints about subsequent
              -- instructions. Those get pretty-printed by objdump as
              -- ARM-style instructions with predication suffixes and
              -- that isn't legal, so we can safely ignore those in
              -- thiese Thumb tests.
              , "mov(eq|ne|le|ge|lt|gt|hi|ls)"

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

    matchInstruction name = "(^[[:space:]]*" <> name <> conditions <> suffix <> "[[:space:]]?)"

    conditions = "(" <> (concat $ L.intersperse "|"
                  (PP.render <$> PP.pPrint <$> Thumb.mkPred <$> [0..13])) <> ")?"
    suffix = "(.n)?"

expectedFailures :: RE.RE
expectedFailures = rx (L.intercalate "|" rxes)
  where
    rxes = [ -- The tablegen data for MVN is currently incorrect
             -- w.r.t. unpredictable bits. The only variant of
             -- MVN that we've encountered that actually sets
             -- some unpredictable bits is 'mvnpl'. A bug has been
             -- submitted upstream. In the mean time, for details, see
             -- https://bugs.llvm.org/show_bug.cgi?id=33011
             "^[[:space:]]*mvnpl"
           ]
