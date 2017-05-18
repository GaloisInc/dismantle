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

import qualified Dismantle.ARM as ARM

ignored :: [(FilePath, [Word64])]
ignored =
    [ ("tests/bin/ls"
      , [ 0x1e198
        -- This address is ignored because objdump considers this byte
        -- sequence to be a 'tst' instruction, but that is incorrect.
        -- The ARM ARM specifies this as an MRS instruction. We parse it
        -- correctly.
        -- 1e198:       01010101        tsteq   r1, r1, lsl #2
        ]
      )
    ]

arm :: ArchTestConfig
arm = ATC { archName = "arm"
          , disassemble = ARM.disassembleInstruction
          , assemble = ARM.assembleInstruction
          , prettyPrint = ARM.ppInstruction
          , expectFailure = Just expectedFailures
          , skipPrettyCheck = Just (rx ".*")
          , ignoreAddresses = ignored
          , normalizePretty = normalize
          }

main :: IO ()
main = do
  tg <- binaryTestSuite arm "tests/bin"
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
    rxes = others <> (matchInstruction <$> skipped)

    others = [ -- We need to ignore "add" instructions when they mention
               -- the PC since objdump disassembles those as "add" but
               -- we disassemble them as the (admittedly nicer) "adr".
               "add[[:space:]]..,[[:space:]]pc"
             ]

    skipped = [
              -- We ignore branching instructions because objdump
              -- resolves the branch targets and we can't, so we can
              -- never match objdump's output.
                "bls"
              , "bl"
              , "b"

              -- These get represented as load/store multiple with sp
              -- mutation; push and pop are not even mentioned in the
              -- Tgen data.
              , "push"
              , "pop"

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

              -- Ignored because the Tgen format string uses "ldm" for
              -- this instruction and objdump uses "ldmfd".
              , "ldmfd"

              -- Ignored because the Tgen format string for these
              -- reference an implied operand that isn't mentioned
              -- in the bit patterns, so it's impossible for us to
              -- pretty-print these.
              , "ldrdeq"
              , "strdeq"

              -- Ignored because we don't have enough context to format
              -- the arguments correctly.
              , "stcl"
              , "ldrsht"
              , "ldcl"
              ]

    matchInstruction name = "(^[[:space:]]*" <> name <> conditions <> "[[:space:]])"

    conditions = "(" <> (concat $ L.intersperse "|"
                  (PP.render <$> PP.pPrint <$> ARM.mkPred <$> [0..13])) <> ")?"

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
