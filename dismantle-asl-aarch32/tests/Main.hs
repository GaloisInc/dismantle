{-# LANGUAGE OverloadedStrings #-}

{-
  Note: when a binary representation of an ARM instruction is
        displayed, the byte order is: 6 7 4 5 2 3 0 1

        In the documentation, bits 31 .... 0  correspond to bytes 0 ... 7

  For example:

    10001100.00110000.10010011.11100001      orrs r3, r3, ip, lsl #1
    ----____ ----____ ----____ ----____
      6   7    4   5    2   3    0   1

    [defined on page F7.1.127, F7-2738]

    Translation:
     cond 0011 100S .rn. .rd. ..imm12....
        rn = r3
        rd = r3
        lsl = 0b00
        imm12 = 0000 1000 1100  = 0x08c
-}

module Main ( main ) where

import qualified Data.ByteString.Lazy as BL
import           Data.Char (isSpace)
import qualified Data.List as L
import           Data.Monoid ((<>))
import           Data.Parameterized.List
import qualified Data.Text.Lazy as TL
import           Data.Word (Word64)
import qualified Data.Word.Indexed as WI
import qualified Test.Tasty as T
import qualified Text.PrettyPrint.HughesPJClass as PP

import qualified Dismantle.ASL.AArch32 as ARM
import qualified Dismantle.ASL.AArch32.ISA as ARM
import           Dismantle.Testing
import           Dismantle.Testing.ParserTests ( parserTests )
import qualified Dismantle.Testing.Regex as RE

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
    , ("tests/bin/libfftw3.so.3.4.4"
      , [
        -- These addresses are ignored because objdump considers them to
        -- be 'cmn' instructions but that is clearly wrong in the ARM
        -- ARM.
          0x9a948
        , 0xea750
        , 0x131780
        , 0x181300

        -- These addresses all use the same non-instruction byte
        -- sequence: 0x3c182b51. objdump claims this disassembles to
        -- ldccc, but none of the ARM ARM patterns for LDC match this
        -- sequence.
        , 0x36408
        , 0x372e8
        , 0x37718
        , 0x37b48
        , 0x5a498
        , 0x5a4e8
        , 0x5b220
        , 0x5b698
        , 0x7b520
        , 0x7b570
        , 0x7c298
        , 0x7c728
        , 0xb07f0
        , 0xb0c38
        , 0xb0cb8
        , 0xb1978
        , 0xb19c0
        , 0xd41d0
        , 0xd4690
        , 0xd5820
        , 0xe6cb8
        , 0xe7180
        , 0xe82f8
        , 0xf5ee0
        , 0xf6358
        , 0xf63a0
        , 0xf6f78
        , 0xf6fa8
        , 0x1488d0
        , 0x148d20
        , 0x168d40
        , 0x169df8
        , 0x17ba78
        , 0x17ca88
        , 0x18cab8
        , 0x18cec8

        -- These addresses are considered 'sbc' instructions by objdump,
        -- but we disassemble them as 'adrpc' legally.
        , 0x1b7d8
        , 0x3c9f0
        , 0x9a928
        , 0xb7098
        , 0xea738
        , 0x1319e8
        , 0x14d0f0
        , 0x181308

        -- These addresses are considered to be 'and' instructions by
        -- objdump, but we disassemble them as 'adr'. objdump is wrong.
        , 0x20270
        , 0x20720
        , 0x20748
        , 0x20fa0
        , 0x9d298
        , 0x9d2a0
        , 0x133ea8
        , 0x133ee8

        -- Objdump claims "strdvc" but the Tgen says otherwise for that
        -- instruction.
        , 0x9a930
        , 0xea740
        , 0x131778
        , 0x1812f8

        -- This location is handled properly by our library, but our
        -- pretty-print for it is nicer than objdump.
        , 0xb0ec
        ]
      )
    ]

arm :: ArchTestConfig
arm = ATC { testingISA = ARM.isa
          , disassemble = ARM.disassembleInstruction
          , assemble = ARM.assembleInstruction
          , prettyPrint = ARM.ppInstruction
          , expectFailure = Just expectedFailures
          , skipPrettyCheck = Just skipPretty
          , ignoreAddresses = ignored
          , customObjdumpArgs = []
          , normalizePretty = normalize
          , comparePretty = Just cmpInstrs
          , instructionFilter = ((== FullWord) . insnLayout)
          }

main :: IO ()
main = do
--  let i = ARM.Instruction ARM.MOV_i_A1_A (ARM.Bv4 (WI.w 0x0) :< ARM.Bv1 (WI.w 0) :<
--  ARM.Bv4 (WI.w 0x0) :< ARM.Bv12 (WI.w 0x0) :< Nil)
  let i = ARM.Instruction ARM.VMLA_i_T2A2_A
          (ARM.Bv1 (WI.w 0) :<
           ARM.Bv1 (WI.w 0) :<
           ARM.Bv2 (WI.w 0) :<
           ARM.Bv4 (WI.w 0) :<
           ARM.Bv4 (WI.w 0) :<
           ARM.Bv1 (WI.w 0) :<
           ARM.Bv1 (WI.w 0) :<
           ARM.Bv1 (WI.w 0) :<
           ARM.Bv4 (WI.w 0) :<
           Nil)
  -- print i
  let ai = ARM.assembleInstruction i
  print $ BL.unpack ai
  -- let (_, mi) = ARM.disassembleInstruction ai
  -- case mi of
  --   Nothing -> print "Failure"
  --   Just i' -> print i'
  -- tg <- binaryTestSuite arm "tests/bin"
  -- T.defaultMain $ T.testGroup "dismantle-arm" [tg]

  return ()

normalize :: TL.Text -> TL.Text
normalize =
    -- Then remove whitespace
    TL.filter (not . isSpace) .
    -- Remove square brackets and "#"
    TL.filter (flip notElem ("[]#"::String)) .
    -- First, trim any trailing comments
    (fst . TL.breakOn ";")

cmpInstrs :: TL.Text -> TL.Text -> Bool
cmpInstrs objdump dismantle =
  -- Many operations have an optional shift amount that does not need
  -- to be specified when the shift amount is zero.  Some objdump
  -- output contains this value however, but dismantle doesn't, so if
  -- a direct comparison fails, try stripping a ", 0" from the objdump
  -- version and check equality of that result (n.b. comparing the
  -- normalized versions, spaces are stripped).
  objdump == dismantle ||
  (",0" `TL.isSuffixOf` objdump &&
    (TL.reverse $ TL.drop 2 $ TL.reverse objdump) == dismantle)

rx :: String -> RE.Regex
rx s =
  case RE.mkRegex s of
    Left e -> error ("Invalid regex <<" ++ s ++ ">> because: " ++ e)
    Right r -> r

skipPretty :: RE.Regex
skipPretty = rx ".*"

expectedFailures :: RE.Regex
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
