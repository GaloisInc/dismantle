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

import qualified Dismantle.ARM.A32 as ARM
import qualified Dismantle.ARM.ISA as ARM
import           Dismantle.Testing
import           Dismantle.Testing.ParserTests ( parserTests )
import qualified Dismantle.Testing.Regex as RE

ignored :: [(FilePath, [Word64])]
ignored =
    [ ("test/bin/ls"
      , [
        -- These are related to github issue #13 - there is an inconsistency in
        -- the spec between the ARMv7 and ARMv8 masks for the STC instructions
          0x11b50
        , 0x166d8
        , 0x166f0

        -- FIXME: Unknown
        , 0x1d77c
        ]
      )
    , ("test/bin/libfftw3.so.3.4.4"
      , [
        -- -- These addresses all use the same non-instruction byte
        -- -- sequence: 0x3c182b51. objdump claims this disassembles to
        -- -- ldccc, but none of the ARM ARM patterns for LDC match this
        -- -- sequence.
        -- , 0x36408
        -- , 0x372e8
        -- , 0x37718
        -- , 0x37b48
        -- , 0x5a498
        -- , 0x5a4e8
        -- , 0x5b220
        -- , 0x5b698
        -- , 0x7b520
        -- , 0x7b570
        -- , 0x7c298
        -- , 0x7c728
        -- , 0xb07f0
        -- , 0xb0c38
        -- , 0xb0cb8
        -- , 0xb1978
        -- , 0xb19c0
        -- , 0xd41d0
        -- , 0xd4690
        -- , 0xd5820
        -- , 0xe6cb8
        -- , 0xe7180
        -- , 0xe82f8
        -- , 0xf5ee0
        -- , 0xf6358
        -- , 0xf63a0
        -- , 0xf6f78
        -- , 0xf6fa8
        -- , 0x1488d0
        -- , 0x148d20
        -- , 0x168d40
        -- , 0x169df8
        -- , 0x17ba78
        -- , 0x17ca88
        -- , 0x18cab8
        -- , 0x18cec8

        -- -- These addresses are considered 'sbc' instructions by objdump,
        -- -- but we disassemble them as 'adrpc' legally.
        -- , 0x1b7d8
        -- , 0x3c9f0
        -- , 0x9a928
        -- , 0xb7098
        -- , 0xea738
        -- , 0x1319e8
        -- , 0x14d0f0
        -- , 0x181308

        -- -- These addresses are considered to be 'and' instructions by
        -- -- objdump, but we disassemble them as 'adr'. objdump is wrong.
        -- , 0x20270
        -- , 0x20720
        -- , 0x20748
        -- , 0x20fa0
        -- , 0x9d298
        -- , 0x9d2a0
        -- , 0x133ea8
        -- , 0x133ee8

        -- -- Objdump claims "strdvc" but the Tgen says otherwise for that
        -- -- instruction.
        -- , 0x9a930
        -- , 0xea740
        -- , 0x131778
        -- , 0x1812f8

        -- -- This location is handled properly by our library, but our
        -- -- pretty-print for it is nicer than objdump.
        -- , 0xb0ec


        -- These are related to github issue #13 - there is an inconsistency in
        -- the spec between the ARMv7 and ARMv8 masks for the STC instructions
          0x28178
        , 0x28c00
        , 0x2e5a8
        , 0x2efd0
        , 0x30508
        , 0x30ef0
        , 0x47c88
        , 0x48600
        , 0x504d0
        , 0x50f58
        , 0x51a70
        , 0x53020
        , 0x64330
        , 0x64ca8
        , 0x6e458
        , 0x6eee0
        , 0x6f9f0
        , 0x70fb0
        , 0xa0988
        , 0xa3c18
        , 0xa4438
        , 0xa4c20
        , 0xa5a18
        , 0xa6240
        , 0xa8bc8
        , 0xa94c0
        , 0xaa6c0
        , 0xaba40
        , 0xc1278
        , 0xc2800
        , 0xc9c10
        , 0xcb4e8
        , 0xcbcd0
        , 0xcd2c8
        , 0xdd960
        , 0xdeec8
        , 0xecd60
        , 0xed628
        , 0xeda10
        , 0xef638
        , 0xf0b78
        , 0xf1388
        , 0x1020e0
        , 0x102a48
        , 0x10e0d8
        , 0x10ea40
        , 0x11e0d8
        , 0x11e4e0
        , 0x11ed08
        , 0x11f108
        , 0x12b498
        , 0x12b898
        , 0x12c0c0
        , 0x12c4b0
        , 0x1372c8
        , 0x137b70
        , 0x13a6d0
        , 0x13af28
        , 0x13b728
        , 0x13b738
        , 0x140b88
        , 0x141390
        , 0x141b98
        , 0x142460
        , 0x142ca0
        , 0x1434a8
        , 0x1434d8
        , 0x1455e8
        , 0x1455f0
        , 0x157e00
        , 0x158900
        , 0x1604b8
        , 0x1610b8
        , 0x1633a0
        , 0x164220
        , 0x174500
        , 0x175110
        , 0x183720
        , 0x183a38
        , 0x184a08
        , 0x185a30
        , 0x187ad8
        , 0x188308
        , 0x1893d8
        , 0x189c28
        , 0x197170
        , 0x197e18
        , 0x1a36d0
        , 0x1a4118
        , 0x1b1c08
        , 0x1b2468
        , 0x1b2ee0
        , 0x1bdb60
        , 0x1be3c0
        , 0x1bee38
        , 0x1c43a0
        , 0x1c4a80
        -- FIXME: Unknown
        , 0x0c6b4
        , 0x10b84
        , 0x0001b7e8
        , 0x0001b7f0
        , 0x0001bc58
        , 0x0001bc60
        , 0x0001d3a0
        , 0x0001d3a8
        , 0x0001d840
        , 0x0001d860
        , 0x0001d878
        , 0x0001e0f8
        , 0x0001e110
        , 0x000202a0
        , 0x000206f0
        , 0x00020f60
        , 0x0002fac8
        , 0x000304f8
        , 0x00030f00
        , 0x000318c8
        , 0x000363f8
        , 0x00036418
        , 0x00036a90
        , 0x00036eb8
        , 0x000372c8
        , 0x000372d8
        , 0x00037708
        , 0x00037b58
        , 0x00037b68
        , 0x00037f78
        , 0x00037f80
        , 0x0003ca00
        , 0x0003ca08
        , 0x0003ce00
        , 0x0003ce08
        , 0x00051a80
        , 0x00052558
        , 0x00053040
        , 0x00053b18
        , 0x0005a488
        , 0x0005a4a8
        , 0x0005a4b8
        , 0x0005a4d8
        , 0x0005a4f8
        , 0x0005b200
        , 0x0005b210
        , 0x0005b670
        , 0x0005b678
        , 0x0005b688
        , 0x0006fa00
        , 0x000704d8
        , 0x00070fd0
        , 0x00071aa8
        , 0x0007b510
        , 0x0007b530
        , 0x0007b540
        , 0x0007b560
        , 0x0007b580
        , 0x0007c278
        , 0x0007c288
        , 0x0007c700
        , 0x0007c708
        , 0x0007c718
        , 0x0009a930
        , 0x0009a938
        , 0x0009a960
        , 0x0009ab50
        , 0x0009ab58
        , 0x0009b818
        , 0x0009b820
        , 0x0009bc60
        , 0x0009bc78
        , 0x0009ce40
        , 0x000a3c38
        , 0x000a4418
        , 0x000a4428
        , 0x000a4c38
        , 0x000aafa0
        , 0x000aba48
        , 0x000ada18
        , 0x000ae490
        , 0x000ae4a0
        , 0x000ae4b0
        , 0x000b0c10
        , 0x000b0c20
        , 0x000b0c50
        , 0x000b0c58
        , 0x000b0c78
        , 0x000b0c88
        , 0x000b0ca8
        , 0x000b0cc8
        , 0x000b1920
        , 0x000b1928
        , 0x000b1958
        , 0x000b1970
        , 0x000b1998
        , 0x000b19b0
        , 0x000b6bb8
        , 0x000b6bc0
        , 0x000b70a8
        , 0x000b70b0
        , 0x000c9c28
        , 0x000ca410
        , 0x000cc810
        , 0x000cd2f0
        , 0x000cddc8
        , 0x000d41e8
        , 0x000d41f0
        , 0x000d4660
        , 0x000d4670
        , 0x000d46a8
        , 0x000d46c0
        , 0x000d57f8
        , 0x000d5800
        , 0x000d5810
        , 0x000d5868
        , 0x000e6cd0
        , 0x000e6cd8
        , 0x000e7150
        , 0x000e7160
        , 0x000e7198
        , 0x000e71b0
        , 0x000e82d0
        , 0x000e82d8
        , 0x000e82e8
        , 0x000e8340
        , 0x000ea720
        , 0x000ea728
        , 0x000ea740
        , 0x000ee240
        , 0x000eea50
        , 0x000f13b0
        , 0x000f1be8
        , 0x000f2558
        , 0x000f2570
        , 0x000f2578
        , 0x000f62f8
        , 0x000f6310
        , 0x000f6320
        , 0x000f6338
        , 0x000f6368
        , 0x000f6378
        , 0x000f6390
        , 0x000f63b0
        , 0x000f6f88
        , 0x000f6f98
        , 0x000f6fb8
        , 0x000f6fc8
        , 0x000f6ff8
        , 0x000f7048
        , 0x00131778
        , 0x00131a08
        , 0x00131a10
        , 0x00132598
        , 0x001325b0
        , 0x001329a0
        , 0x00133978
        , 0x0013af30
        , 0x0013b748
        , 0x0013bf88
        , 0x0013bfb8
        , 0x00141350
        , 0x00141bd8
        , 0x00142490
        , 0x00142c78
        , 0x00142cc0
        , 0x001434b8
        , 0x00143d18
        , 0x00144540
        , 0x00144570
        , 0x00144d68
        , 0x00144da0
        , 0x001455a8
        , 0x001455d8
        , 0x00148470
        , 0x00148480
        , 0x001488d8
        , 0x00148d10
        , 0x0014d110
        , 0x0014d118
        , 0x0014d518
        , 0x0014d520
        , 0x001619a0
        , 0x00162538
        , 0x001633c0
        , 0x00164240
        , 0x001688b0
        , 0x00168d50
        , 0x00168d60
        , 0x00168d70
        , 0x00169e30
        , 0x00169e40
        , 0x00169e70
        , 0x0017ba38
        , 0x0017ba58
        , 0x0017ba68
        , 0x0017ba88
        , 0x0017cef8
        , 0x0017cf08
        , 0x0017cf30
        , 0x001810b0
        , 0x001812f0
        , 0x001812f8
        , 0x00185208
        , 0x00185228
        , 0x00185a50
        , 0x00188348
        , 0x00188350
        , 0x00188b80
        , 0x00188b88
        , 0x001893a8
        , 0x001893d0
        , 0x0018a470
        , 0x0018a488
        , 0x0018caa8
        , 0x0018cec0
        , 0x0018d400
        , 0x0018d710
        , 0x0018d728
        , 0x001cd34c
        , 0x001cd350
        ]
      )
    ]

arm :: ArchTestConfig
arm = ATC { testingISA = ARM.isa "A32"
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
  tg <- binaryTestSuite arm "test/bin"
  T.defaultMain $ T.testGroup "dismantle-arm-xml" [tg]

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
