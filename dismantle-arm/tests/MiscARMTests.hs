{-# LANGUAGE BinaryLiterals #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}

module MiscARMTests ( miscArmTests ) where

import           Data.Bits ( shiftL, (.|.) )
import           Data.Char (isSpace)
import qualified Data.List as L
import           Data.Monoid ( (<>) )
import qualified Data.Text.Lazy as TL
import           Data.Word ( Word64, Word32 )
import qualified Data.Word.Indexed as W
import qualified Test.Tasty as T
import           Test.Tasty.HUnit
import           Text.PrettyPrint.HughesPJClass hiding ( (<>) )

import qualified Dismantle.ARM as ARM
import qualified Dismantle.ARM.ISA as ARM
import           Dismantle.ARM.Operands
import           Dismantle.Testing
import           Dismantle.Testing.ParserTests ( parserTests )
import qualified Dismantle.Testing.Regex as RE


miscArmTests :: IO T.TestTree
miscArmTests = return $ T.testGroup "Misc ARM" $
               map mkSoRegImmPPTest shiftValues <>
               [ mkSoRegImmFieldTest ]

-- This is actually a test of the maskM, extract, and insert
-- operations internal to the Operands implementation.  Just one
-- operand using these is tested, but the assumption is that they will
-- then work for all operands.
mkSoRegImmFieldTest :: T.TestTree
mkSoRegImmFieldTest =
    let mkT (act, w32, imm, ty, reg) =
            testCase ("from w32 " <> show w32) $ do
              imm @=? soRegImmImmediate (mkSoRegImm w32)
              ty  @=? soRegImmShiftType (mkSoRegImm w32)
              reg @=? soRegImmReg (mkSoRegImm w32)
              act @=? soRegImmToBits (mkSoRegImm w32)
    in T.testGroup "mkSoRegImm fields" $
     map mkT [ (0, 0, 0, 0, gpr 0)
             , (0b0111111101111, 0xffffffff, 0b11111, 0b11, gpr 0b1111)
             , (0b0000000001111, 0b111111111111111111000000011111, 0, 0, gpr 0b1111)
             , (0b0000001100000, 0b111111111111111111000001110000, 0, 0b11, gpr 0)
             , (0b0111110000000, 0b111111111111111111111110000000, 0b11111, 0, gpr 0)
             ]

type ShiftTypeName = String
type OpcodeName = String
type ShiftTypeVal = Word32
type ImmVal = W.W 5

mkSoRegImmPPTest :: (ShiftTypeName, ShiftTypeVal, [ImmRangeSpec]) -> T.TestTree
mkSoRegImmPPTest (n, v, mbr) =
    let mkInp ty imm = mkSoRegImm $ (shiftL (ty :: ShiftTypeVal) 5) .|.
                                    (shiftL ((fromIntegral $ W.unW $ (imm :: ImmVal)) :: Word32) 7)
        -- mkInp values: see ORR F7.1.128, F7-2740 and DecodeImmShift F2.4.3, F2-2420
    in mkImmShiftPPT "SoRegImm" mkInp n v mbr

mkImmShiftPPT :: Pretty a =>
                 OpcodeName -> (ShiftTypeVal -> ImmVal -> a)
              -> ShiftTypeName -> ShiftTypeVal -> [ImmRangeSpec]  -- these three are from shiftValues
              -> T.TestTree
mkImmShiftPPT oprndName mkOprnd ty tyVal immRanges =
    let mkTest imm =
            case (filter (immInRange imm) immRanges) of
              [] -> []  -- this ty + imm isn't valid, so no tests
              (x:[]) -> [ testCase (caseName imm) $
                          let pps = show $ pPrint (mkOprnd tyVal imm)
                              mbImm (_,_,f) = f imm
                          in case mbImm x of
                               Nothing -> return ()
                               Just ir -> do ty `L.isInfixOf` pps @? ty <> " not in " <> pps
                                             ir `L.isInfixOf` pps @? ir <> " offset not in " <> pps
                        ]
              xs -> [testCase (caseName imm) $
                     assertFailure $ "Multiple ranges matched for " <> oprndName <> " imm=" <> show imm]
        immInRange imm (immLo, immHi, _) = immLo <= imm && imm <= immHi
        caseName imm = "pPrint ImmShift " <> ty <> " " <> show imm
    in T.testGroup ("pPrint ImmShift " <> ty) $ concatMap mkTest [minBound..maxBound]


-- If ImmVal -> Maybe String is Nothing, then this immval is not valid
-- for this shift type and no test is performed.
type ImmRangeSpec = (ImmVal, ImmVal, ImmVal -> Maybe String)

shiftValues :: [ (ShiftTypeName, ShiftTypeVal, [ImmRangeSpec]) ]
shiftValues =
    let showVal = (Just . (<>) "#" . show . pPrint)
        showValTo32 v = Just $ if v == 0 then "#32" else ("#" <> show (pPrint v))
    in [ ("lsl", 0b00, [(1, 31, showVal), (0, 0, const Nothing)])  -- 0 is no lsl shown
       , ("lsr", 0b01, [(0, 31, showValTo32)])
       , ("asr", 0b10, [(0, 31, showValTo32)])
       , ("ror", 0b11, [(1, 31, showVal), (0, 0, const Nothing)]) -- 0 is rrx, not ror
       , ("rrx", 0b11, [(0, 0, const (Just "")), (1, 31, const Nothing)]) -- rrx is only if imm == 0
       ]
