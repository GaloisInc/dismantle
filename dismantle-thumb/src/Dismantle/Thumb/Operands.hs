{-# OPTIONS_HADDOCK not-home #-}
{-# LANGUAGE BinaryLiterals #-}
module Dismantle.Thumb.Operands (
  GPR,
  gpr,
  unGPR,

  LowGPR,
  lowGpr,
  unLowGPR,

  Reglist,
  mkRegList,
  regListToBits,

  Bit,
  mkBit,
  bitToBits,

  Imm1_32,
  mkImm1_32,
  imm1_32ToBits,

  ITMask,
  mkITMask,
  itMaskToBits,

  TBrTarget,
  mkTBrTarget,
  tBrTargetToBits,

  T2SoImm,
  mkT2SoImm,
  t2SoImmToBits,

  Opcode,
  mkOpcode,
  opcodeToBits,

  BfInvMaskImm,
  mkBfInvMaskImm,
  bfInvMaskImmToBits,

  T2AddrModeSoReg,
  mkT2AddrModeSoReg,
  t2AddrModeSoRegToBits,

  T2AddrModeImm12,
  mkT2AddrModeImm12,
  t2AddrModeImm12ToBits,

  T2AddrModeImm8S4Pre,
  mkT2AddrModeImm8S4Pre,
  t2AddrModeImm8S4PreToBits,

  T2AddrModeImm8S4,
  mkT2AddrModeImm8S4,
  t2AddrModeImm8S4ToBits,

  T2AddrModeImm8,
  mkT2AddrModeImm8,
  t2AddrModeImm8ToBits,

  T2AddrModeImm8Offset,
  mkT2AddrModeImm8Offset,
  t2AddrModeImm8OffsetToBits,

  T2AddrModeImm8S4Offset,
  mkT2AddrModeImm8S4Offset,
  t2AddrModeImm8S4OffsetToBits,

  T2AddrModeNegImm8,
  mkT2AddrModeNegImm8,
  t2AddrModeNegImm8ToBits,

  T2AddrModeImm8Pre,
  mkT2AddrModeImm8Pre,
  t2AddrModeImm8PreToBits,

  TAdrLabel,
  mkTAdrLabel,
  tAdrLabelToBits,

  T2AdrLabel,
  mkT2AdrLabel,
  t2AdrLabelToBits,

  T2LdrLabel,
  mkT2LdrLabel,
  t2LdrLabelToBits,

  ThumbBlTarget,
  mkThumbBlTarget,
  thumbBlTargetToBits,

  ThumbBlxTarget,
  mkThumbBlxTarget,
  thumbBlxTargetToBits,

  AddrModeImm01020S4,
  mkAddrModeImm01020S4,
  addrModeImm01020S4ToBits,

  AddrModeIs1,
  mkAddrModeIs1,
  addrModeIs1ToBits,

  AddrModeIs2,
  mkAddrModeIs2,
  addrModeIs2ToBits,

  AddrModeIs4,
  mkAddrModeIs4,
  addrModeIs4ToBits,

  TImm0508S4,
  mkTImm0508S4,
  tImm0508S4ToBits,

  TImm01020S4,
  mkTImm01020S4,
  tImm01020S4ToBits,

  AddrModePc,
  mkAddrModePc,
  addrModePcToBits,

  AddrModeSp,
  mkAddrModeSp,
  addrModeSpToBits,

  AddrModeRr,
  mkAddrModeRr,
  addrModeRrToBits,

  Pred,
  mkPred,
  predToBits,

  T2SoReg,
  mkT2SoReg,
  t2SoRegToBits
  ) where

import Data.Bits
import Data.Maybe (catMaybes)
import Data.Monoid
import Data.Word ( Word8, Word16, Word32 )

import qualified Text.PrettyPrint.HughesPJClass as PP

-- For Arbitrary instances
import Dismantle.Tablegen.TH.Pretty ()

import qualified Dismantle.Arbitrary as A

import qualified Dismantle.ARM.Operands as ARM

-- | A bit field description and functions for using it

data Field = Field { _fieldBits :: Int
                   -- ^ The number of bits in the field
                   , fieldOffset :: Int
                   -- ^ The offset of the rightmost bit in the field,
                   -- starting from zero
                   }

mkMask :: Field -> Word32
mkMask (Field bits offset) = (2 ^ bits - 1) `shiftL` offset

insert :: (Integral a) => Field -> a -> Word32 -> Word32
insert (Field bits offset) src dest =
    dest .|. (((fromIntegral src) .&. (2 ^ bits - 1)) `shiftL` offset)

extract :: Field -> Word32 -> Word32
extract f val = (val .&. (mkMask f)) `shiftR` (fieldOffset f)

-- Operand data types

newtype Bit = Bit { _unBit :: Word8 }
  deriving (Eq, Ord, Show)

instance PP.Pretty Bit where
  pPrint (Bit v) = PP.int $ fromIntegral v

mkBit :: Word32 -> Bit
mkBit v = Bit $ fromIntegral v

bitToBits :: Bit -> Word32
bitToBits (Bit v) = fromIntegral v

data Reglist = Reglist { _unReglist :: Word16 }
  deriving (Eq, Ord, Show)

instance PP.Pretty Reglist where
  pPrint (Reglist bits) =
      let positions :: [Int]
          positions = [0..15]
          members = catMaybes $ regMember <$> positions
          regMember rn = if (((bits `shiftR` rn) .&. 0b1) == 0b1)
                         then Just rn
                         else Nothing
          regs = GPR <$> fromIntegral <$> members
      in PP.braces (PP.hsep $ PP.punctuate (PP.text ",") (PP.pPrint <$> regs))

mkRegList :: Word32 -> Reglist
mkRegList v = Reglist $ fromIntegral v

regListToBits :: Reglist -> Word32
regListToBits (Reglist v) = fromIntegral v

-- | General-purpose low register by number (0-7) for 16-bit thumb
-- instructions
newtype LowGPR = LowGPR { unLowGPR :: Word8 }
  deriving (Eq, Ord, Show)

instance PP.Pretty LowGPR where
  pPrint (LowGPR rno) = PP.char 'r' <> PP.int (fromIntegral rno)

lowGpr :: Word8 -> LowGPR
lowGpr = LowGPR

-- | General-purpose register by number
newtype GPR = GPR { unGPR :: Word8 }
  deriving (Eq, Ord, Show)

instance PP.Pretty GPR where
  pPrint (GPR 10) = PP.text "sl"
  pPrint (GPR 11) = PP.text "fp"
  pPrint (GPR 12) = PP.text "ip"
  pPrint (GPR 13) = PP.text "sp"
  pPrint (GPR 14) = PP.text "lr"
  pPrint (GPR 15) = PP.text "pc"
  pPrint (GPR rno) = PP.char 'r' <> PP.int (fromIntegral rno)

gpr :: Word8 -> GPR
gpr = GPR

-- | Coprocessor operation opcode register by number
newtype Opcode = Opcode { _unOpcode :: Word8 }
  deriving (Eq, Ord, Show)

mkOpcode :: Word32 -> Opcode
mkOpcode w = Opcode $ fromIntegral w

opcodeToBits :: Opcode -> Word32
opcodeToBits (Opcode i) = fromIntegral i

instance PP.Pretty Opcode where
  pPrint (Opcode o) = PP.pPrint o

data T2AddrModeImm8S4Pre =
    T2AddrModeImm8S4Pre { t2AddrModeImm8s4PreAdd       :: Word8
                        , t2AddrModeImm8s4PreImmediate :: Word8
                        , t2AddrModeImm8s4PreReg       :: GPR
                        }
                        deriving (Eq, Ord, Show)

instance PP.Pretty T2AddrModeImm8S4Pre where
    pPrint _ = PP.text "not implemented 1"

mkT2AddrModeImm8S4Pre :: Word32 -> T2AddrModeImm8S4Pre
mkT2AddrModeImm8S4Pre w = T2AddrModeImm8S4Pre (fromIntegral add) (fromIntegral imm8) (GPR $ fromIntegral reg)
  where
    add = extract (Field 1 8) w
    reg = extract (Field 4 9) w
    imm8 = extract (Field 8 0) w

t2AddrModeImm8S4PreToBits :: T2AddrModeImm8S4Pre -> Word32
t2AddrModeImm8S4PreToBits (T2AddrModeImm8S4Pre add imm (GPR reg)) =
    insert (Field 4 9) reg $
    insert (Field 1 8) add $
    insert (Field 8 0) imm 0

data T2LdrLabel =
    T2LdrLabel { t2LdrLabelImm :: Word16
               , t2LdrLabelAdd :: Word8
               }
               deriving (Eq, Ord, Show)

instance PP.Pretty T2LdrLabel where
    pPrint _ = PP.text "not implemented 2"

mkT2LdrLabel :: Word32 -> T2LdrLabel
mkT2LdrLabel w = T2LdrLabel (fromIntegral imm) (fromIntegral add)
  where
    add = extract (Field 1 12) w
    imm = extract (Field 12 0) w

t2LdrLabelToBits :: T2LdrLabel -> Word32
t2LdrLabelToBits (T2LdrLabel add imm) =
    insert (Field 1 12) add $
    insert (Field 12 0) imm 0

data T2AddrModeImm8S4 =
    T2AddrModeImm8S4 { t2AddrModeImm8s4Add       :: Word8
                     , t2AddrModeImm8s4Immediate :: Word8
                     , t2AddrModeImm8s4Reg       :: GPR
                     }
                     deriving (Eq, Ord, Show)

instance PP.Pretty T2AddrModeImm8S4 where
    pPrint _ = PP.text "not implemented 3"

mkT2AddrModeImm8S4 :: Word32 -> T2AddrModeImm8S4
mkT2AddrModeImm8S4 w = T2AddrModeImm8S4 (fromIntegral add) (fromIntegral imm8) (GPR $ fromIntegral reg)
  where
    add = extract (Field 1 8) w
    reg = extract (Field 4 9) w
    imm8 = extract (Field 8 0) w

t2AddrModeImm8S4ToBits :: T2AddrModeImm8S4 -> Word32
t2AddrModeImm8S4ToBits (T2AddrModeImm8S4 add imm (GPR reg)) =
    insert (Field 4 9) reg $
    insert (Field 1 8) add $
    insert (Field 8 0) imm 0

data AddrModeIs1 =
    AddrModeIs1 { addrModeIs1Reg :: LowGPR
                , addrModeIs1Imm :: Word8
                }
                deriving (Eq, Ord, Show)

instance PP.Pretty AddrModeIs1 where
  pPrint m =
      let suf = PP.char ',' PP.<+> ((PP.char '#') <> (PP.pPrint $ addrModeIs1Imm m))
      in PP.brackets $ PP.pPrint (addrModeIs1Reg m) <> suf

addrModeIs1RegField :: Field
addrModeIs1RegField = Field 3 0

addrModeIs1ImmField :: Field
addrModeIs1ImmField = Field 5 3

mkAddrModeIs1 :: Word32 -> AddrModeIs1
mkAddrModeIs1 w = AddrModeIs1 (LowGPR $ fromIntegral reg) (fromIntegral imm)
  where
    reg = extract addrModeIs1RegField w
    imm = extract addrModeIs1ImmField w

addrModeIs1ToBits :: AddrModeIs1 -> Word32
addrModeIs1ToBits (AddrModeIs1 (LowGPR r) imm) =
    insert addrModeIs1RegField r $
    insert addrModeIs1ImmField imm 0

data AddrModeIs2 =
    AddrModeIs2 { addrModeIs2Reg :: LowGPR
                , addrModeIs2Imm :: Word8
                }
                deriving (Eq, Ord, Show)

instance PP.Pretty AddrModeIs2 where
  pPrint m =
      let suf = PP.char ',' PP.<+> ((PP.char '#') <> (PP.pPrint $ addrModeIs2Imm m `shiftL` 2))
      in PP.brackets $ PP.pPrint (addrModeIs2Reg m) <> suf

addrModeIs2RegField :: Field
addrModeIs2RegField = Field 3 0

addrModeIs2ImmField :: Field
addrModeIs2ImmField = Field 5 3

mkAddrModeIs2 :: Word32 -> AddrModeIs2
mkAddrModeIs2 w = AddrModeIs2 (LowGPR $ fromIntegral reg) (fromIntegral imm)
  where
    reg = extract addrModeIs2RegField w
    imm = extract addrModeIs2ImmField w

addrModeIs2ToBits :: AddrModeIs2 -> Word32
addrModeIs2ToBits (AddrModeIs2 (LowGPR r) imm) =
    insert addrModeIs2RegField r $
    insert addrModeIs2ImmField imm 0

data AddrModeIs4 =
    AddrModeIs4 { addrModeIs4Reg :: LowGPR
                , addrModeIs4Imm :: Word8
                }
                deriving (Eq, Ord, Show)

instance PP.Pretty AddrModeIs4 where
  pPrint m =
      let suf = PP.char ',' PP.<+> ((PP.char '#') <> (PP.pPrint $ addrModeIs4Imm m `shiftL` 2))
      in PP.brackets $ PP.pPrint (addrModeIs4Reg m) <> suf

addrModeIs4RegField :: Field
addrModeIs4RegField = Field 3 0

addrModeIs4ImmField :: Field
addrModeIs4ImmField = Field 5 3

mkAddrModeIs4 :: Word32 -> AddrModeIs4
mkAddrModeIs4 w = AddrModeIs4 (LowGPR $ fromIntegral reg) (fromIntegral imm)
  where
    reg = extract addrModeIs4RegField w
    imm = extract addrModeIs4ImmField w

addrModeIs4ToBits :: AddrModeIs4 -> Word32
addrModeIs4ToBits (AddrModeIs4 (LowGPR r) imm) =
    insert addrModeIs4RegField r $
    insert addrModeIs4ImmField imm 0

data TAdrLabel =
    TAdrLabel { tAddrLabelImm :: Word8
              }
              deriving (Eq, Ord, Show)

instance PP.Pretty TAdrLabel where
  pPrint m = PP.pPrint (((fromIntegral $ tAddrLabelImm m) :: Word32) `shiftL` 2)

tAddrLabelImmField :: Field
tAddrLabelImmField = Field 8 0

mkTAdrLabel :: Word32 -> TAdrLabel
mkTAdrLabel w = TAdrLabel (fromIntegral imm)
  where
    imm = extract tAddrLabelImmField w

tAdrLabelToBits :: TAdrLabel -> Word32
tAdrLabelToBits (TAdrLabel imm) =
    insert tAddrLabelImmField imm 0

data T2AdrLabel =
    T2AdrLabel { t2AdrLabelImm     :: Word16
               , t2AdrLabelVariant :: Word8
               }
               deriving (Eq, Ord, Show)

instance PP.Pretty T2AdrLabel where
  pPrint _ = PP.text "not implemented 4"

t2AddrLabelImmField :: Field
t2AddrLabelImmField = Field 11 0

t2AddrLabelVariantField :: Field
t2AddrLabelVariantField = Field 1 12

mkT2AdrLabel :: Word32 -> T2AdrLabel
mkT2AdrLabel w = T2AdrLabel (fromIntegral imm) (fromIntegral var)
  where
    imm = extract t2AddrLabelImmField w
    var = extract t2AddrLabelVariantField w

t2AdrLabelToBits :: T2AdrLabel -> Word32
t2AdrLabelToBits (T2AdrLabel imm var) =
    insert t2AddrLabelImmField imm $
    insert t2AddrLabelVariantField var 0

data TImm0508S4 =
    TImm0508S4 { tImm0508S4Imm :: Word8
               }
               deriving (Eq, Ord, Show)

instance PP.Pretty TImm0508S4 where
  pPrint m =
      PP.brackets $ PP.char '#' <> PP.pPrint (((fromIntegral $ tImm0508S4Imm m) :: Word32) `shiftL` 2)

tImm0508S4ImmField :: Field
tImm0508S4ImmField = Field 8 0

mkTImm0508S4 :: Word32 -> TImm0508S4
mkTImm0508S4 w = TImm0508S4 (fromIntegral imm)
  where
    imm = extract tImm0508S4ImmField w

tImm0508S4ToBits :: TImm0508S4 -> Word32
tImm0508S4ToBits (TImm0508S4 imm) =
    insert tImm0508S4ImmField imm 0

data TImm01020S4 =
    TImm01020S4 { tImm01020S4Imm :: Word8
               }
               deriving (Eq, Ord, Show)

instance PP.Pretty TImm01020S4 where
  pPrint m =
      PP.brackets $ PP.char '#' <> PP.pPrint (((fromIntegral $ tImm01020S4Imm m) :: Word32) `shiftL` 2)

tImm01020S4ImmField :: Field
tImm01020S4ImmField = Field 8 0

mkTImm01020S4 :: Word32 -> TImm01020S4
mkTImm01020S4 w = TImm01020S4 (fromIntegral imm)
  where
    imm = extract tImm01020S4ImmField w

tImm01020S4ToBits :: TImm01020S4 -> Word32
tImm01020S4ToBits (TImm01020S4 imm) =
    insert tImm01020S4ImmField imm 0

data AddrModeSp =
    AddrModeSp { addrModeSpImm :: Word8
               }
               deriving (Eq, Ord, Show)

instance PP.Pretty AddrModeSp where
  pPrint m =
      PP.brackets $ PP.text "sp," PP.<+> PP.pPrint (((fromIntegral $ addrModeSpImm m) :: Word32) `shiftL` 2)

addrModeSpImmField :: Field
addrModeSpImmField = Field 8 0

mkAddrModeSp :: Word32 -> AddrModeSp
mkAddrModeSp w = AddrModeSp (fromIntegral imm)
  where
    imm = extract addrModeSpImmField w

addrModeSpToBits :: AddrModeSp -> Word32
addrModeSpToBits (AddrModeSp imm) =
    insert addrModeSpImmField imm 0

data AddrModePc =
    AddrModePc { addrModePcImm :: Word8
               }
               deriving (Eq, Ord, Show)

instance PP.Pretty AddrModePc where
  pPrint m =
      PP.brackets $ PP.text "pc," PP.<+> PP.pPrint (((fromIntegral $ addrModePcImm m) :: Word32) `shiftL` 2)

addrModePcImmField :: Field
addrModePcImmField = Field 8 0

mkAddrModePc :: Word32 -> AddrModePc
mkAddrModePc w = AddrModePc (fromIntegral imm)
  where
    imm = extract addrModePcImmField w

addrModePcToBits :: AddrModePc -> Word32
addrModePcToBits (AddrModePc imm) =
    insert addrModePcImmField imm 0

data T2SoImm = T2SoImm { t2SoImm :: Word16 }
             deriving (Eq, Ord, Show)

-- See the ARM ARM DDI 0406C.b A6.3.2, "Operation of modified immediate
-- constants, Thumb instructions", ThumbExpandImm() definition.
thumbExpandImm :: Word16 -> Word32
thumbExpandImm imm12 = imm32
    where
        mask1 = 0b11 `shiftL` 10

        imm32 = if imm12 .&. mask1 == 0
                then a
                else b

        a = let mask = 0b11111111
                low8 = fromIntegral $ imm12 .&. mask
            in case extract (Field 2 8) (fromIntegral imm12) of
                0b00 -> low8
                0b01 -> (low8 `shiftL` 16) .|.
                         low8
                0b10 -> (low8 `shiftL` 24) .|.
                        (low8 `shiftL` 8)
                0b11 -> (low8 `shiftL` 24) .|.
                        (low8 `shiftL` 16) .|.
                        (low8 `shiftL` 8)  .|.
                         low8
                _ -> error "BUG: invalid field value in thumbExpandImm"

        b = let low7 = (fromIntegral imm12) .&. 0b1111111
                unrotated_value = (0b1 `shiftL` 7) .|. low7
                rot = fromIntegral $ extract (Field 5 7) (fromIntegral imm12)
            in unrotated_value `rotateR` rot

instance PP.Pretty T2SoImm where
    pPrint (T2SoImm v) = PP.text $ "#" <> (show $ thumbExpandImm v)

mkT2SoImm :: Word32 -> T2SoImm
mkT2SoImm w = T2SoImm $ fromIntegral w

t2SoImmToBits :: T2SoImm -> Word32
t2SoImmToBits (T2SoImm w) = fromIntegral w

data BfInvMaskImm =
    BfInvMaskImm { bfInvMaskImmLsbits :: Word8
                 , bfInvMaskImmMsbits :: Word8
                 }
                 deriving (Eq, Ord, Show)

instance PP.Pretty BfInvMaskImm where
    pPrint (BfInvMaskImm l m) =
        let w = ((fromIntegral m) :: Int) + 1 - lsb
            lsb = ((fromIntegral l) :: Int)
        in (PP.char '#' <> PP.text (show lsb) <> PP.char ',') PP.<+>
           (PP.char '#' <> PP.text (show w))

bfInvMaskImmLsbitsField :: Field
bfInvMaskImmLsbitsField = Field 5 0

bfInvMaskImmMsbitsField :: Field
bfInvMaskImmMsbitsField = Field 5 5

mkBfInvMaskImm :: Word32 -> BfInvMaskImm
mkBfInvMaskImm w = BfInvMaskImm (fromIntegral l) (fromIntegral m)
  where
      m = extract bfInvMaskImmMsbitsField w
      l = extract bfInvMaskImmLsbitsField w

bfInvMaskImmToBits :: BfInvMaskImm -> Word32
bfInvMaskImmToBits (BfInvMaskImm l m) =
    insert bfInvMaskImmMsbitsField m $
    insert bfInvMaskImmLsbitsField l 0

data T2AddrModeSoReg =
    T2AddrModeSoReg { t2AddrModeSoRegShiftAmt  :: Word8
                    , t2AddrModeSoRegRm        :: GPR
                    , t2AddrModeSoRegRn        :: GPR
                    }
                    deriving (Eq, Ord, Show)

instance PP.Pretty T2AddrModeSoReg where
    pPrint (T2AddrModeSoReg s rm rn) =
        PP.brackets $ (PP.pPrint rn <> PP.char ',') PP.<+>
                      (PP.pPrint rm <>
                       (if s == 0
                        then mempty
                        else PP.text ", lsl #" <> PP.pPrint s))

t2AddrModeSoRegRmField :: Field
t2AddrModeSoRegRmField = Field 4 2

t2AddrModeSoRegShiftAmtField :: Field
t2AddrModeSoRegShiftAmtField = Field 2 0

t2AddrModeSoRegRnField :: Field
t2AddrModeSoRegRnField = Field 4 6

mkT2AddrModeSoReg :: Word32 -> T2AddrModeSoReg
mkT2AddrModeSoReg w = T2AddrModeSoReg (fromIntegral s) (GPR $ fromIntegral rm) (GPR $ fromIntegral rn)
  where
      rm  = extract t2AddrModeSoRegRmField w
      rn  = extract t2AddrModeSoRegRnField w
      s   = extract t2AddrModeSoRegShiftAmtField w

t2AddrModeSoRegToBits :: T2AddrModeSoReg -> Word32
t2AddrModeSoRegToBits (T2AddrModeSoReg s (GPR rm) (GPR rn)) =
    insert t2AddrModeSoRegRmField rm $
    insert t2AddrModeSoRegRnField rn $
    insert t2AddrModeSoRegShiftAmtField s 0

data Imm1_32 = Imm1_32 { unImm1_32 :: Word8
                       }
                       deriving (Eq, Ord, Show)

instance PP.Pretty Imm1_32 where
  pPrint v = PP.char '#' <> (PP.pPrint (unImm1_32 v + 1))

imm1_32Field :: Field
imm1_32Field = Field 5 0

mkImm1_32 :: Word32 -> Imm1_32
mkImm1_32 w = Imm1_32 $ fromIntegral i
  where
    i = extract imm1_32Field w

imm1_32ToBits :: Imm1_32 -> Word32
imm1_32ToBits (Imm1_32 i) =
    insert imm1_32Field (fromIntegral i) 0

data T2SoReg =
    T2SoReg { t2SoRegImm5      :: Word8
            , t2SoRegShiftType :: ARM.ShiftType
            , t2SoRegRm        :: GPR
            }
            deriving (Eq, Ord, Show)

instance PP.Pretty T2SoReg where
    pPrint _ = PP.text "not implemented 6"

t2SoRegImm3Field :: Field
t2SoRegImm3Field = Field 3 9

t2SoRegImm2Field :: Field
t2SoRegImm2Field = Field 2 7

t2SoRegShiftTypeField :: Field
t2SoRegShiftTypeField = Field 2 5

t2SoRegRmField :: Field
t2SoRegRmField = Field 4 0

mkT2SoReg :: Word32 -> T2SoReg
mkT2SoReg w = T2SoReg (fromIntegral imm) st (GPR $ fromIntegral reg)
  where
      reg  = extract t2SoRegRmField w
      imm2 = extract t2SoRegImm2Field w
      imm3 = extract t2SoRegImm3Field w
      imm = (imm3 `shiftL` 2) .|. imm2
      st   = ARM.decodeShiftType $ extract t2SoRegShiftTypeField w

t2SoRegToBits :: T2SoReg -> Word32
t2SoRegToBits (T2SoReg imm st (GPR reg)) =
    let imm3 = imm `shiftR` 2
        imm2 = imm .&. 0b11
    in insert t2SoRegRmField reg $
       insert t2SoRegImm2Field imm2 $
       insert t2SoRegImm3Field imm3 $
       insert t2SoRegShiftTypeField (ARM.encodeShiftType st) 0

data ThumbBlxTarget =
    ThumbBlxTarget { _thumbBlxTargetS      :: Word8
                   , _thumbBlxTargetImm10H :: Word16
                   , _thumbBlxTargetJ1     :: Word8
                   , _thumbBlxTargetJ2     :: Word8
                   , _thumbBlxTargetImm10L :: Word16
                   }
                   deriving (Eq, Ord, Show)

instance PP.Pretty ThumbBlxTarget where
    pPrint _ = PP.text "not implemented 7"

thumbBlxTargetSField :: Field
thumbBlxTargetSField = Field 1 23

thumbBlxTargetImm10HField :: Field
thumbBlxTargetImm10HField = Field 10 11

thumbBlxTargetJ1Field :: Field
thumbBlxTargetJ1Field = Field 1 22

thumbBlxTargetJ2Field :: Field
thumbBlxTargetJ2Field = Field 1 21

thumbBlxTargetImm10LField :: Field
thumbBlxTargetImm10LField = Field 10 1

mkThumbBlxTarget :: Word32 -> ThumbBlxTarget
mkThumbBlxTarget w = ThumbBlxTarget (fromIntegral s)
                                    (fromIntegral imm10h)
                                    (fromIntegral j1)
                                    (fromIntegral j2)
                                    (fromIntegral imm10l)
  where
      s      = extract thumbBlxTargetSField w
      imm10h = extract thumbBlxTargetImm10HField w
      j1     = extract thumbBlxTargetJ1Field w
      j2     = extract thumbBlxTargetJ2Field w
      imm10l = extract thumbBlxTargetImm10LField w

thumbBlxTargetToBits :: ThumbBlxTarget -> Word32
thumbBlxTargetToBits (ThumbBlxTarget s imm10h j1 j2 imm10l) =
    insert thumbBlxTargetSField s $
    insert thumbBlxTargetImm10HField imm10h $
    insert thumbBlxTargetJ1Field j1 $
    insert thumbBlxTargetJ2Field j2 $
    insert thumbBlxTargetImm10LField imm10l 0

data ThumbBlTarget =
    ThumbBlTarget { _thumbBlTargetS     :: Word8
                  , _thumbBlTargetImm10 :: Word16
                  , _thumbBlTargetJ1    :: Word8
                  , _thumbBlTargetJ2    :: Word8
                  , _thumbBlTargetImm11 :: Word16
                  }
                  deriving (Eq, Ord, Show)

instance PP.Pretty ThumbBlTarget where
    pPrint _ = PP.text "not implemented 8"

thumbBlTargetSField :: Field
thumbBlTargetSField = Field 1 23

thumbBlTargetImm10Field :: Field
thumbBlTargetImm10Field = Field 10 11

thumbBlTargetJ1Field :: Field
thumbBlTargetJ1Field = Field 1 22

thumbBlTargetJ2Field :: Field
thumbBlTargetJ2Field = Field 1 21

thumbBlTargetImm11Field :: Field
thumbBlTargetImm11Field = Field 11 0

mkThumbBlTarget :: Word32 -> ThumbBlTarget
mkThumbBlTarget w = ThumbBlTarget (fromIntegral s)
                                  (fromIntegral imm10)
                                  (fromIntegral j1)
                                  (fromIntegral j2)
                                  (fromIntegral imm11)
  where
      s     = extract thumbBlTargetSField w
      imm10 = extract thumbBlTargetImm10Field w
      j1    = extract thumbBlTargetJ1Field w
      j2    = extract thumbBlTargetJ2Field w
      imm11 = extract thumbBlTargetImm11Field w

thumbBlTargetToBits :: ThumbBlTarget -> Word32
thumbBlTargetToBits (ThumbBlTarget s imm10 j1 j2 imm11) =
    insert thumbBlTargetSField s $
    insert thumbBlTargetImm10Field imm10 $
    insert thumbBlTargetJ1Field j1 $
    insert thumbBlTargetJ2Field j2 $
    insert thumbBlTargetImm11Field imm11 0

data TBrTarget =
    TBrTarget { tBrTargetImm :: Word16
              }
              deriving (Eq, Ord, Show)

instance PP.Pretty TBrTarget where
  pPrint m = PP.pPrint (((fromIntegral $ tBrTargetImm m) :: Word32) `shiftL` 2)

tBrTargetImmField :: Field
tBrTargetImmField = Field 11 0

mkTBrTarget :: Word32 -> TBrTarget
mkTBrTarget w = TBrTarget (fromIntegral imm)
  where
    imm = extract tBrTargetImmField w

tBrTargetToBits :: TBrTarget -> Word32
tBrTargetToBits (TBrTarget imm) =
    insert tBrTargetImmField imm 0

-- | An T2AddrMode_Imm8Pre memory reference for a load or store instruction
-- (with a 8-bit immediate)
data T2AddrModeImm8Pre = T2AddrModeImm8Pre { t2AddrModeImm8PreRn   :: GPR
                                           , t2AddrModeImm8PreImm8 :: Word8
                                           , t2AddrModeImm8PreUBit :: Word8
                                           }
  deriving (Eq, Ord, Show)

instance PP.Pretty T2AddrModeImm8Pre where
  pPrint _ = PP.text "not implemented 9"

t2AddrModeImm8PreRegField :: Field
t2AddrModeImm8PreRegField = Field 4 9

t2AddrModeImm8PreUBitField :: Field
t2AddrModeImm8PreUBitField = Field 1 8

t2AddrModeImm8PreImmField :: Field
t2AddrModeImm8PreImmField = Field 8 0

mkT2AddrModeImm8Pre :: Word32 -> T2AddrModeImm8Pre
mkT2AddrModeImm8Pre w = T2AddrModeImm8Pre (GPR $ fromIntegral reg) (fromIntegral imm) (fromIntegral u)
  where
    reg = extract t2AddrModeImm8PreRegField w
    imm = extract t2AddrModeImm8PreImmField w
    u   = extract t2AddrModeImm8PreUBitField w

t2AddrModeImm8PreToBits :: T2AddrModeImm8Pre -> Word32
t2AddrModeImm8PreToBits (T2AddrModeImm8Pre (GPR r) imm u) =
    insert t2AddrModeImm8PreRegField r $
    insert t2AddrModeImm8PreUBitField u $
    insert t2AddrModeImm8PreImmField imm 0

-- | An T2AddrMode_NegImm8 memory reference for a load or store instruction
-- (with a 8-bit immediate)
data T2AddrModeNegImm8 = T2AddrModeNegImm8 { t2AddrModeNegImm8Register  :: GPR
                                           , t2AddrModeNegImm8Immediate :: Word8
                                           , t2AddrModeNegImm8Add       :: Word8
                                           }
  deriving (Eq, Ord, Show)

instance PP.Pretty T2AddrModeNegImm8 where
  pPrint _ = PP.text "not implemented 10"

t2AddrModeNegImm8RegField :: Field
t2AddrModeNegImm8RegField = Field 4 9

t2AddrModeNegImm8AddField :: Field
t2AddrModeNegImm8AddField = Field 1 9

t2AddrModeNegImm8ImmField :: Field
t2AddrModeNegImm8ImmField = Field 8 0

mkT2AddrModeNegImm8 :: Word32 -> T2AddrModeNegImm8
mkT2AddrModeNegImm8 w = T2AddrModeNegImm8 (GPR $ fromIntegral reg) (fromIntegral imm) (fromIntegral a)
  where
    reg = extract t2AddrModeNegImm8RegField w
    a   = extract t2AddrModeNegImm8AddField w
    imm = extract t2AddrModeNegImm8ImmField w

t2AddrModeNegImm8ToBits :: T2AddrModeNegImm8 -> Word32
t2AddrModeNegImm8ToBits (T2AddrModeNegImm8 (GPR r) imm a) =
    insert t2AddrModeNegImm8RegField r $
    insert t2AddrModeNegImm8AddField a $
    insert t2AddrModeNegImm8ImmField imm 0

-- | An T2AddrMode_Imm8S4Offset memory reference for a load or store instruction
-- (with a 8S4-bit immediate)
data T2AddrModeImm8S4Offset =
    T2AddrModeImm8S4Offset { t2AddrModeImm8S4OffsetAdd :: Word8
                           , t2AddrModeImm8S4OffsetImm :: Word8
                           }
                           deriving (Eq, Ord, Show)

instance PP.Pretty T2AddrModeImm8S4Offset where
  pPrint _ = PP.text "not implemented 11"

t2AddrModeImm8S4OffsetAddField :: Field
t2AddrModeImm8S4OffsetAddField = Field 1 8

t2AddrModeImm8S4OffsetImmField :: Field
t2AddrModeImm8S4OffsetImmField = Field 8 0

mkT2AddrModeImm8S4Offset :: Word32 -> T2AddrModeImm8S4Offset
mkT2AddrModeImm8S4Offset w = T2AddrModeImm8S4Offset (fromIntegral add) (fromIntegral imm)
  where
    add = extract t2AddrModeImm8S4OffsetAddField w
    imm = extract t2AddrModeImm8S4OffsetImmField w

t2AddrModeImm8S4OffsetToBits :: T2AddrModeImm8S4Offset -> Word32
t2AddrModeImm8S4OffsetToBits (T2AddrModeImm8S4Offset add imm) =
    insert t2AddrModeImm8S4OffsetAddField add $
    insert t2AddrModeImm8S4OffsetImmField imm 0

-- | An T2AddrMode_Imm8Offset memory reference for a load or store instruction
-- (with a 8-bit immediate)
data T2AddrModeImm8Offset =
    T2AddrModeImm8Offset { t2AddrModeImm8OffsetAdd :: Word8
                         , t2AddrModeImm8OffsetImm :: Word8
                         }
                         deriving (Eq, Ord, Show)

instance PP.Pretty T2AddrModeImm8Offset where
  pPrint _ = PP.text "not implemented 12"

t2AddrModeImm8OffsetAddField :: Field
t2AddrModeImm8OffsetAddField = Field 1 8

t2AddrModeImm8OffsetImmField :: Field
t2AddrModeImm8OffsetImmField = Field 8 0

mkT2AddrModeImm8Offset :: Word32 -> T2AddrModeImm8Offset
mkT2AddrModeImm8Offset w = T2AddrModeImm8Offset (fromIntegral add) (fromIntegral imm)
  where
    add = extract t2AddrModeImm8OffsetAddField w
    imm = extract t2AddrModeImm8OffsetImmField w

t2AddrModeImm8OffsetToBits :: T2AddrModeImm8Offset -> Word32
t2AddrModeImm8OffsetToBits (T2AddrModeImm8Offset add imm) =
    insert t2AddrModeImm8OffsetAddField add $
    insert t2AddrModeImm8OffsetImmField imm 0

-- | An T2AddrMode_Imm8 memory reference for a load or store instruction
-- (with a 8-bit immediate)
data T2AddrModeImm8 = T2AddrModeImm8 { t2AddrModeImm8Register  :: GPR
                                     , t2AddrModeImm8Immediate :: Word8
                                     }
  deriving (Eq, Ord, Show)

instance PP.Pretty T2AddrModeImm8 where
  pPrint m =
      let suf = PP.char ',' PP.<+> ((PP.char '#') <> (PP.pPrint $ t2AddrModeImm8Immediate m))
      in PP.brackets $ PP.pPrint (t2AddrModeImm8Register m) <> suf

t2AddrModeImm8RegField :: Field
t2AddrModeImm8RegField = Field 4 8

t2AddrModeImm8ImmField :: Field
t2AddrModeImm8ImmField = Field 8 0

mkT2AddrModeImm8 :: Word32 -> T2AddrModeImm8
mkT2AddrModeImm8 w = T2AddrModeImm8 (GPR $ fromIntegral reg) (fromIntegral imm)
  where
    reg = extract t2AddrModeImm8RegField w
    imm = extract t2AddrModeImm8ImmField w

t2AddrModeImm8ToBits :: T2AddrModeImm8 -> Word32
t2AddrModeImm8ToBits (T2AddrModeImm8 (GPR r) imm) =
    insert t2AddrModeImm8RegField r $
    insert t2AddrModeImm8ImmField imm 0

-- | An T2AddrMode_Imm12 memory reference for a load or store instruction
-- (with a 12-bit immediate)
data T2AddrModeImm12 = T2AddrModeImm12 { t2AddrModeImm12Register  :: GPR
                                       , t2AddrModeImm12Immediate :: Word16
                                       }
  deriving (Eq, Ord, Show)

instance PP.Pretty T2AddrModeImm12 where
  pPrint m =
      let suf = PP.char ',' PP.<+> ((PP.char '#') <> (PP.pPrint $ t2AddrModeImm12Immediate m))
      in PP.brackets $ PP.pPrint (t2AddrModeImm12Register m) <> suf

t2AddrModeImm12RegField :: Field
t2AddrModeImm12RegField = Field 4 13

t2AddrModeImm12ImmField :: Field
t2AddrModeImm12ImmField = Field 12 0

mkT2AddrModeImm12 :: Word32 -> T2AddrModeImm12
mkT2AddrModeImm12 w = T2AddrModeImm12 (GPR $ fromIntegral reg) (fromIntegral imm)
  where
    reg = extract t2AddrModeImm12RegField w
    imm = extract t2AddrModeImm12ImmField w

t2AddrModeImm12ToBits :: T2AddrModeImm12 -> Word32
t2AddrModeImm12ToBits (T2AddrModeImm12 (GPR r) imm) =
    insert t2AddrModeImm12RegField r $
    insert t2AddrModeImm12ImmField imm 0

data AddrModeRr =
    AddrModeRr { _addrModeRmReg :: LowGPR
               , _addrModeRnReg :: LowGPR
               }
               deriving (Eq, Ord, Show)

instance PP.Pretty AddrModeRr where
  pPrint (AddrModeRr rm rn) =
      PP.brackets $ PP.pPrint rn <> (PP.text "," PP.<+> PP.pPrint rm)

addrModeRrRmField :: Field
addrModeRrRmField = Field 3 3

addrModeRrRnField :: Field
addrModeRrRnField = Field 3 0

mkAddrModeRr :: Word32 -> AddrModeRr
mkAddrModeRr w = AddrModeRr (LowGPR $ fromIntegral rm) (LowGPR $ fromIntegral rn)
  where
    rm = extract addrModeRrRmField w
    rn = extract addrModeRrRnField w

addrModeRrToBits :: AddrModeRr -> Word32
addrModeRrToBits (AddrModeRr (LowGPR rm) (LowGPR rn)) =
    insert addrModeRrRmField rm $
    insert addrModeRrRnField rn 0

data AddrModeImm01020S4 =
    AddrModeImm01020S4 { _addrModeImm01020S4Reg :: GPR
                       , _addrModeImm01020S4Imm :: Word8
                       }
                       deriving (Eq, Ord, Show)

instance PP.Pretty AddrModeImm01020S4 where
  pPrint (AddrModeImm01020S4 reg imm) =
      PP.pPrint reg <> (PP.text ", " PP.<+> PP.pPrint (((fromIntegral imm) :: Word32) `shiftL` 2))

addrModeImm01020S4RegField :: Field
addrModeImm01020S4RegField = Field 4 8

addrModeImm01020S4ImmField :: Field
addrModeImm01020S4ImmField = Field 8 0

mkAddrModeImm01020S4 :: Word32 -> AddrModeImm01020S4
mkAddrModeImm01020S4 w = AddrModeImm01020S4 (GPR $ fromIntegral rm) (fromIntegral imm)
  where
    rm = extract addrModeImm01020S4RegField w
    imm = extract addrModeImm01020S4ImmField w

addrModeImm01020S4ToBits :: AddrModeImm01020S4 -> Word32
addrModeImm01020S4ToBits (AddrModeImm01020S4 (GPR rm) imm) =
    insert addrModeImm01020S4RegField rm $
    insert addrModeImm01020S4ImmField imm 0

data ITMask = ITMask { _unITMask :: Word8
                     }
                     deriving (Eq, Ord, Show)

instance PP.Pretty ITMask where
    -- See the ARM ARM DDI 0406C.b, A8-391
    pPrint (ITMask 0b1110) = PP.text ""
    pPrint _               = PP.text "<UND>"

mkITMask :: Word32 -> ITMask
mkITMask = ITMask . fromIntegral

itMaskToBits :: ITMask -> Word32
itMaskToBits (ITMask p) = fromIntegral p

-- | Four-bit condition flag sequence
data Pred = Pred { _unPred :: Word8
                 }
  deriving (Eq, Ord, Show)

instance PP.Pretty Pred where
    -- See the ARM ARM DDI 0406C.b, A8.3 Conditional Execution
    pPrint (Pred 0b0000) = PP.text "eq"
    pPrint (Pred 0b0001) = PP.text "ne"
    pPrint (Pred 0b0010) = PP.text "cs"
    pPrint (Pred 0b0011) = PP.text "cc"
    pPrint (Pred 0b0100) = PP.text "mi"
    pPrint (Pred 0b0101) = PP.text "pl"
    pPrint (Pred 0b0110) = PP.text "vs"
    pPrint (Pred 0b0111) = PP.text "vc"
    pPrint (Pred 0b1000) = PP.text "hi"
    pPrint (Pred 0b1001) = PP.text "ls"
    pPrint (Pred 0b1010) = PP.text "ge"
    pPrint (Pred 0b1011) = PP.text "lt"
    pPrint (Pred 0b1100) = PP.text "gt"
    pPrint (Pred 0b1101) = PP.text "le"
    pPrint (Pred 0b1110) = PP.text ""
    pPrint _             = PP.text "<UND>"

mkPred :: Word32 -> Pred
mkPred = Pred . fromIntegral

predToBits :: Pred -> Word32
predToBits (Pred p) = fromIntegral p

instance A.Arbitrary GPR where
  arbitrary g = GPR <$> A.uniformR (0, 15) g

instance A.Arbitrary Opcode where
  arbitrary g = Opcode <$> A.uniformR (0, 7) g

instance A.Arbitrary Pred where
  arbitrary g = Pred <$> A.uniformR (0, 16) g

instance A.Arbitrary Bit where
  arbitrary g = Bit <$> A.uniformR (0, 1) g

instance A.Arbitrary Reglist where
  arbitrary g = Reglist <$> A.arbitrary g

instance A.Arbitrary LowGPR where
  arbitrary g = LowGPR <$> A.arbitrary g

instance A.Arbitrary AddrModeIs1 where
  arbitrary g = AddrModeIs1 <$> A.arbitrary g
                            <*> A.arbitrary g

instance A.Arbitrary AddrModeIs2 where
  arbitrary g = AddrModeIs2 <$> A.arbitrary g
                            <*> A.arbitrary g

instance A.Arbitrary AddrModeIs4 where
  arbitrary g = AddrModeIs4 <$> A.arbitrary g
                            <*> A.arbitrary g

instance A.Arbitrary AddrModePc where
  arbitrary g = AddrModePc <$> A.arbitrary g

instance A.Arbitrary AddrModeSp where
  arbitrary g = AddrModeSp <$> A.arbitrary g

instance A.Arbitrary TAdrLabel where
  arbitrary g = TAdrLabel <$> A.arbitrary g

instance A.Arbitrary TBrTarget where
  arbitrary g = TBrTarget <$> A.arbitrary g

instance A.Arbitrary ThumbBlxTarget where
  arbitrary g = ThumbBlxTarget <$> A.arbitrary g
                               <*> A.arbitrary g
                               <*> A.arbitrary g
                               <*> A.arbitrary g
                               <*> A.arbitrary g

instance A.Arbitrary ThumbBlTarget where
  arbitrary g = ThumbBlTarget <$> A.arbitrary g
                              <*> A.arbitrary g
                              <*> A.arbitrary g
                              <*> A.arbitrary g
                              <*> A.arbitrary g

instance A.Arbitrary AddrModeRr where
  arbitrary g = AddrModeRr <$> A.arbitrary g
                           <*> A.arbitrary g

instance A.Arbitrary AddrModeImm01020S4 where
  arbitrary g = AddrModeImm01020S4 <$> A.arbitrary g
                                   <*> A.arbitrary g

instance A.Arbitrary T2AddrModeImm12 where
  arbitrary g = T2AddrModeImm12 <$> A.arbitrary g
                                <*> A.arbitrary g

instance A.Arbitrary T2SoReg where
  arbitrary g = T2SoReg <$> A.arbitrary g
                        <*> A.arbitrary g
                        <*> A.arbitrary g

instance A.Arbitrary T2AddrModeImm8 where
  arbitrary g = T2AddrModeImm8 <$> A.arbitrary g
                               <*> A.arbitrary g

instance A.Arbitrary T2AddrModeImm8Pre where
  arbitrary g = T2AddrModeImm8Pre <$> A.arbitrary g
                                  <*> A.arbitrary g
                                  <*> A.arbitrary g

instance A.Arbitrary T2AddrModeImm8S4 where
  arbitrary g = T2AddrModeImm8S4 <$> A.arbitrary g
                                 <*> A.arbitrary g
                                 <*> A.arbitrary g

instance A.Arbitrary T2AddrModeImm8S4Pre where
  arbitrary g = T2AddrModeImm8S4Pre <$> A.arbitrary g
                                    <*> A.arbitrary g
                                    <*> A.arbitrary g

instance A.Arbitrary T2AddrModeNegImm8 where
  arbitrary g = T2AddrModeNegImm8 <$> A.arbitrary g
                                  <*> A.arbitrary g
                                  <*> A.arbitrary g

instance A.Arbitrary T2AddrModeSoReg where
  arbitrary g = T2AddrModeSoReg <$> A.arbitrary g
                                <*> A.arbitrary g
                                <*> A.arbitrary g

instance A.Arbitrary T2AdrLabel where
  arbitrary g = T2AdrLabel <$> A.arbitrary g
                           <*> A.arbitrary g

instance A.Arbitrary T2LdrLabel where
  arbitrary g = T2LdrLabel <$> A.arbitrary g
                           <*> A.arbitrary g

instance A.Arbitrary T2AddrModeImm8Offset where
  arbitrary g = T2AddrModeImm8Offset <$> A.arbitrary g
                                     <*> A.arbitrary g

instance A.Arbitrary T2AddrModeImm8S4Offset where
  arbitrary g = T2AddrModeImm8S4Offset <$> A.arbitrary g
                                       <*> A.arbitrary g

instance A.Arbitrary T2SoImm where
  arbitrary g = T2SoImm <$> A.arbitrary g

instance A.Arbitrary TImm0508S4 where
  arbitrary g = TImm0508S4 <$> A.arbitrary g

instance A.Arbitrary TImm01020S4 where
  arbitrary g = TImm01020S4 <$> A.arbitrary g

instance A.Arbitrary Imm1_32 where
  arbitrary g = Imm1_32 <$> A.arbitrary g

instance A.Arbitrary BfInvMaskImm where
  arbitrary g = BfInvMaskImm <$> A.arbitrary g
                             <*> A.arbitrary g
