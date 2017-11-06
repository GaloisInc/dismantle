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

  TBrTarget,
  mkTBrTarget,
  tBrTargetToBits,

  Opcode,
  mkOpcode,
  opcodeToBits,

  TAdrLabel,
  mkTAdrLabel,
  tAdrLabelToBits,

  ThumbBlTarget,
  mkThumbBlTarget,
  thumbBlTargetToBits,

  ThumbBlxTarget,
  mkThumbBlxTarget,
  thumbBlxTargetToBits,

  AddrModeIs1,
  mkAddrModeIs1,
  addrModeIs1ToBits,

  AddrModeIs2,
  mkAddrModeIs2,
  addrModeIs2ToBits,

  AddrModeIs4,
  mkAddrModeIs4,
  addrModeIs4ToBits,

  AddrModePc,
  mkAddrModePc,
  addrModePcToBits,

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

data AddrModeIs1 =
    AddrModeIs1 { addrModeIs1Reg :: LowGPR
                , addrModeIs1Imm :: Word8
                }
                deriving (Eq, Ord, Show)

instance PP.Pretty AddrModeIs1 where
  pPrint m =
      let suf = if addrModeIs1Imm m == 0
                then mempty
                else PP.char ',' PP.<+> ((PP.char '#') <> (PP.pPrint $ addrModeIs1Imm m))
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
      let suf = if addrModeIs2Imm m == 0
                then mempty
                else PP.char ',' PP.<+> ((PP.char '#') <> (PP.pPrint $ addrModeIs2Imm m))
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
      let suf = if addrModeIs4Imm m == 0
                then mempty
                else PP.char ',' PP.<+> ((PP.char '#') <> (PP.pPrint $ addrModeIs4Imm m))
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

data AddrModePc =
    AddrModePc { addrModePcImm :: Word8
               }
               deriving (Eq, Ord, Show)

instance PP.Pretty AddrModePc where
  pPrint m = PP.pPrint (((fromIntegral $ addrModePcImm m) :: Word32) `shiftL` 2)

addrModePcImmField :: Field
addrModePcImmField = Field 8 0

mkAddrModePc :: Word32 -> AddrModePc
mkAddrModePc w = AddrModePc (fromIntegral imm)
  where
    imm = extract addrModePcImmField w

addrModePcToBits :: AddrModePc -> Word32
addrModePcToBits (AddrModePc imm) =
    insert addrModeIs4ImmField imm 0

data T2SoReg =
    T2SoReg { t2SoRegImm5      :: Word8
            , t2SoRegShiftType :: ARM.ShiftType
            , t2SoRegRm        :: GPR
            }

instance PP.Pretty T2SoReg where
    pPrint _ = PP.text "not implemented"

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
    pPrint _ = PP.text "not implemented"

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
    pPrint _ = PP.text "not implemented"

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
    TBrTarget { tBrTargetImm :: Word8
              }
              deriving (Eq, Ord, Show)

instance PP.Pretty TBrTarget where
  pPrint m = PP.pPrint (((fromIntegral $ tBrTargetImm m) :: Word32) `shiftL` 2)

tBrTargetImmField :: Field
tBrTargetImmField = Field 8 0

mkTBrTarget :: Word32 -> TBrTarget
mkTBrTarget w = TBrTarget (fromIntegral imm)
  where
    imm = extract tBrTargetImmField w

tBrTargetToBits :: TBrTarget -> Word32
tBrTargetToBits (TBrTarget imm) =
    insert addrModeIs4ImmField imm 0

data AddrModeRr =
    AddrModeRr { _addrModeRmReg :: LowGPR
               , _addrModeRnReg :: LowGPR
               }
               deriving (Eq, Ord, Show)

instance PP.Pretty AddrModeRr where
  pPrint (AddrModeRr rm rn) =
      PP.pPrint rm <> (PP.text ", " PP.<+> PP.pPrint rn)

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
