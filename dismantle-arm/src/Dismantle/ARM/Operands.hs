{-# OPTIONS_HADDOCK not-home #-}
module Dismantle.ARM.Operands (
  GPR(..),
  DPR(..),
  QPR(..),
  QQPR(..),
  CR(..),
  FR(..),
  VR(..),
  AddrMode3(..),
  mkAddrMode3,
  addrMode3ToBits
  ) where

import Data.Bits
import Data.Monoid
import Data.Word ( Word8, Word16, Word32 )

import qualified Text.PrettyPrint.HughesPJClass as PP

import Dismantle.Tablegen.TH.Pretty

newtype CR = CR { unCR :: Word8 }
  deriving (Eq, Ord, Show)

-- | Floating-point register by number
newtype FR = FR { unFR :: Word8 }
  deriving (Eq, Ord, Show)

-- | General-purpose register by number
newtype GPR = GPR { unGPR :: Word8 }
  deriving (Eq, Ord, Show)

-- | Double-precision register by number
newtype DPR = DPR { unDPR :: Word8 }
  deriving (Eq, Ord, Show)

-- | 128-bit vector register by number
newtype QPR = QPR { unQPR :: Word8 }
  deriving (Eq, Ord, Show)

-- | 256-bit vector register (128-bit register pair) by number (must be
-- even)
newtype QQPR = QQPR { unQQPR :: Word8 }
  deriving (Eq, Ord, Show)

-- | Vector register by number
newtype VR = VR { unVR :: Word8 }
  deriving (Eq, Ord, Show)

data Field = Field { fieldBits :: Int
                   , fieldOffset :: Int
                   }

addrMode3RegField :: Field
addrMode3RegField = Field 4 16

addrMode3AddField :: Field
addrMode3AddField = Field 1 23

addrMode3ImmLField :: Field
addrMode3ImmLField = Field 4 0

addrMode3ImmHField :: Field
addrMode3ImmHField = Field 4 8

mkAddrMode3 :: Word32 -> AddrMode3
mkAddrMode3 w = AddrMode3 (GPR $ fromIntegral reg) (fromIntegral imm) (add == 1)
  where
    reg   = extract addrMode3RegField w
    add   = extract addrMode3AddField w
    imm4L = extract addrMode3ImmLField w
    imm4H = extract addrMode3ImmHField w
    imm   = imm4L .|. (imm4H `shiftL` 4)

addrMode3ToBits :: AddrMode3 -> Word32
addrMode3ToBits (AddrMode3 (GPR r) imm add) =
    insert addrMode3RegField r $
    insert addrMode3AddField (if add then 1 else 0) $
    insert addrMode3ImmLField imm $
    insert addrMode3ImmHField (imm `shiftR` 4) 0

mkMask :: Field -> Word32
mkMask (Field bits offset) = (2 ^ bits - 1) `shiftL` offset

insert :: (Integral a) => Field -> a -> Word32 -> Word32
insert (Field bits offset) src dest =
    dest .|. (((fromIntegral src) .&. (2 ^ bits - 1)) `shiftL` offset)

extract :: Field -> Word32 -> Word32
extract f val = (val .&. (mkMask f)) `shiftR` (fieldOffset f)

-- | An AddrMode3 memory reference for a load or store instruction
data AddrMode3 = AddrMode3 { addrMode3Register  :: GPR
                           , addrMode3Immediate :: Word8
                           , addrMode3Add       :: Bool
                           }
  deriving (Eq, Ord, Show)

instance PP.Pretty GPR where
  pPrint (GPR rno) = PP.char 'r' <> PP.int (fromIntegral rno)

instance PP.Pretty DPR where
  pPrint (DPR rno) = PP.char 'd' <> PP.int (fromIntegral rno)

instance PP.Pretty QPR where
  pPrint (QPR rno) = PP.char 'q' <> PP.int (fromIntegral rno)

instance PP.Pretty QQPR where
  pPrint (QQPR rno) = PP.char 'q' <> PP.int (fromIntegral rno)

instance PP.Pretty CR where
  pPrint (CR rno) = PP.char 'c' <> PP.int (fromIntegral rno)

instance PP.Pretty FR where
  pPrint (FR rno) = PP.char 'f' <> PP.int (fromIntegral rno)

instance PP.Pretty VR where
  pPrint (VR rno) = PP.char 'v' <> PP.int (fromIntegral rno)

instance PP.Pretty AddrMode3 where
  pPrint m =
      let opStr = if addrMode3Add m then mempty else PP.char '-'
      in (PP.pPrint (addrMode3Register m) <> PP.char ',') PP.<+>
         (opStr <> PP.pPrint (addrMode3Immediate m))
