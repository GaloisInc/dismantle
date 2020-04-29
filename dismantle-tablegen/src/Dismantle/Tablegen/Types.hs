{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveLift #-}
module Dismantle.Tablegen.Types (
  InstructionDescriptor(..),
  OperandDescriptor(..),
  OperandType(..),
  RegisterClass(..),
  ISADescriptor(..),
  IBit(..)
  ) where

import GHC.Generics ( Generic )
import Control.DeepSeq
import Data.List (nub)
import Data.Word ( Word8 )
import Language.Haskell.TH.Syntax (Lift)

import Dismantle.Tablegen.Parser.Types (OBit)
import qualified Dismantle.Tablegen.Patterns as DTP

-- | A bit position in an instruction.
newtype IBit = IBit Int
             deriving (Show, Read, Eq, Generic, NFData, Ord, Lift)

instance Num IBit where
    (IBit a) + (IBit b) = IBit $ a + b
    (IBit a) * (IBit b) = IBit $ a * b
    abs (IBit a) = IBit $ abs a
    signum (IBit a) = IBit $ signum a
    fromInteger = IBit . fromInteger
    negate (IBit a) = IBit (negate a)

-- | The type of data contained in a field operand.
--
-- For now, this is just a wrapper around a string.  Later in the
-- process, we will parse that out using information in the tablegen
-- files.
--
-- Those have definitions for 'DAGOperand's, which will let us
-- classify each operand with great precision.  There are subtypes of
-- DAGOperand:
--
-- * RegisterClass: this defines a *class* of registers
-- * RegisterOperand: references a register class
--
-- It seems like some of the details are ISA-specific, so we don't
-- want to commit to a representation at this point.
data OperandType = OperandType String
                 deriving (Eq, Ord, Show, Generic, NFData)

-- | Description of an operand field in an instruction (could be a
-- register reference or an immediate)
data OperandDescriptor =
  OperandDescriptor { opName :: String
                    , opChunks :: [(IBit, OBit, Word8)]
                    -- ^ (Bit in the instruction, bit in the operand, number of bits in chunk)
                    , opType :: !OperandType
                    }
  deriving (Eq, Ord, Show)

instance NFData OperandDescriptor where
  rnf od = opName od `deepseq` opChunks od `deepseq` od `seq` ()

-- | Description of an instruction, abstracted from the tablegen
-- definition
data InstructionDescriptor =
  InstructionDescriptor { idMask :: [DTP.Bit]
                        , idNegMasks :: [[DTP.Bit]]
                        , idMnemonic :: String
                        , idInputOperands :: [OperandDescriptor]
                        , idOutputOperands :: [OperandDescriptor]
                        , idNamespace :: String
                        , idDecoderNamespace :: String
                        , idAsmString :: String
                        , idPseudo :: Bool
                        , idDefaultPrettyVariableValues :: [(String, String)]
                        , idPrettyVariableOverrides :: [(String, String)]
                        }
  deriving (Eq, Ord, Show, Generic, NFData)

data RegisterClass = RegisterClass String
  deriving (Show, Generic, NFData)

data ISADescriptor =
  ISADescriptor { isaInstructions :: [InstructionDescriptor]
                , isaOperands :: [OperandType]
                -- ^ All of the operand types used in an ISA.
                , isaErrors :: [(String, String)]
                -- ^ Errors while mapping operand classes to bit
                -- fields in the instruction encoding; the first
                -- String is the mnemonic, while the second is the
                -- operand name.
                }
  deriving (Show, Generic, NFData)

instance Semigroup ISADescriptor where
  d1 <> d2 = ISADescriptor { isaInstructions = isaInstructions d1 <> isaInstructions d2
                           , isaOperands = nub (isaOperands d1 <> isaOperands d2)
                           , isaErrors = nub (isaErrors d1 <> isaErrors d2)
                           }

instance Monoid ISADescriptor where
  mempty = ISADescriptor { isaInstructions = []
                         , isaOperands = []
                         , isaErrors = []
                         }
