{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
module Dismantle.Tablegen.Types (
  InstructionDescriptor(..),
  FieldDescriptor(..),
  FieldType(..),
  RegisterDirection(..),
  RegisterClass(..),
  ISADescriptor(..)
  ) where

import GHC.Generics ( Generic )
import Control.DeepSeq
import qualified Data.Array.Unboxed as UA
import qualified Data.Set as S
import Data.Word ( Word8 )

import qualified Dismantle.Tablegen.ByteTrie as BT

-- | The direction of a register field (input, output, or both)
data RegisterDirection = In | Out | Both
  deriving (Eq, Ord, Show, Generic, NFData)

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
data FieldType = FieldType String
               deriving (Eq, Ord, Show, Generic, NFData)

-- | Description of an operand field in an instruction (could be a
-- register reference or an immediate)
data FieldDescriptor =
  FieldDescriptor { fieldName :: String
                  , fieldBits :: !(UA.UArray Int Word8)
                  , fieldDirection :: !RegisterDirection
                  , fieldType :: !FieldType
                  }
  deriving (Eq, Ord, Show)

instance NFData FieldDescriptor where
  rnf fd = fieldName fd `deepseq` fd `seq` ()

-- | Description of an instruction, abstracted from the tablegen
-- definition
data InstructionDescriptor =
  InstructionDescriptor { idMask :: [BT.Bit]
                        , idMnemonic :: String
                        , idFields :: [FieldDescriptor]
                        , idNamespace :: String
                        , idDecoder :: String
                        , idAsmString :: String
                        , idPseudo :: Bool
                        }
  deriving (Eq, Ord, Show, Generic, NFData)

data RegisterClass = RegisterClass String
  deriving (Show, Generic, NFData)

data ISADescriptor =
  ISADescriptor { isaInstructions :: [InstructionDescriptor]
                , isaRegisterClasses :: [RegisterClass]
                , isaRegisters :: [(String, RegisterClass)]
                , isaInstructionClasses :: [(S.Set (RegisterDirection, FieldType), [InstructionDescriptor])]
                -- ^ Instructions grouped by shape w.r.t. operands
                }
  deriving (Show, Generic, NFData)
