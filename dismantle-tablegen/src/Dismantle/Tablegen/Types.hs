{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
module Dismantle.Tablegen.Types (
  InstructionDescriptor(..),
  FieldDescriptor(..),
  FieldType(..),
  RegisterDirection(..)
  ) where

import GHC.Generics ( Generic )
import Control.DeepSeq
import qualified Data.Array.Unboxed as UA
import Data.Word ( Word8 )

import qualified Dismantle.Tablegen.ByteTrie as BT

-- | The direction of a register field (input, output, or both)
data RegisterDirection = In | Out | Both
  deriving (Show, Generic, NFData)

-- | The type of data contained in a field operand.
--
-- This is designed to distinguish between register references,
-- immediates, and other types of values.
data FieldType = Immediate
               | Register
               | Offset
               | Predication
               | Memory
               | Address
               deriving (Show, Generic, NFData)

-- | Description of an operand field in an instruction (could be a
-- register reference or an immediate)
data FieldDescriptor =
  FieldDescriptor { fieldName :: String
                  , fieldBits :: !(UA.UArray Int Word8)
                  , fieldDirection :: !RegisterDirection
                  , fieldType :: !FieldType
                  }
  deriving (Show)

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
  deriving (Show, Generic, NFData)
