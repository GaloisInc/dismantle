{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TemplateHaskell #-}
module Dismantle.Tablegen.Types (
  InstructionDescriptor(..),
  OperandDescriptor(..),
  OperandType(..),
  RegisterClass(..),
  ISADescriptor(..)
  ) where

import GHC.Generics ( Generic )
import Control.DeepSeq
import Data.Word ( Word8 )
import Language.Haskell.TH
import Language.Haskell.TH.Syntax ( Lift(..) )

import qualified Dismantle.Tablegen.ByteTrie as BT

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

instance Lift OperandType where
  lift (OperandType s) = conE 'OperandType `appE` lift s

-- | Description of an operand field in an instruction (could be a
-- register reference or an immediate)
data OperandDescriptor =
  OperandDescriptor { opName :: String
                    , opBits :: [(Int, Word8)]
                    , opType :: !OperandType
                    }
  deriving (Eq, Ord, Show)

instance Lift OperandDescriptor where
  lift od = conE 'OperandDescriptor `appE`
                 lift (opName od) `appE`
                 lift (opBits od) `appE`
                 lift (opType od)

instance NFData OperandDescriptor where
  rnf od = opName od `deepseq` od `seq` ()

-- | Description of an instruction, abstracted from the tablegen
-- definition
data InstructionDescriptor =
  InstructionDescriptor { idMask :: [BT.Bit]
                        , idMnemonic :: String
                        , idInputOperands :: [OperandDescriptor]
                        , idOutputOperands :: [OperandDescriptor]
                        , idNamespace :: String
                        , idDecoder :: String
                        , idAsmString :: String
                        , idPseudo :: Bool
                        }
  deriving (Eq, Ord, Show, Generic, NFData)

instance Lift InstructionDescriptor where
  lift i = conE 'InstructionDescriptor `appE`
                lift (idMask i) `appE`
                lift (idMnemonic i) `appE`
                lift (idInputOperands i) `appE`
                lift (idOutputOperands i) `appE`
                lift (idNamespace i) `appE`
                lift (idDecoder i) `appE`
                lift (idAsmString i) `appE`
                lift (idPseudo i)

data RegisterClass = RegisterClass String
  deriving (Show, Generic, NFData)

instance Lift RegisterClass where
  lift (RegisterClass rc) = conE 'RegisterClass `appE` lift rc

data ISADescriptor =
  ISADescriptor { isaInstructions :: [InstructionDescriptor]
                , isaRegisterClasses :: [RegisterClass]
                , isaRegisters :: [(String, RegisterClass)]
                , isaOperands :: [OperandType]
                -- ^ All of the operand types used in an ISA.
                , isaErrors :: [(String, String)]
                -- ^ Errors while mapping operand classes to bit
                -- fields in the instruction encoding; the first
                -- String is the mnemonic, while the second is the
                -- operand name.
                }
  deriving (Show, Generic, NFData)

instance Lift ISADescriptor where
  lift isad = conE 'ISADescriptor `appE`
                   lift (isaInstructions isad) `appE`
                   lift (isaRegisterClasses isad) `appE`
                   lift (isaRegisters isad) `appE`
                   lift (isaOperands isad) `appE`
                   lift (isaErrors isad)
