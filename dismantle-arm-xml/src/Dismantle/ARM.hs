module Dismantle.ARM
  ( findInstruction
  ) where

import           Data.Parameterized (showF)
import qualified Dismantle.XML.AArch32 as D
import qualified Language.ASL.Syntax as AS

data ASLException = OpcodeNotFound String
                  | AmbiguousOpcode String [AS.Instruction]

findInstruction :: D.Opcode o sh -> [AS.Instruction] -> Either ASLException AS.Instruction
findInstruction oc insts =
  let namePrefix = "aarch32_" ++ showF oc
      instHasPrefix = undefined
  in case filter instHasPrefix insts of
    [i] -> Right i
    [] -> Left $ OpcodeNotFound namePrefix
    is -> Left $ AmbiguousOpcode namePrefix is
