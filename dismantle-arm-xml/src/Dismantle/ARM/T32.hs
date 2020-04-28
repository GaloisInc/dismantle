{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveLift #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE CPP #-}

-- Dump TH splices to two files on disk. The generated file
-- Dismantle/PPC.dump-splices will contain all splices, and not be
-- valid Haskell, while the generated file Dismantle/PPC.th.hs will
-- have only the top-level splices, and will be valid Haskell. The
-- second file can be used when generating TAGS.
{-# OPTIONS_GHC -ddump-splices -ddump-to-file -dth-dec-file -Wno-missing-signatures -O0 #-}
module Dismantle.ARM.T32 (
  Instruction,
  AnnotatedInstruction,
  GenericInstruction(..),
  List(..),
  Annotated(..),
  Operand(..),
  OperandRepr(..),
  operandReprString,
  Opcode(..),
  disassembleInstruction,
  assembleInstruction,
  aslEncodingMap,
  ppInstruction
  ) where

import Data.Parameterized.List ( List(..) )

import Dismantle.Instruction
import Dismantle.ARM.TH ( genISA )
import Dismantle.ARM.ISA ( isa )
import Dismantle.Tablegen.TH ( genInstances )

#ifdef ASL_LITE
$(genISA (isa "T32") "data/ISA_uboot_req" "t32_encindex.xml" "data/Parsed/arm_instrs.sexpr" "T32.log")
#else
$(genISA (isa "T32") "data/ISA_v85A_AArch32_xml_00bet9" "t32_encindex.xml" "data/Parsed/arm_instrs.sexpr" "T32.log")
#endif
$(return [])

-- We need a separate call to generate some instances, since the helper(s) that
-- create these instances use reify, which we can't call until we flush the TH
-- generation using the @$(return [])@ trick.
$(genInstances)
