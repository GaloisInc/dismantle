{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveLift #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -fno-spec-constr -fno-specialise -fmax-simplifier-iterations=1 -fno-call-arity  -fbinary-blob-threshold=5000 #-}
-- Dump TH splices to two files on disk. The generated file
-- Dismantle/PPC.dump-splices will contain all splices, and not be
-- valid Haskell, while the generated file Dismantle/PPC.th.hs will
-- have only the top-level splices, and will be valid Haskell. The
-- second file can be used when generating TAGS.
{-# OPTIONS_GHC -ddump-splices -ddump-to-file -dth-dec-file #-}
-- | Description: PPC opcodes generated from LLVM .tgen file
--
-- PPC opcodes generated from LLVM .tgen file.
--
-- Note the instruction format here is much closer to binary encoding
-- than (IBM) PPC assembly. For example
--
-- * jump targets must be divided by 4, because in the binary encoding
--   the low order two bits of the offset are not included.
--
-- * operands are in binary encoding order, not assembler order, and the
--   two orders usually differ.
--
-- * opcodes here are taken from the LLVM @:\/data\/PPC.tgen@ file, and
--   these opcode names often differ from the assembler opcode names
--   (e.g. @ADD4@ for @add@, @BC@ for @bt@, @BCn@ for @bf@, @GBC@ for
--   @bc@).
--
-- In practice, an easy to way to find the relationship between the
-- instruction representation here and the more familiar assembler
-- notation:
--
-- * for going from PPC assembler to dismantle representation, get the
--     binary output of the assembler (e.g. using @objdump@) and then
--     feed it into 'disassembleinstruction'. For example:
--
--     @
--     ghci> disassembleInstruction $ B.pack [0x7c,0xc0,0x00,0x26]
--     (4,Just Instruction MFCR (Gprc (GPR {unGPR = 6}) :< Nil))
--     @
--
--     where @B@ is @Data.ByteString.Lazy@.
--
-- * for going from dismantle representation to PPC assembler, use
--     'ppInstruction'. For example:
--
--     @
--     ghci> ppInstruction  $ Instruction BCn (Condbrtarget (CBT {unCBT = -2}) :< Crbitrc (CRBitRC {unCRBitRC = 3}) :< Nil)
--     bc 4, 4*cr0+so, -8
--     @
--
--     which is more concisely represented using the @bns@ or @bf@
--     mnemonics.
--
-- It can also be useful to use the round trip
--
-- @
-- disassembleInstruction . assembleInstruction
-- @
--
-- to find which opcode dismantle prefers for a given dismantle
-- opcode. There are many mnemonic opcodes in PPC, where a more
-- specific opcode is really just a more general opcode with some
-- arguments fixed. For example, if you round trip the above @BCn@
-- example, you'll find that dismantle prefers @GBC@ (called @bc@ in
-- PPC assembler).
--
-- If an opcode you want is not defined in the @PPC.tgen@ file, then
-- you can add it with an "override" in @:\/data\/override@; see for
-- example the override there that defines @ADD4Oo@ (i.e. @addo.@),
-- which was gotten by copying and hand modifying the entry for
-- @ADD4o@ (i.e. @add.@) in the @PPC.tgen@. BUT WAIT! You should
-- probably try 'disassembleinstruction' first, as explained above,
-- because it's likely the opcode you want is defined, it just has a
-- weird LLVM name you've never heard of :P
--
--  See @SFE.Instrument.TrapArithOverflows.Arch.PPC@ in @sfe@ for
--  concrete examples of converting PPC assembler to the
--  representation here.
module Dismantle.PPC (
  Instruction,
  AnnotatedInstruction,
  GenericInstruction(..),
  List(..),
  Annotated(..),
  Operand(..),
  OperandRepr(..),
  operandReprString,
  Opcode(..),
  module Dismantle.PPC.Operands,
  disassembleInstruction,
  assembleInstruction,
  ppInstruction
  )where

import Data.Parameterized.List ( List(..) )

import Dismantle.Instruction
import Dismantle.PPC.Operands
import Dismantle.PPC.ISA ( isa )
import Dismantle.Tablegen.TH ( genISA, genInstances )

$(genISA isa "data/PPC.tgen" ["data/override"])
$(return [])

-- We need a separate call to generate some instances, since the helper(s) that
-- create these instances use reify, which we can't call until we flush the TH
-- generation using the @$(return [])@ trick.
$(genInstances)
