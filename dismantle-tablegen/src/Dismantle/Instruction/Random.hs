{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeOperators #-}
module Dismantle.Instruction.Random (
  RandomizableOpcode,
  randomInstruction,
  randomizeOperand,
  randomizeOpcode,
  ArbitraryOperands(..),
  ArbitraryOperandList(..)
  ) where

import qualified Data.Foldable as F
import qualified Data.Sequence as Seq
import qualified Data.Set as S
import qualified Data.Type.Equality as E

import Data.Parameterized.Classes ( OrdF(..) )
import Data.Parameterized.Some ( Some(..) )

import Data.EnumF ( EnumF(..) )
import qualified Data.Set.NonEmpty as NES
import qualified Dismantle.Arbitrary as A
import qualified Dismantle.Instruction as I

class ArbitraryOperandList f (tps :: [k]) where
  arbitraryOperandList :: A.Gen -> IO (I.OperandList f tps)

instance ArbitraryOperandList f '[] where
  arbitraryOperandList _gen = return I.Nil

instance (A.Arbitrary (f tp), ArbitraryOperandList f tps) => ArbitraryOperandList f (tp ': tps) where
  arbitraryOperandList gen = (I.:>) <$> A.arbitrary gen <*> arbitraryOperandList gen

-- | The @c@ here is the opcode type or tag type, called @t@ in
-- 'Dismantle.Instruction'.
type RandomizableOpcode c o = (E.TestEquality (c o), EnumF (c o), OrdF (c o))

class ArbitraryOperands c o where
  arbitraryOperands :: A.Gen -> c o sh -> IO (I.OperandList o sh)

-- | Generate a random instruction
randomInstruction :: (ArbitraryOperands c o, OrdF (c o))
                  => A.Gen
                  -> NES.Set (Some (c o))
                  -> IO (I.GenericInstruction c o)
randomInstruction gen pool = do
  sop <- A.choose pool gen
  case sop of
    Some opcode -> I.Instruction opcode <$> arbitraryOperands gen opcode

-- | Given a base set of allowed opcodes, select a random one that
-- matches the shape of the input opcode and return it.
replaceOpcode :: (RandomizableOpcode c o) => A.Gen -> S.Set (Some (c o)) -> c o sh -> IO (c o sh)
replaceOpcode g baseSet o = do
  case Seq.length available of
    -- The opcode being replaced will always be in the base set, so
    -- the 'available' set can't be empty. It would be easy to simply
    -- form a 'NES.Set' from the the given opcode and the available
    -- set, and then just use 'A.choose', but then we'd need 'Ord'
    -- constraints on the opcodes, which are not forth coming given
    -- the existential quantification in 'I.GenericInstruction' in
    -- 'randomizeOpcode' ...
    0 -> error "replaceOpcode: bug! The opcode being replaced should always be in the base set!"
    len -> do
      ix <- A.uniformR (0, len - 1) g
      return $! available `Seq.index` ix
  where
    eligible = congruentF o
    available = F.foldl' (addOpcodeIfCompatible baseSet) Seq.empty eligible

-- | Add given opcode to accumulator if it appears in the base set.
addOpcodeIfCompatible :: (RandomizableOpcode c o) => S.Set (Some (c o)) -> Seq.Seq (c o sh) -> c o sh -> Seq.Seq (c o sh)
addOpcodeIfCompatible s acc o =
  case S.member (Some o) s of
    True -> acc Seq.|> o
    False -> acc

-- | Randomly replace the opcode of an instruction with another opcode
-- chosen uniformly from the set of all compatible opcodes in the base
-- set.
--
-- The operands are preserved.
randomizeOpcode :: (RandomizableOpcode c o)
                => A.Gen
                -> S.Set (Some (c o))
                -> I.GenericInstruction c o
                -> IO (I.GenericInstruction c o)
randomizeOpcode gen baseSet = I.traverseOpcode (replaceOpcode gen baseSet)

-- | Randomly replace one operand of an instruction.
--
-- In the STOKE paper Section 4.3, the arbitrary operand is taken from
-- a restricted set in the case of immediates, namely
--
-- > [-16,..,16] \union [ 2**k | k <- [5,..] ]
--
-- Presumably we'd just like to use an appropriate 'Arbitrary'
-- instance for operands here, but
--
-- > forall sh. Arbitrary (o sh)
--
-- is problematic ...
randomizeOperand :: A.Gen
                 -> (forall sh . A.Gen -> o sh -> IO (o sh))
                 -> I.GenericInstruction c o
                 -> IO (I.GenericInstruction c o)
randomizeOperand gen arbitraryOperand (I.Instruction op os) = do
  updateAt <- A.uniformR (0, (I.operandListLength os - 1)) gen
  os' <- I.traverseOperandListIndexed (f' updateAt gen) os
  return (I.Instruction op os')
  where
    f' target g ix o
      | ix == target = arbitraryOperand g o
      | otherwise = return o
