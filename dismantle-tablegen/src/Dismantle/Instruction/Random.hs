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
  ArbitraryOperand(..),
  ArbitraryOperands(..),
  ArbitraryShapedList(..)
  ) where

import qualified Data.Foldable as F
import qualified Data.Sequence as Seq
import qualified Data.Type.Equality as E

import Data.Parameterized.Classes ( OrdF(..) )
import Data.Parameterized.Some ( Some(..) )
import Data.Parameterized.ShapedList ( lengthFC, ShapedList(..), traverseFCIndexed, indexAsInt )

import Data.EnumF ( EnumF(..) )
import qualified Data.Set.NonEmpty as NES
import qualified Dismantle.Arbitrary as A
import qualified Dismantle.Instruction as I

class ArbitraryShapedList f (tps :: [k]) where
  arbitraryShapedList :: A.Gen -> IO (ShapedList f tps)

instance ArbitraryShapedList f '[] where
  arbitraryShapedList _gen = return Nil

instance (A.Arbitrary (f tp), ArbitraryShapedList f tps) => ArbitraryShapedList f (tp ': tps) where
  arbitraryShapedList gen = (:>) <$> A.arbitrary gen <*> arbitraryShapedList gen

-- | The @c@ here is the opcode type or tag type, called @t@ in
-- 'Dismantle.Instruction'.
type RandomizableOpcode c o = (E.TestEquality (c o), EnumF (c o), OrdF (c o))

-- | Used to perturb existing operands in stratified synthesis.
--
-- In the STOKE paper Section 4.3, the arbitrary operand is taken from
-- a restricted set in the case of immediates, namely
--
-- > [-16..16] \union [ +/- 2^k | k <- [5..] ]
class ArbitraryOperand o where
  arbitraryOperand :: A.Gen -> o sh -> IO (o sh)

class ArbitraryOperands c o where
  arbitraryOperands :: A.Gen -> c o sh -> IO (ShapedList o sh)

-- | Generate a random instruction
randomInstruction :: (ArbitraryOperands c o, OrdF (c o))
                  => A.Gen
                  -> NES.Set (Some (c o))
                  -> IO (I.GenericInstruction c o)
randomInstruction gen baseSet = do
  sop <- A.choose baseSet gen
  case sop of
    Some opcode -> I.Instruction opcode <$> arbitraryOperands gen opcode

-- | Given a base set of allowed opcodes, select a random one that
-- matches the shape of the input opcode and return it.
replaceOpcode :: (RandomizableOpcode c o) => A.Gen -> NES.Set (Some (c o)) -> c o sh -> IO (c o sh)
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
addOpcodeIfCompatible :: (RandomizableOpcode c o) => NES.Set (Some (c o)) -> Seq.Seq (c o sh) -> c o sh -> Seq.Seq (c o sh)
addOpcodeIfCompatible s acc o =
  case NES.member (Some o) s of
    True -> acc Seq.|> o
    False -> acc

-- | Randomly replace the opcode of an instruction with another opcode
-- chosen uniformly from the set of all compatible opcodes in the base
-- set.
--
-- The operands are preserved.
randomizeOpcode :: (RandomizableOpcode c o)
                => A.Gen
                -> NES.Set (Some (c o))
                -> I.GenericInstruction c o
                -> IO (I.GenericInstruction c o)
randomizeOpcode gen baseSet = I.traverseOpcode (replaceOpcode gen baseSet)

-- | Randomly replace one operand of an instruction.
randomizeOperand :: (ArbitraryOperand o)
                 => A.Gen
                 -> I.GenericInstruction c o
                 -> IO (I.GenericInstruction c o)
randomizeOperand gen (I.Instruction op os) = do
  updateAt <- A.uniformR (0, (lengthFC os - 1)) gen
  os' <- traverseFCIndexed (f' updateAt gen) os
  return (I.Instruction op os')
  where
    f' target g ix o
      | indexAsInt ix == target = arbitraryOperand g o
      | otherwise = return o
