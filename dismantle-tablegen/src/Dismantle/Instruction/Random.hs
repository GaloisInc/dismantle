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
randomInstruction :: (ArbitraryOperands c o)
                  => A.Gen
                  -> S.Set (Some (c o))
                  -> IO (Maybe (I.GenericInstruction c o))
randomInstruction gen pool
  | S.null pool = return Nothing
  | otherwise = do
     sop <- A.choose gen pool
     case sop of
       Some opcode -> Just <$> I.Instruction opcode <$> arbitraryOperands gen opcode

-- | Given a set of allowed opcodes, select a random one that matches the shape
-- of the input opcode and return it.
--
-- If there are no opcodes in the set with a matching shape, the opcode itself is returned.
--
-- FIXME: Switch to return Nothing if there are no alternate opcodes available?
-- Using `MaybeT IO` would work with `traverseOpcode`
replaceOpcode :: (RandomizableOpcode c o) => A.Gen -> S.Set (Some (c o)) -> c o sh -> IO (c o sh)
replaceOpcode g os o = do
  case Seq.length available of
    0 -> return o
    len -> do
      ix <- A.uniformR (0, len - 1) g
      return $! available `Seq.index` ix
  where
    eligible = congruentF o
    available = F.foldl' (checkCompatibleOpcode os) Seq.empty eligible

-- | Collect compatible opcodes that appear in the given set into a list with
-- the shape recovered
checkCompatibleOpcode :: (RandomizableOpcode c o) => S.Set (Some (c o)) -> Seq.Seq (c o sh) -> c o sh -> Seq.Seq (c o sh)
checkCompatibleOpcode s acc o =
  case S.member (Some o) s of
    True -> acc Seq.|> o
    False -> acc

randomizeOpcode :: (RandomizableOpcode c o)
                => A.Gen
                -> S.Set (Some (c o))
                -> I.GenericInstruction c o
                -> IO (I.GenericInstruction c o)
randomizeOpcode gen os = I.traverseOpcode (replaceOpcode gen os)

randomizeOperand :: A.Gen
                 -> (forall sh . A.Gen -> o sh -> IO (o sh))
                 -> I.GenericInstruction c o
                 -> IO (I.GenericInstruction c o)
randomizeOperand gen f (I.Instruction op os) = do
  updateAt <- A.uniformR (0, (I.operandListLength os - 1)) gen
  os' <- I.traverseOperandListIndexed (f' updateAt gen) os
  return (I.Instruction op os')
  where
    f' target g ix o
      | ix == target = f g o
      | otherwise = return o
