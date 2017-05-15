{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RankNTypes #-}
module Dismantle.Instruction.Random (
  RandomizableOpcode,
  randomInstruction,
  randomizeOperand,
  randomizeOpcode
  ) where

import qualified Data.Foldable as F
import qualified Data.Sequence as Seq
import qualified Data.Set as S
import qualified Data.Type.Equality as E
import qualified System.Random.MWC as R

import Data.EnumF ( EnumF(..) )
import qualified Dismantle.Instruction as I

type RandomizableOpcode c o = (E.TestEquality (c o), EnumF (c o))

-- | Generate a random instruction
randomInstruction :: IO (I.GenericInstruction c o)
randomInstruction = undefined

-- | Given a set of allowed opcodes, select a random one that matches the shape
-- of the input opcode and return it.
--
-- If there are no opcodes in the set with a matching shape, the opcode itself is returned.
replaceOpcode :: (RandomizableOpcode c o) => R.GenIO -> S.Set (I.SomeOpcode c o) -> c o sh -> IO (c o sh)
replaceOpcode g os o = do
  case Seq.length available of
    0 -> return o
    len -> do
      ix <- R.uniformR (0, len - 1) g
      return $! available `Seq.index` ix
  where
    eligible = congruentF o
    available = F.foldl' (checkCompatibleOpcode os) Seq.empty eligible

-- | Collect compatible opcodes that appear in the given set into a list with
-- the shape recovered
checkCompatibleOpcode :: (RandomizableOpcode c o) => S.Set (I.SomeOpcode c o) -> Seq.Seq (c o sh) -> c o sh -> Seq.Seq (c o sh)
checkCompatibleOpcode s acc o =
  case S.member (I.SomeOpcode o) s of
    True -> acc Seq.|> o
    False -> acc

randomizeOpcode :: (RandomizableOpcode c o)
                => R.GenIO
                -> S.Set (I.SomeOpcode c o)
                -> I.GenericInstruction c o
                -> IO (I.GenericInstruction c o)
randomizeOpcode gen os = I.traverseOpcode (replaceOpcode gen os)

randomizeOperand :: R.GenIO
                 -> (forall sh . R.GenIO -> o sh -> IO (o sh))
                 -> I.GenericInstruction c o
                 -> IO (I.GenericInstruction c o)
randomizeOperand gen f (I.Instruction op os) = do
  updateAt <- R.uniformR (0, (I.operandListLength os - 1)) gen
  os' <- I.traverseOperandListIndexed (f' updateAt gen) os
  return (I.Instruction op os')
  where
    f' target g ix o
      | ix == target = f g o
      | otherwise = return o
