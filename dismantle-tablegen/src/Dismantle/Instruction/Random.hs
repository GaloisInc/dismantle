{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE PolyKinds #-}
module Dismantle.Instruction.Random (
  randomInstruction,
  replaceOpcode
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
      let Just o' = Seq.lookup ix available
      return o'
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
