{-# OPTIONS_HADDOCK not-home #-}
module Dismantle.PPC.Operands (
  GPR(..),
  CR(..),
  FR(..),
  VR(..)
  ) where

import Data.Word ( Word8 )

newtype CR = CR Word8
  deriving (Eq, Ord, Show)

newtype FR = FR Word8
  deriving (Eq, Ord, Show)

newtype GPR = GPR Word8
  deriving (Eq, Ord, Show)

newtype VR = VR Word8
  deriving (Eq, Ord, Show)

