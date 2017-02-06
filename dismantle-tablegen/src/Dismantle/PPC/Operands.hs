{-# OPTIONS_HADDOCK not-home #-}
module Dismantle.PPC.Operands (
  GPR(..),
  CR(..),
  FR(..),
  VR(..)
  ) where

import Data.Monoid
import Data.Word ( Word8 )

import qualified Text.PrettyPrint.HughesPJClass as PP

newtype CR = CR Word8
  deriving (Eq, Ord, Show)

newtype FR = FR Word8
  deriving (Eq, Ord, Show)

newtype GPR = GPR Word8
  deriving (Eq, Ord, Show)

newtype VR = VR Word8
  deriving (Eq, Ord, Show)

instance PP.Pretty GPR where
  pPrint (GPR rno) = PP.char 'r' <> PP.int (fromIntegral rno)

instance PP.Pretty CR where
  pPrint (CR rno) = PP.char 'c' <> PP.int (fromIntegral rno)

instance PP.Pretty FR where
  pPrint (FR rno) = PP.char 'f' <> PP.int (fromIntegral rno)

instance PP.Pretty VR where
  pPrint (VR rno) = PP.char 'v' <> PP.int (fromIntegral rno)
