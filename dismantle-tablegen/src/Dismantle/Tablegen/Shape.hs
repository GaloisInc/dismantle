{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE TypeOperators #-}
module Dismantle.Tablegen.Shape (
  OperandList(..)
  ) where

data OperandList f sh where
  Nil  :: OperandList f '[]
  (:>) :: f tp -> OperandList f sh -> OperandList f (tp ': sh)

infixr 5 :>
