{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE TypeOperators #-}
module Dismantle.Tablegen.Shape (
  type Shape(..),
  EmptyShape,
  SingleShape,
  OperandList(..)
  ) where

type EmptyShape = 'EmptyShape
type (c ::Shape k) ::> (a::k) = c '::> a
type SingleShape x = EmptyShape ::> x

data Shape a = EmptyShape
             | Shape a ::> a

data OperandList f ctx where
  EmptyList :: OperandList f EmptyShape
  (:>) :: OperandList f ctx -> f tp -> OperandList f (ctx ::> tp)
