{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE PolyKinds #-}
module Dismantle.Tablegen.TH.CaptureInfo (
  CaptureInfo(..)
  ) where

import           Data.Functor.Const ( Const )
import           Language.Haskell.TH
import qualified Data.Parameterized.List as SL

data CaptureInfo o sh =
  CaptureInfo { capturedOpcode :: o sh
              -- ^ The opcode being captured, along with a witnessed constraint
              , capturedOpcodeName :: Name
              -- ^ The TH name of the opcode
              , capturedOperandNames :: SL.List (Const Name) sh
              -- ^ A list of names in the same shape as the opcode; each name is
              -- referenced by 'genCase' to generate a match expression for the
              -- operand list for the opcode
              , genCase :: Name -> Exp -> Q Exp
              -- ^ A generator for a case expression to match the operands of
              -- the opcode.  It uses the names in 'capturedOperandNames'.
              }
