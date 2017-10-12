{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE PolyKinds #-}
module Dismantle.Tablegen.TH.CaptureInfo (
  CaptureInfo(..)
  ) where

import           Language.Haskell.TH
import           Data.Parameterized.FreeParamF ( FreeParamF(..) )
import qualified Data.Parameterized.ShapedList as SL
import           Data.Parameterized.Witness ( Witness(..) )

data CaptureInfo c o sh =
  CaptureInfo { capturedOpcode :: Witness c o sh
              -- ^ The opcode being captured, along with a witnessed constraint
              , capturedOpcodeName :: Name
              -- ^ The TH name of the opcode
              , capturedOperandNames :: SL.ShapedList (FreeParamF Name) sh
              -- ^ A list of names in the same shape as the opcode; each name is
              -- referenced by 'genCase' to generate a match expression for the
              -- operand list for the opcode
              , genCase :: Name -> Exp -> Q Exp
              -- ^ A generator for a case expression to match the operands of
              -- the opcode.  It uses the names in 'capturedOperandNames'.
              }
