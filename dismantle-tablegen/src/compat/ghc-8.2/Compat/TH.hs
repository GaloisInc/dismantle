module Compat.TH (
  mkStandaloneDerivD
  ) where

import Language.Haskell.TH

mkStandaloneDerivD :: Cxt -> Type -> Dec
mkStandaloneDerivD = StandaloneDerivD Nothing
