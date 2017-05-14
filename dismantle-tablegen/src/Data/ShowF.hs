{-# LANGUAGE PolyKinds #-}
module Data.ShowF ( ShowF(..) ) where

class ShowF f where
  {-# MINIMAL showF | showsF #-}

  showF :: f tp -> String
  showF f = showsF f ""

  showsF :: f tp -> String -> String
  showsF f s = showF f ++ s

