{-# LANGUAGE DataKinds #-}
{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE ViewPatterns #-}
module Dismantle.Tablegen.TH (
  genISA
  ) where

import GHC.TypeLits ( Symbol )

import Data.Char ( toUpper )
import qualified Data.Text.Lazy.IO as TL
import Language.Haskell.TH

import Dismantle.Tablegen
import Dismantle.Tablegen.Instruction
import Dismantle.Tablegen.Types

genISA :: ISA -> FilePath -> DecsQ
genISA isa path = do
  desc <- runIO $ loadISA isa path
  opType <- mkOperandType desc
  return $ concat [ opType
                  ]

loadISA :: ISA -> FilePath -> IO ISADescriptor
loadISA isa path = do
  txt <- TL.readFile path
  case parseTablegen path txt of
    Left err -> fail (show err)
    Right defs -> return $ filterISA isa defs

-- | Generate a type to represent operands for this ISA
--
-- The type is always named @Operand@ and has a single type parameter
-- of kind 'Symbol'.
--
-- FIXME: We'll definitely need a mapping from string names to
-- suitable constructor names, as well as a description of the type
-- structure.
--
-- String -> (String, Q Type)
mkOperandType :: ISADescriptor -> Q [Dec]
mkOperandType isa = return [ DataD [] (mkName "Operand") [] (Just ksig) cons []
                           , StandaloneDerivD [] (ConT ''Show `AppT` (ConT (mkName "Operand") `AppT` VarT (mkName "tp")))
                           ]
  where
    ksig = ArrowT `AppT` ConT ''Symbol `AppT` StarT
    tyvars = [ KindedTV (mkName "tp") (ConT ''Symbol)
             ]
    cons = map mkOperandCon (isaOperands isa)

mkOperandCon :: FieldType -> Con
mkOperandCon (FieldType (toTypeName -> name)) = GadtC [n] [] ty
  where
    n = mkName name
    ty = ConT (mkName "Operand") `AppT` LitT (StrTyLit name)

toTypeName :: String -> String
toTypeName s =
  case s of
    [] -> error "Empty names are not allowed"
    c:rest -> toUpper c : rest

{-

For each ISA, we have to generate:

1) A datatype representing all possible operands (along with an
associated tag type, one tag for each operand type).  There may be
some sub-types (e.g., a separate Register type to be a parameter to a
Reg32 operand).

2) An ADT representing all possible instructions - this is a simple
GADT with no parameters, but the return types are lists of the types
of the operands for the instruction represented by the tag.

3) A type alias instantiating the underlying Instruction type with the
Tag and Operand types.

4) A pretty printer

5) A parser

6) An unparser

-}

-- data Operand (tp :: Symbol) where
data Operand :: Symbol -> * where
  OImm32 :: Int -> Operand "Imm32"
  OReg32 :: Int -> Operand "Reg32"

deriving instance Show (Operand tp)

s2 :: OperandList Operand '["Imm32", "Reg32"]
s2 = OImm32 5 :> OReg32 0 :> Nil

s3 = case s2 of
  OImm32 _ :> l -> l

insn = Instruction Add s2

data ISATag o sh where
  Add :: ISATag Operand '["Imm32", "Reg32"]
  Sub :: ISATag Operand '["Imm32", "Reg32"]

type I = Instruction ISATag Operand
type AnnotatedI = Instruction ISATag (Annotated () Operand)

foo :: I -> Int
foo i =
  case i of
    Instruction Add (OImm32 imm :> OReg32 regNo :> Nil) -> imm + regNo
