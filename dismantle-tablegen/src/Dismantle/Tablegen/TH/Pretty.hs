{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE ViewPatterns #-}
{-# OPTIONS_GHC -Wno-orphans #-}
-- | Helpers for the pretty printer generated with TH
module Dismantle.Tablegen.TH.Pretty (
  PrettyOperand(..),
  prettyInstruction
  ) where

import Data.Char ( isAlpha )
import Data.Int ( Int32, Int64 )
import Data.Word ( Word8, Word16, Word32, Word64 )
import qualified Data.Map as M
import Data.Monoid
import qualified Text.PrettyPrint.HughesPJClass as PP

-- | Wrap 1) the name of an operand, 2) the operand itself, and 3) the
-- formatter for the operand.
data PrettyOperand = forall t . PrettyOperand String t (t -> PP.Doc)

-- | Pretty print an instruction based on a format string and a
-- list of operands and formatters.
--
-- Variable names are prefixed with a dollar sign sigil in the format
-- string.
--
-- If a variable is unbound, the output document will contain a big
-- warning.
prettyInstruction :: String -> [PrettyOperand] -> PP.Doc
prettyInstruction fmt (toOpMap -> operands) = format fmt operands

toOpMap :: [PrettyOperand] -> M.Map String PP.Doc
toOpMap = foldr formatOperand M.empty


formatOperand :: PrettyOperand -> M.Map String PP.Doc -> M.Map String PP.Doc
formatOperand (PrettyOperand name val fmt) = M.insert name (fmt val)

format :: String -> M.Map String PP.Doc -> PP.Doc
format fmt operands =
  case fmt of
    [] -> mempty
    '$' : '{' : rest -> formatBracketedVar rest operands ""
    '$' : rest -> formatUnbracketedVar rest operands ""
    c : rest -> PP.char c <> format rest operands

formatBracketedVar :: String -> M.Map String PP.Doc -> String -> PP.Doc
formatBracketedVar fmt operands varName =
  case fmt of
    '}' : rest ->
      case M.lookup (reverse varName) operands of
        Nothing -> PP.text ("[UndefinedVar: " ++ reverse varName ++ "]")
        Just s -> s <> format rest operands
    c : rest -> formatBracketedVar rest operands (c : varName)
    [] -> PP.text ("[UnterminatedVar: " ++ reverse varName ++ "]")

formatUnbracketedVar :: String -> M.Map String PP.Doc -> String -> PP.Doc
formatUnbracketedVar fmt operands varName =
  case fmt of
    [] ->
      case M.lookup (reverse varName) operands of
        Nothing -> PP.text ("[UndefinedVar: " ++ reverse varName ++ "]")
        Just s -> s
    c : rest
      | isAlpha c -> formatUnbracketedVar rest operands (c : varName)
      | otherwise ->
        case M.lookup (reverse varName) operands of
          Nothing -> PP.text ("[UndefinedVar: " ++ reverse varName ++ "]") <> PP.char c <> format rest operands
          Just s -> s <> PP.char c <> format rest operands

instance PP.Pretty Int32 where
  pPrint = PP.integer . fromIntegral

instance PP.Pretty Int64 where
  pPrint = PP.integer . fromIntegral

instance PP.Pretty Word8 where
  pPrint = PP.integer . fromIntegral

instance PP.Pretty Word16 where
  pPrint = PP.integer . fromIntegral

instance PP.Pretty Word32 where
  pPrint = PP.integer . fromIntegral

instance PP.Pretty Word64 where
  pPrint = PP.integer . fromIntegral
