{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE ViewPatterns #-}
-- | Helpers for the pretty printer generated with TH
module Dismantle.Tablegen.TH.Pretty (
  PrettyOperand(..),
  prettyInstruction
  ) where

import Data.Char ( isAlpha )
import qualified Data.Map as M
import Data.Monoid
import qualified Text.PrettyPrint as PP

-- | Wrap 1) the name of an operand, 2) the operand itself, and 3) the
-- formatter for the operand.
data PrettyOperand = forall t . PrettyOperand String t (t -> String)

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

toOpMap :: [PrettyOperand] -> M.Map String String
toOpMap = foldr formatOperand M.empty


formatOperand :: PrettyOperand -> M.Map String String -> M.Map String String
formatOperand (PrettyOperand name val fmt) = M.insert name (fmt val)

format :: String -> M.Map String String -> PP.Doc
format fmt operands =
  case fmt of
    [] -> mempty
    '$' : '{' : rest -> formatBracketedVar rest operands ""
    '$' : rest -> formatUnbracketedVar rest operands ""
    c : rest -> PP.char c <> format rest operands

formatBracketedVar :: String -> M.Map String String -> String -> PP.Doc
formatBracketedVar fmt operands varName =
  case fmt of
    '}' : rest ->
      case M.lookup (reverse varName) operands of
        Nothing -> PP.text ("[UndefinedVar: " ++ reverse varName ++ "]")
        Just s -> PP.text s <> format rest operands
    c : rest -> formatBracketedVar rest operands (c : varName)
    [] -> PP.text ("[UnterminatedVar: " ++ reverse varName ++ "]")

formatUnbracketedVar :: String -> M.Map String String -> String -> PP.Doc
formatUnbracketedVar fmt operands varName =
  case fmt of
    [] ->
      case M.lookup (reverse varName) operands of
        Nothing -> PP.text ("[UndefinedVar: " ++ reverse varName ++ "]")
        Just s -> PP.text s
    c : rest
      | isAlpha c -> formatUnbracketedVar rest operands (c : varName)
      | otherwise ->
        case M.lookup (reverse varName) operands of
          Nothing -> PP.text ("[UndefinedVar: " ++ reverse varName ++ "]") <> PP.char c <> format rest operands
          Just s -> PP.text s <> PP.char c <> format rest operands


