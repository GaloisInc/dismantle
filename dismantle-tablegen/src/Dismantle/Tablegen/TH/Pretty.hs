{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE ViewPatterns #-}
{-# OPTIONS_GHC -Wno-orphans #-}
-- | Helpers for the pretty printer generated with TH
module Dismantle.Tablegen.TH.Pretty (
  PrettyOperand(..),
  prettyInstruction
  ) where

import Control.Applicative ((<|>))
import Data.Char ( isAlphaNum )
import Data.Int ( Int16, Int32, Int64 )
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
prettyInstruction :: [(String, String)] -> String -> [PrettyOperand] -> PP.Doc
prettyInstruction defaults fmt (toOpMap -> operands) = format defaults fmt operands

toOpMap :: [PrettyOperand] -> M.Map String PP.Doc
toOpMap = foldr formatOperand M.empty


formatOperand :: PrettyOperand -> M.Map String PP.Doc -> M.Map String PP.Doc
formatOperand (PrettyOperand name val fmt) = M.insert name (fmt val)

format :: [(String, String)] -> String -> M.Map String PP.Doc -> PP.Doc
format defaults fmt operands =
  case fmt of
    [] -> mempty
    '$' : '{' : rest -> formatBracketedVar defaults rest operands ""
    '$' : rest -> formatUnbracketedVar defaults rest operands ""
    c : rest -> PP.char c <> format defaults rest operands

formatBracketedVar :: [(String, String)] -> String -> M.Map String PP.Doc -> String -> PP.Doc
formatBracketedVar defaults fmt operands varName =
  let name = reverse varName
  in case fmt of
    '}' : rest ->
      let ppOperand = M.lookup name operands
          ppDefault = PP.text <$> lookup name defaults
          result = ppOperand <|> ppDefault
      in case result of
        Nothing -> PP.text ("[UndefinedVar: " ++ name ++ "]")
        Just s -> s <> format defaults rest operands
    c : rest -> formatBracketedVar defaults rest operands (c : varName)
    [] -> PP.text ("[UnterminatedVar: " ++ name ++ "]")

formatUnbracketedVar :: [(String, String)] -> String -> M.Map String PP.Doc -> String -> PP.Doc
formatUnbracketedVar defaults fmt operands varName =
  let name = reverse varName
  in case fmt of
    [] ->
      let ppOperand = M.lookup name operands
          ppDefault = PP.text <$> lookup name defaults
          result = ppOperand <|> ppDefault
      in case result of
        Nothing -> PP.text ("[UndefinedVar: " ++ name ++ "]")
        Just s -> s
    c : rest
      | isAlphaNum c -> formatUnbracketedVar defaults rest operands (c : varName)
      | otherwise ->
        case M.lookup name operands of
          Nothing -> PP.text ("[UndefinedVar: " ++ name ++ "]") <> PP.char c <> format defaults rest operands
          Just s -> s <> PP.char c <> format defaults rest operands

instance PP.Pretty Int16 where
  pPrint = PP.integer . fromIntegral

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
