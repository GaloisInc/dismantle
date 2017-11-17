module Main where

import Data.List (intercalate, groupBy)
import Data.Monoid ((<>))
import Data.Char (toUpper, toLower)
import System.Environment (getArgs)

capitalize :: String -> String
capitalize [] = []
capitalize (c:cs) = toUpper c : cs

unCapitalize :: String -> String
unCapitalize [] = []
unCapitalize (c:cs) = toLower c : cs

snakeToCamel :: String -> String
snakeToCamel [] = []
snakeToCamel s =
    let parts = filter (/= "_") $ groupBy (\a b -> a /= '_' && b /= '_') s
    in concat $ head parts : (capitalize <$> tail parts)

toTypeName :: String -> String
toTypeName = capitalize . snakeToCamel

boilerplate :: String -> String
boilerplate op =
    let ty = toTypeName op
        lowerTy = unCapitalize $ snakeToCamel op
        derived = ["Eq", "Ord", "Show"]
        mkFunc = "mk" <> ty
        toBitsFunc = lowerTy <> "ToBits"
        operandFunc = lowerTy <> "Operand"
    in unlines [ "  , " <> ty
               , "  , " <> mkFunc
               , "  , " <> toBitsFunc
               , "  , " <> operandFunc
               , ""
               , "data " <> ty <> " = " <> ty <> " {"
               , "         " <> replicate (length ty * 2) ' ' <> "}"
               , "        " <> replicate (length ty) ' ' <> "deriving (" <> (intercalate ", " derived) <> ")"
               , ""
               , "instance PP.Pretty " <> ty <> " where"
               , "  pPrint _ = PP.text \"" <> ty <> ": not implemented\""
               , ""
               , mkFunc <> " :: Word32 -> " <> ty
               , mkFunc <> " w = " <> ty
               , ""
               , toBitsFunc <> " :: " <> ty <> " -> Word32"
               , toBitsFunc <> " " <> ty <> " = 0"
               , ""
               , operandFunc <> " :: OperandPayload"
               , operandFunc <> " ="
               , "  OperandPayload { opTypeT = [t| " <> ty <> " |]"
               , "                 , opConE  = Just (varE '" <> mkFunc <> ")"
               , "                 , opWordE = Just (varE '" <> toBitsFunc <> ")"
               , "                 }"
               , ""
               , "instance A.Arbitrary " <> ty <> " where"
               , "  arbitrary g = pure " <> ty
               ]

main :: IO ()
main = do
    args <- getArgs
    let [opName] = args
    putStr $ boilerplate opName
