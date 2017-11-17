module Main where

import Control.Monad (forM)
import Data.List (intercalate, intersperse, groupBy)
import Data.Monoid ((<>))
import Data.Char (toUpper, toLower)
import Text.Read (readMaybe)
import System.Environment (getArgs)
import System.Exit (exitFailure)

capitalize :: String -> String
capitalize [] = []
capitalize (c:cs) = toUpper c : cs

unCapitalize :: String -> String
unCapitalize [] = []
unCapitalize (c:cs) = toLower c : cs

unwordsBy :: Char -> String -> [String]
unwordsBy c = filter (/= [c]) . groupBy (\a b -> a /= c && b /= c)

snakeToCamel :: String -> String
snakeToCamel [] = []
snakeToCamel s =
    let parts = unwordsBy '_' s
    in concat $ head parts : (capitalize <$> tail parts)

toTypeName :: String -> String
toTypeName = capitalize . snakeToCamel

data FieldInfo =
    FieldInfo { fieldName :: String
              , fieldType :: String
              , fieldSize :: Int
              , fieldOffset :: Int
              }

parseField :: String -> Maybe FieldInfo
parseField s =
    case unwordsBy ':' s of
        (name : sz : off : rest) | length rest <= 1 -> do
            size <- readMaybe sz
            offset <- readMaybe off

            let ty = case rest of
                  [t] -> t
                  [] ->
                      let bytes = if size `mod` 8 == 0
                                  then size `div` 8
                                  else (size `div` 8) + 1
                          bits = bytes * 8
                      in "Word" <> show bits

            return $ FieldInfo name ty size offset
        _ -> Nothing

boilerplate :: String -> [FieldInfo] -> String
boilerplate op fields =
    let ty = toTypeName op
        lowerTy = unCapitalize $ snakeToCamel op
        derived = ["Eq", "Ord", "Show"]
        mkFunc = "mk" <> ty
        toBitsFunc = lowerTy <> "ToBits"
        operandFunc = lowerTy <> "Operand"
        exports = [ ty
                  , mkFunc
                  , toBitsFunc
                  , operandFunc
                  ]
        sections = [ (("  , " <>) <$> exports)
                   , tySection
                   , ppInstance
                   , arbitraryInstance
                   , fieldDefsSection
                   , toBitsSection
                   , mkOpSection
                   , payloadSection
                   ]
        fieldIndent = "         " <> replicate (length ty * 2) ' '
        fullRecordFieldName f = lowerTy <> (capitalize $ fieldName f)
        fullFieldName f = lowerTy <> (capitalize $ fieldName f) <> "Field"
        formatRecordField f = fullRecordFieldName f <> " :: " <> fieldType f
        fieldDefsSection = concat $ intersperse [""] $ defineField <$> fields
        defineField f =
            [ fullFieldName f <> " :: Field"
            , fullFieldName f <> " = Field " <> (show $ fieldSize f) <> " " <> (show $ fieldOffset f)
            ]
        tySection = [ "data " <> ty <> " = " <> ty <> " { " <>
                        (intercalate ("\n" <> fieldIndent <> ", ") (formatRecordField <$> fields))
                    , fieldIndent <> "} deriving (" <> (intercalate ", " derived) <> ")"
                    ]
        ppInstance = [ "instance PP.Pretty " <> ty <> " where"
                     , "  pPrint _ = PP.text \"" <> ty <> ": not implemented\""
                     ]
        arbitraryInstance =
            let arbExpr = if null fields
                          then "pure " <> ty
                          else ty <> " <$> " <>
                               (intercalate " <*> " $ replicate (length fields) "A.arbitrary g")
            in [ "instance A.Arbitrary " <> ty <> " where"
               , "  arbitrary g = " <> arbExpr
               ]
        mkOpSection =
            let ls = extract <$> fields
                extract f = "(fromIntegral $ extract " <> fullFieldName f <> " w)"
            in [ mkFunc <> " :: Word32 -> " <> ty
               , mkFunc <> " w ="
               , "  " <> ty <> " " <> (concat (intersperse ("\n" <> replicate (length ty + 3) ' ') ls))
               ]
        toBitsSection =
            let ls = mkInsert <$> fields
                mkInsert f = "insert " <> fullFieldName f <> " (" <> fullRecordFieldName f <> " val)"
            in [ toBitsFunc <> " :: " <> ty <> " -> Word32"
               , toBitsFunc <> " val ="
               , "  " <> (concat $ intersperse " $\n  " ls) <> " 0"
               ]
        payloadSection = [ operandFunc <> " :: OperandPayload"
                         , operandFunc <> " ="
                         , "  OperandPayload { opTypeT = [t| " <> ty <> " |]"
                         , "                 , opConE  = Just (varE '" <> mkFunc <> ")"
                         , "                 , opWordE = Just (varE '" <> toBitsFunc <> ")"
                         , "                 }"
                         ]
    in unlines $ unlines <$> sections

main :: IO ()
main = do
    args <- getArgs
    let (opName:fieldStrings) = args

    fields <- forM fieldStrings $ \s ->
        case parseField s of
            Nothing -> do
                putStrLn $ "Invalid field: " <> s
                exitFailure
            Just f -> return f

    putStr $ boilerplate opName fields
