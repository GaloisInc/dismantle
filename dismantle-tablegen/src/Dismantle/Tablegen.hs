{-# LANGUAGE TypeFamilies #-}
module Dismantle.Tablegen (
  parseTablegen,
  module Dismantle.Tablegen.Types
  ) where

import Control.Applicative
import Control.Monad ( void )
import Data.Text.Lazy ( Text )
import Text.Megaparsec as P
import Text.Megaparsec.Text.Lazy ( Parser )
import qualified Text.Megaparsec.Lexer as L

import Prelude

import Dismantle.Tablegen.Types

header :: String -> Parser ()
header hdr = P.some (P.char '-') >> sc >> P.string hdr >> sc >> P.some (P.char '-') >> return ()

parseTablegen :: String -> Text -> Either (P.ParseError Char P.Dec) Records
parseTablegen = P.runParser p

p :: Parser Records
p = do
  header "Classes"
  klasses <- P.many parseClass
  header "Defs"
  defs <- P.many parseDef
  return Records { tblClasses = klasses
                 , tblDefs = defs
                 }

parseClass :: Parser ClassDecl
parseClass = do
  _ <- symbol "class"
  n <- name
  tps <- parseClassParameters
  _ <- symbol "{"
  md <- parseMetadataComment
  decls <- P.many parseNamedDeclItem
  _ <- symbol "}"
  return ClassDecl { classDeclName = n
                   , classDeclParams = tps
                   , classDeclMetadata = md
                   , classDecls = decls
                   }

parseDef :: Parser Def
parseDef = do
  _ <- symbol "class"
  n <- name
  _ <- symbol "{"
  md <- parseMetadataComment
  decls <- P.many parseNamedDeclItem
  _ <- symbol "}"
  return Def { defName = n
             , defMetadata = md
             , defDecls = decls
             }

parseNamedDeclItem :: Parser (Named DeclItem)
parseNamedDeclItem = do
  t <- lexeme parseDeclType
  n <- lexeme name
  _ <- symbol "="
  di <- parseDeclItem t
  _ <- symbol ";"
  return $ Named n di

parseDeclType :: Parser DeclType
parseDeclType = P.choice [ TGBits <$> (symbol "bits<" *> parseInt) <* symbol ">"
                         , TGBit <$ symbol "bit"
                         , TGString <$ symbol "string"
                         , TGInt <$ symbol "int"
                         , TGDag <$ symbol "dag"
                         , TGList <$> (symbol "list<" *> parseDeclType) <* symbol ">"
                         , TGFieldBits <$> (symbol "field" >> symbol "bits<" >> (parseInt <* symbol ">"))
                         ]

-- | Parse a decl item.
--
-- We try to parse the unknown value (?) first, as it fails fast and
-- should be an unambiguous parse.
parseDeclItem :: DeclType -> Parser DeclItem
parseDeclItem dt = parseUnknownDeclItem dt <|> parseKnownDeclItem dt

parseUnknownDeclItem :: DeclType -> Parser DeclItem
parseUnknownDeclItem dt = UnknownItem dt <$ symbol "?"

parseKnownDeclItem :: DeclType -> Parser DeclItem
parseKnownDeclItem dt =
  case dt of
    TGBit -> BitItem <$> parseBit
    TGString -> P.choice [ StringItem <$> lexeme parseStringLiteral
                         , StringExprItem <$> undefined
                         ]
    TGInt -> IntItem <$> lexeme parseInt
    TGFieldBits _ -> FieldBits <$> (symbol "{" *> P.sepBy1 (lexeme parseFieldItem) (symbol ",") <* symbol "}")
    TGDag -> DagItem <$ P.skipMany (P.satisfy (/= ';'))
    TGBits _ ->
      P.choice [ ExpectedBits <$> (symbol "{" *> P.sepBy1 (lexeme parseBit) (symbol ",") <* symbol "}")
               , ExpectedUnknownBits <$> (symbol "{" *> P.sepBy1 (lexeme parseUnknownBit) (symbol ",") <* symbol "}")
               ]

parseBit :: Parser Bool
parseBit = P.choice [ False <$ symbol "0"
                    , True <$ symbol "1"
                    ]

parseUnknownBit :: Parser (Maybe Bool)
parseUnknownBit = P.choice [ Just <$> parseBit
                           , Nothing <$ symbol "?"
                           ]

parseFieldItem :: Parser FieldItem
parseFieldItem =
  P.choice [ ExpectedBit False <$ P.char '0'
           , ExpectedBit True <$ P.char '1'
           , FieldBit <$> name <*> (symbol "{" *> parseInt <* symbol "}")
           ]

parseStringLiteral :: Parser String
parseStringLiteral = symbol "\"" >> P.manyTill P.anyChar (symbol "\"")

parseMetadataComment :: Parser [Metadata]
parseMetadataComment = do
  (symbol "//" *> P.some parseMetadata) <|> pure []

parseMetadata :: Parser Metadata
parseMetadata = Metadata <$> name

parseClassParameters :: Parser [ClassParameter]
parseClassParameters =
  (symbol "<" *> P.many parseClassParameter <* symbol ">") <|> pure []

parseClassParameter :: Parser ClassParameter
parseClassParameter = undefined

sc :: Parser ()
sc = L.space (void P.spaceChar) (return ()) (return ())

symbol :: String -> Parser String
symbol = L.symbol sc

parseInt :: Parser Int
parseInt = fromIntegral <$> lexeme L.integer

lexeme :: Parser a -> Parser a
lexeme = L.lexeme sc

name :: Parser String
name = lexeme (P.some P.alphaNumChar)
