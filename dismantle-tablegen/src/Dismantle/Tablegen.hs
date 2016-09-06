{-# LANGUAGE TypeFamilies #-}
module Dismantle.Tablegen (
  parseTablegen,
  module Dismantle.Tablegen.Types
  ) where

import Control.Applicative
import qualified Control.Monad.State.Strict as St
import qualified Data.Map.Strict as M
import Data.Text.Lazy ( Text )
import Text.Megaparsec as P
import qualified Text.Megaparsec.Lexer as L

import Prelude

import Dismantle.Tablegen.Types

data TGState = TGState { internTable :: M.Map String String
                       }

internString :: String -> Parser String
internString s = do
  it <- St.gets internTable
  case M.lookup s it of
    Just s' -> return s'
    Nothing -> do
      St.modify' (\st -> st { internTable = M.insert s s (internTable st) })
      return s

type Parser = P.ParsecT P.Dec Text (St.State TGState)

header :: String -> Parser ()
header hdr = sc >> P.some (P.char '-') >> sc >> symbol hdr >> sc >> P.some (P.char '-') >> sc >> return ()

parseTablegen :: String -> Text -> Either (P.ParseError Char P.Dec) Records
parseTablegen fname t = St.evalState (P.runParserT p fname t) emptyState
  where
    emptyState = TGState M.empty

p :: Parser Records
p = do
  header "Classes"
  klasses <- P.some parseClass
  header "Defs"
  defs <- P.some parseDef
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
  decls <- P.some parseNamedDeclItem
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
  decls <- P.some parseNamedDeclItem
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

tryChoice :: [Parser a] -> Parser a
tryChoice = P.choice . fmap P.try

parseDeclType :: Parser DeclType
parseDeclType = tryChoice [ TGBits <$> (symbol "bits<" *> parseInt) <* symbol ">"
                         , TGBit <$ symbol "bit"
                         , TGString <$ symbol "string"
                         , TGInt <$ symbol "int"
                         , TGDag <$ symbol "dag"
                         , TGList <$> (symbol "list<" *> parseDeclType) <* symbol ">"
                         , TGFieldBits <$> (symbol "field" >> symbol "bits<" >> (parseInt <* symbol ">"))
                         , TGClass <$> name
                         ]

-- | Parse a decl item.
--
-- We try to parse the unknown value (?) first, as it fails fast and
-- should be an unambiguous parse.
parseDeclItem :: DeclType -> Parser DeclItem
parseDeclItem dt =
  tryChoice [ parseUnknownDeclItem dt, parseKnownDeclItem dt ]

parseUnknownDeclItem :: DeclType -> Parser DeclItem
parseUnknownDeclItem dt = UnknownItem dt <$ symbol "?"

parseKnownDeclItem :: DeclType -> Parser DeclItem
parseKnownDeclItem dt =
  case dt of
    TGBit -> BitItem <$> parseBit
    TGString ->
      tryChoice [ StringItem <$> lexeme parseStringLiteral
                , StringExprItem <$> P.someTill P.anyChar (P.char ';')
--                            (P.skipSome (P.satisfy (/= ';')) *> pure undefined)
                ]
    TGInt -> IntItem <$> lexeme parseInt
    TGFieldBits _ -> FieldBits <$> (symbol "{" *> P.sepBy1 (lexeme parseFieldItem) (symbol ",") <* symbol "}")
    TGDag -> DagItem <$ P.someTill P.anyChar (P.char ';')
     -- P.skipSome (P.satisfy (/= ';'))
    TGBits _ ->
      tryChoice [ ExpectedBits <$> (symbol "{" *> P.sepBy1 (lexeme parseBit) (symbol ",") <* symbol "}")
                , ExpectedUnknownBits <$> (symbol "{" *> P.sepBy1 (lexeme parseUnknownBit) (symbol ",") <* symbol "}")
                ]
    TGList dt' ->
      tryChoice [ ListItem <$> (symbol "[" *> P.sepBy1 (lexeme (parseKnownDeclItem dt')) (symbol ",") <* symbol "]")
                , ClassItem <$> lexeme name
                ]
    TGClass _ -> ClassItem <$> lexeme name

parseBit :: Parser Bool
parseBit = tryChoice [ False <$ symbol "0"
                    , True <$ symbol "1"
                    ]

parseUnknownBit :: Parser (Maybe Bool)
parseUnknownBit = tryChoice [ Just <$> parseBit
                           , Nothing <$ symbol "?"
                           ]

parseFieldItem :: Parser FieldItem
parseFieldItem =
  tryChoice [ ExpectedBit False <$ P.char '0'
           , ExpectedBit True <$ P.char '1'
           , FieldBit <$> name <*> (symbol "{" *> parseInt <* symbol "}")
           ]

parseStringLiteral :: Parser String
parseStringLiteral = (P.char '"' >> P.someTill P.anyChar (P.char '"')) >>= internString

parseMetadataComment :: Parser [Metadata]
parseMetadataComment = do
  tryChoice [ symbol "//" *> P.some parseMetadata, pure [] ]

parseMetadata :: Parser Metadata
parseMetadata = Metadata <$> name

parseClassParameters :: Parser [ClassParameter]
parseClassParameters =
  (symbol "<" *> P.sepBy parseClassParameter (symbol ",") <* symbol ">") <|> pure []

parseClassParameter :: Parser ClassParameter
parseClassParameter = do
  t <- parseDeclType
  n <- name
  _ <- symbol "="
  di <- parseDeclItem t
  return $ ClassParameter t n di

sc :: Parser ()
sc = P.hidden (P.skipMany P.spaceChar)
  -- L.space (void P.spaceChar) undefined undefined -- (return ()) (return ())

symbol :: String -> Parser String
symbol s = L.symbol sc s >>= internString

parseInt :: Parser Int
parseInt = fromIntegral <$> lexeme L.integer

lexeme :: Parser a -> Parser a
lexeme = L.lexeme sc

name :: Parser String
name = lexeme (P.some nameChar) >>= internString

nameChar :: Parser Char
nameChar = tryChoice [ P.alphaNumChar
                     , P.oneOf [ ':', '_' ]
                     ]
