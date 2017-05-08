{-# LANGUAGE TypeFamilies #-}
-- | A parser for the output of the @llvm-tablegen@ tool's dumped output.
--
-- The full reference for the language is at
-- http://llvm.org/docs/TableGen/LangRef.html.  The current parser
-- only approximates that grammar.
module Dismantle.Tablegen.Parser (
  parseTablegen
  ) where

import Control.Applicative
import qualified Control.Monad.State.Strict as St
import qualified Data.List.Split as L
import qualified Data.Map.Strict as M
import Data.Text.Lazy ( Text )
import Text.Megaparsec as P
import qualified Text.Megaparsec.Lexer as L

import Prelude

import Dismantle.Tablegen.Parser.Types

parseTablegen :: String
              -- ^ The name of the file (used for error messages)
              -> Text
              -- ^ The content of the file to parse
              -> Either (P.ParseError Char P.Dec) Records
parseTablegen fname t = St.evalState (P.runParserT p fname t) emptyState
  where
    emptyState = TGState M.empty

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
  _ <- symbol "def"
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
parseDeclType =
  tryChoice [ TGFieldBits <$> (symbol "field" >> symbol "bits" >> P.between (symbol "<") (symbol ">") parseInt)
            , TGBits <$> (symbol "bits<" *> parseInt) <* symbol ">"
            , TGBit <$ symbol "bit"
            , TGString <$ symbol "string"
            , TGInt <$ symbol "int"
            , TGDag <$ symbol "dag"
            , TGList <$> (symbol "list<" *> parseDeclType) <* symbol ">"
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
    TGBit -> tryChoice [ BitItem <$> parseBit
                       , ExprItem <$> parseSimpleValue
                       ]
    TGString ->
      tryChoice [ StringItem <$> lexeme parseStringLiteral
                , ExprItem <$> parseSimpleValue
                ]
    TGInt ->
      tryChoice [ IntItem <$> lexeme parseInt
                , ExprItem <$> parseSimpleValue
                ]
    TGFieldBits _ ->
      FieldBits <$> P.between (symbol "{") (symbol "}") (P.sepBy1 (lexeme parseUnknownBit) (symbol ","))
    TGDag -> parseDAGItem
    TGBits _ ->
      tryChoice [ ExpectedBits <$> P.between (symbol "{") (symbol "}") (P.sepBy1 (lexeme parseBit) (symbol ","))
                , ExpectedUnknownBits <$> P.between (symbol "{") (symbol "}") (P.sepBy1 (lexeme parseUnknownBit) (symbol ","))
                ]
    TGList dt' ->
      tryChoice [ ListItem <$> P.between (symbol "[") (symbol "]") (P.sepBy (lexeme (parseKnownDeclItem dt')) (symbol ","))
                , ClassItem <$> lexeme name
                , ExprItem <$> parseSimpleValue
                ]
    TGClass _ ->
      tryChoice [ ClassItem <$> lexeme name
                , ExprItem <$> parseSimpleValue
                ]

parseDAGItem :: Parser DeclItem
parseDAGItem =
  tryChoice [ DagItem <$> Identifier <$> name
            , DagItem <$> parseSimpleValue
            ]

parseDagArg :: Parser DagArg
parseDagArg =
  tryChoice [ DagVarRef <$> parseVarRef
            , DagArg <$> parseSimpleValue <*> P.optional (P.char ':' *> parseVarRef)
            ]

parseVarRef :: Parser VarName
parseVarRef = VarName <$> (P.char '$' *> name)

parseSimpleValue :: Parser SimpleValue
parseSimpleValue =
  tryChoice [ Identifier <$> name
            , VNum <$> parseInt
            , VUnset <$ P.char '?'
            , VString <$> parseStringLiteral
            , VList <$> between (symbol "[") (symbol "]") (P.sepBy1 parseSimpleValue (symbol ","))
                    <*> P.optional parseType
            , VSequence <$> between (symbol "{") (symbol "}") (P.sepBy1 parseSimpleValue (symbol ","))
            , VAnonRecord <$> name <*> between (symbol "<") (symbol ">") (P.sepBy1 parseSimpleValue (symbol ","))
            , id <$> between (symbol "(") (symbol ")") (VDag <$> parseDagArg <*> P.sepBy parseDagArg (symbol ","))
            , VBang <$> parseBangOperator
                    <*> P.optional parseType
                    <*> between (symbol "(") (symbol ")") (P.sepBy1 parseSimpleValue (symbol ","))
            ]

parseType :: Parser String
parseType = between (symbol "<") (symbol ">") name

parseBangOperator :: Parser BangOperator
parseBangOperator = BangOperator <$> (P.char '!' *> name)

parseBit :: Parser Bool
parseBit = tryChoice [ False <$ symbol "0"
                    , True <$ symbol "1"
                    ]

parseUnknownBit :: Parser (Maybe BitRef)
parseUnknownBit = tryChoice [ Just <$> parseBitRef
                           , Nothing <$ symbol "?"
                           ]

parseBitRef :: Parser BitRef
parseBitRef =
  tryChoice [ ExpectedBit False <$ P.char '0'
            , ExpectedBit True <$ P.char '1'
            , FieldBit <$> name <*> (OBit <$> (P.between (symbol "{") (symbol "}") (lexeme parseInt)))
            , FieldBit <$> name <*> pure (OBit 0)
           ]

parseStringLiteral :: Parser String
parseStringLiteral =
  tryChoice [ parseMultilineStringLiteral
            , P.between (symbol "\"") (symbol "\"") (P.many (P.satisfy (flip notElem "\"\n"))) >>= internString
            ]

-- Multiline literals start with double quote followed by newline, and
-- contain lines until a line starts with optional whitespace followed
-- by a double quote.
parseMultilineStringLiteral :: Parser String
parseMultilineStringLiteral =
    tryChoice [ variant1
              , variant2
              ]
    where
      variant1 = do
        _ <- P.char '"'
        _ <- P.someTill (P.satisfy (/= '"')) P.eol
        lineStrs <- P.manyTill parseLine (P.try parseMultilineLiteralEnd)
        return (unlines lineStrs)
        where
          parseLine = P.manyTill P.anyChar P.eol
          parseMultilineLiteralEnd = P.manyTill (P.satisfy (flip notElem "\"\n")) (P.char '"')

      variant2 = do
        _ <- P.char '"'
        _ <- P.eol
        lineStrs <- P.manyTill parseLine (P.try parseMultilineLiteralEnd)
        return (unlines lineStrs)
        where
          parseLine = P.manyTill P.anyChar P.eol
          parseMultilineLiteralEnd = P.space >> P.char '"'

-- This is tricky -- we have to be careful parsing names.  If we use
-- the 'lexeme' approach, parsing the last one consumes the newline at
-- the end of the line and we can't tell when to stop.
--
-- We could change how whitespace works.  Instead of consuming the
-- whitespace *after* a token, just consume whitespace *before* the
-- token.  Then, we can consume each metadata comment until we see a
-- newline (which won't be consumed by 'lexeme')
parseMetadataComment :: Parser [Metadata]
parseMetadataComment = do
  tryChoice [ mkMetadata <$> (symbol "//" *> P.many (P.satisfy (/= '\n')) <* sc)
            , pure []
            ]
    where
      mkMetadata = map Metadata . L.splitOn " "

parseClassParameters :: Parser [ClassParameter]
parseClassParameters = P.label "ClassParameters" $
  tryChoice [ P.between (symbol "<") (symbol ">") (P.sepBy parseClassParameter (symbol ","))
            , pure []
            ]

parseClassParameter :: Parser ClassParameter
parseClassParameter = do
  t <- parseDeclType
  n <- name
  _ <- symbol "="
  di <- parseDeclItem t
  return $ ClassParameter t n di

sc :: Parser ()
sc = P.hidden (P.skipMany P.spaceChar)

symbol :: String -> Parser String
symbol s = L.symbol sc s >>= internString

parseInt :: Parser Int
parseInt =
  tryChoice [ (fromIntegral . negate) <$> (symbol "-" *> lexeme L.integer)
            , fromIntegral <$> lexeme L.integer
            ]

lexeme :: Parser a -> Parser a
lexeme = L.lexeme sc

name :: Parser String
name = P.label "name" $ lexeme (P.some nameChar) >>= internString

nameChar :: Parser Char
nameChar = tryChoice [ P.alphaNumChar
                     , P.oneOf [ '_', '$', '.', '?', ':' ]
                     ]
