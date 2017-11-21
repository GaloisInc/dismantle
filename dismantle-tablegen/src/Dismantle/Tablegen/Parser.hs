{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE FlexibleInstances #-}
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
import Data.Text.Lazy ( Text, unpack )
import Text.Megaparsec as P
import qualified Text.Megaparsec.Char as P
import qualified Text.Megaparsec.Char.Lexer as L

import Prelude

import Dismantle.Tablegen.Parser.Types

-- For testing internal functions in this module.
{-
import Data.Text.Lazy ( pack )
import Text.Printf

testParseTableGen file = do
  content <- pack <$> readFile file
  case parseTablegen "<test>" content of
    Left e -> printf e
    Right Records {..} -> do
      printf "%i classes, %i defs\n" (length tblClasses) (length tblDefs)

testParser :: Parser a -> Text -> IO ()
testParser parser input = do
  let result = St.evalState
        (P.runParserT parser "<test>" (unpack input)) emptyState
  case result of
    Left e -> printf $ P.parseErrorPretty' input e
    Right _ -> printf "success!\n"
  where
    emptyState = TGState M.empty

-}

-- | So that parse errors can be pretty printed.
instance P.ShowErrorComponent String where
  showErrorComponent = id

parseTablegen :: String
              -- ^ The name of the file (used for error messages)
              -> Text
              -- ^ The content of the file to parse
              -> Either String Records
parseTablegen fname t =
  case St.evalState (P.runParserT p fname (unpack t)) emptyState of
    -- Render the parse error, including the line where it occurred.
    Left e -> Left $ P.parseErrorPretty' t e
    Right r -> Right r
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

type Parser = P.ParsecT String String (St.State TGState)

header :: String -> Parser ()
header hdr = sc >> P.some (P.char '-') >> sc >> symbol hdr >> sc >> P.some (P.char '-') >> sc

-- | Parse a sequence of classes followed by a sequence of defs,
-- skipping @/* ... */@-comments between (but not inside!).
p :: Parser Records
p = do
  skipComments
  header "Classes"
  skipComments
  klasses <- P.many (parseClass <* skipComments)
  skipComments
  header "Defs"
  skipComments
  defs <- P.many (parseDef <* skipComments)
  skipComments
  return Records { tblClasses = klasses
                 , tblDefs = defs
                 }

-- | Skip C-style @/* ... */@ range comments. Like in the TableGen
-- spec, and unlike in C, range comments *can* be nested.
--
-- Not skipping @// ...@ comments since those are meaningful in some
-- places as metadata comments.
skipComments :: Parser ()
skipComments = P.skipMany (L.skipBlockCommentNested "/*" "*/" *> sc)

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
            , TGCode <$ symbol "code"
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
                       , ExprItem <$> parseSimpleValue NoEmbeddedStrings
                       ]
    TGString ->
      tryChoice [ StringItem <$> lexeme (parseStringLiteral WithEmbeddedStrings)
                , ExprItem <$> parseSimpleValue NoEmbeddedStrings
                ]
    TGInt ->
      tryChoice [ IntItem <$> lexeme parseInt
                , ExprItem <$> parseSimpleValue NoEmbeddedStrings
                ]
    TGFieldBits _ ->
      FieldBits <$> P.between (symbol "{") (symbol "}") (P.sepBy1 (lexeme parseUnknownBit) (symbol ","))
    TGDag -> parseDAGItem
    TGCode -> parseDAGItem
    TGBits _ ->
      tryChoice [ ExpectedBits <$> P.between (symbol "{") (symbol "}") (P.sepBy1 (lexeme parseBit) (symbol ","))
                , ExpectedUnknownBits <$> P.between (symbol "{") (symbol "}") (P.sepBy1 (lexeme parseUnknownBit) (symbol ","))
                ]
    TGList dt' ->
      tryChoice [ ListItem <$> P.between (symbol "[") (symbol "]") (P.sepBy (lexeme (parseKnownDeclItem dt')) (symbol ","))
                , ClassItem <$> lexeme name
                , ExprItem <$> parseSimpleValue NoEmbeddedStrings
                ]
    TGClass _ ->
      tryChoice [ ClassItem <$> lexeme name
                , ExprItem <$> parseSimpleValue NoEmbeddedStrings
                ]

parseDAGItem :: Parser DeclItem
parseDAGItem =
  tryChoice [ DagItem <$> Identifier <$> name
            , DagItem <$> parseSimpleValue NoEmbeddedStrings
            ]

parseDagArg :: Parser DagArg
parseDagArg =
  tryChoice [ DagVarRef <$> parseVarRef
            , DagArg <$> parseSimpleValue NoEmbeddedStrings <*> P.optional (P.char ':' *> parseVarRef)
            ]

parseVarRef :: Parser VarName
parseVarRef = VarName <$> (P.char '$' *> name)

data MultilineParseConfig = NoEmbeddedStrings
                          | WithEmbeddedStrings

parseSimpleValue :: MultilineParseConfig -> Parser SimpleValue
parseSimpleValue cfg =
  tryChoice [ Identifier <$> name
            , VNum <$> parseInt
            , VUnset <$ P.char '?'
            , VString <$> parseStringLiteral cfg
            , VList <$> between (symbol "[") (symbol "]") (P.sepBy1 (parseSimpleValue NoEmbeddedStrings) (symbol ","))
                    <*> P.optional parseType
            , VCode <$> (symbol "[{" *> P.manyTill P.anyChar (symbol "}]"))
            , VSequence <$> between (symbol "{") (symbol "}") (P.sepBy (parseSimpleValue NoEmbeddedStrings) (symbol ","))
            , VAnonRecord <$> name <*> between (symbol "<") (symbol ">") (P.sepBy1 (parseSimpleValue NoEmbeddedStrings) (symbol ","))
            , id <$> between (symbol "(") (symbol ")") (VDag <$> parseDagArg <*> P.sepBy parseDagArg (symbol ","))
            , VBang <$> parseBangOperator
                    <*> P.optional parseType
                    <*> between (symbol "(") (symbol ")") (P.sepBy1 (parseSimpleValue NoEmbeddedStrings) (symbol ","))
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

parseStringLiteral :: MultilineParseConfig -> Parser String
parseStringLiteral cfg =
  tryChoice [ parseMultilineStringLiteral cfg
            , P.between (symbol "\"") (symbol "\"") (P.many (P.satisfy (flip notElem "\"\n"))) >>= internString
            ]

-- Multiline literals start with double quote followed by newline, and
-- contain lines until a line starts with optional whitespace followed
-- by a double quote.
--
-- The contents of a multi-line literal can many of:
--
-- 1) A quoted string literal
--
-- 2) Any other character besides '"'
parseMultilineStringLiteral :: MultilineParseConfig -> Parser String
parseMultilineStringLiteral cfg = do
    _ <- P.char '"'
    pfx <- P.manyTill (P.satisfy (`notElem` "\"\n")) P.eol
    -- After this, we are in a state machine where we just match a quoted
    -- string literal and normal characters (including newlines).  The
    -- quoted string literals cannot contain newlines.  If we find a single
    -- quote with no matching end quote on the same line, that is the end of
    -- the string.
    s <- goChar []
    return (pfx ++ "\n" ++ s)
    where
      goChar :: [String] -> Parser String
      goChar acc = do
        let parseSingleLineQuotedString = Just <$> P.between (P.char '"') (P.char '"') (P.many (P.satisfy (`notElem` "\"\n")))
            parseAnyButQuote = (Just . (:[])) <$> P.satisfy (`notElem` "\"")
            parseOnlyQuote = Nothing <$ P.char '"'
            curParser = case cfg of
              NoEmbeddedStrings -> tryChoice [ parseAnyButQuote, parseOnlyQuote ]
              WithEmbeddedStrings -> tryChoice [ parseSingleLineQuotedString
                                               , parseAnyButQuote
                                               , parseOnlyQuote
                                               ]
        elt <- curParser
        case elt of
          Nothing -> return (concat (reverse acc))
          Just s -> goChar (s : acc)

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
  tryChoice [ negate <$> (symbol "-" *> lexeme L.decimal)
            , lexeme L.decimal
            ]

lexeme :: Parser a -> Parser a
lexeme = L.lexeme sc

name :: Parser String
name = P.label "name" $ lexeme (P.some nameChar) >>= internString

nameChar :: Parser Char
nameChar = tryChoice [ P.alphaNumChar
                     , P.oneOf [ '_', '$', '.', '?', ':' ]
                     ]
