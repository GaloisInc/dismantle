module Dismantle.Tablegen.Types (
  Records(..),
  ClassDecl(..),
  Def(..),
  ClassParameter(..),
  Metadata(..),
  Named(..),
  BitRef(..),
  DeclItem(..),
  DeclType(..),
  Expr(..)
  ) where

data Records =
  Records { tblClasses :: [ClassDecl]
          , tblDefs :: [Def]
          }
  deriving (Show)

data ClassDecl =
  ClassDecl { classDeclName :: String
            , classDeclParams :: [ClassParameter]
            , classDeclMetadata :: [Metadata]
            , classDecls :: [Named DeclItem]
            }
  deriving (Show)

data Def =
  Def { defName :: String
      , defMetadata :: [Metadata]
      , defDecls :: [Named DeclItem]
      }
  deriving (Show)

data ClassParameter =
  ClassParameter DeclType String DeclItem
  deriving (Show)

data Metadata =
  Metadata String
  deriving (Show)

data Named a =
  Named String a
  deriving (Show)

data DeclItem =
  BitItem !Bool
  | IntItem !Int
  | StringItem String
  | StringExprItem String
  | FieldBits [Maybe BitRef]
  | ExpectedBits [Bool]
  | ExpectedUnknownBits [Maybe BitRef]
  | DagItem
  | ListItem [DeclItem]
  | ClassItem String
  | ExprItem Expr
  | UnknownItem DeclType
  deriving (Show)

data Expr = ENegate Expr
          | EFuncall String [String] [Expr]
          | ERef String
          | EString String
          | EInt Int
          deriving (Show)

data BitRef = ExpectedBit !Bool
            | FieldBit String Int
            | FieldVarRef String
            deriving (Show)

data DeclType = TGBit
              | TGBits !Int
              | TGString
              | TGInt
              | TGDag
              | TGFieldBits !Int
              | TGList DeclType
              | TGClass String
  deriving (Show)

