module Dismantle.Tablegen.Types (
  Records(..),
  ClassDecl(..),
  Def(..),
  ClassParameter(..),
  Metadata(..),
  Named(..),
  DeclItem(..),
  DeclType(..)
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
  ClassParameter
  deriving (Show)

data Metadata =
  Metadata
  deriving (Show)

data Named a =
  Named String a
  deriving (Show)

data DeclItem =
  BitItem !Bool
  | IntItem !Int
  | StringItem String
  | StringExprItem String
  | UnknownItem DeclType
  deriving (Show)

-- data DeclItem =
--   DeclItem DeclType String DeclValue
--   deriving (Show)

data DeclType = TGBit
              | TGBits !Int
              | TGString
              | TGInt
              | TGDag
              | TGFieldBits !Int
              | TGList DeclType
  deriving (Show)

