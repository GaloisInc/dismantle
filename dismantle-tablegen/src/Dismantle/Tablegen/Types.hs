module Dismantle.Tablegen.Types (
  Records(..),
  ClassDecl(..),
  ClassParameter(..),
  Metadata(..),
  Named(..),
  DeclItem(..),
  DeclType(..)
  ) where

data Records =
  Records { tblClasses :: [ClassDecl]
          , tblDefs :: [Int]
          }
  deriving (Show)

data ClassDecl =
  ClassDecl { classDeclName :: String
            , classDeclParams :: [ClassParameter]
            , classDeclMetadata :: [Metadata]
            , classDecls :: [Named DeclItem]
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
  BitItem Bool
  | StringItem String
  | StringExprItem String
  | UnknownItem DeclType
  deriving (Show)

-- data DeclItem =
--   DeclItem DeclType String DeclValue
--   deriving (Show)

data DeclType = TGBit
              | TGString
              | TGInt
              | TGDag
              | TGFieldBits Int
              | TGList DeclType
  deriving (Show)

