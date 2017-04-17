{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
-- | This module contains low-level types used by the parser.
--
-- It is intended that users never see these types.  Instead, users
-- should only be exposed to the types in Dismantle.Tablegen.Types.
module Dismantle.Tablegen.Parser.Types (
  Records(..),
  ClassDecl(..),
  Def(..),
  ClassParameter(..),
  Metadata(..),
  Named(..),
  BitRef(..),
  DeclItem(..),
  DeclType(..),
  DagArg(..),
  VarName(..),
  BangOperator(..),
  SimpleValue(..)
  ) where

import GHC.Generics (Generic)
import Control.DeepSeq

data Records =
  Records { tblClasses :: [ClassDecl]
          , tblDefs :: [Def]
          }
  deriving (Show, Generic, NFData)

data ClassDecl =
  ClassDecl { classDeclName :: String
            , classDeclParams :: [ClassParameter]
            , classDeclMetadata :: [Metadata]
            , classDecls :: [Named DeclItem]
            }
  deriving (Show, Generic, NFData)

data Def =
  Def { defName :: String
      , defMetadata :: [Metadata]
      , defDecls :: [Named DeclItem]
      }
  deriving (Show, Generic, NFData)

data ClassParameter =
  ClassParameter DeclType String DeclItem
  deriving (Show, Generic, NFData)

data Metadata =
  Metadata String
  deriving (Show, Eq, Generic, NFData)

data Named a =
  Named { namedName :: String
        , namedValue :: a
        }
  deriving (Show, Generic, NFData)

data DeclItem =
  BitItem !Bool
  | IntItem !Int
  | StringItem String
  | StringExprItem String
  | FieldBits [Maybe BitRef]
  | ExpectedBits [Bool]
  | ExpectedUnknownBits [Maybe BitRef]
  | DagItem SimpleValue
  | ListItem [DeclItem]
  | ClassItem String
  | ExprItem SimpleValue
  | UnknownItem DeclType
  deriving (Show, Generic, NFData, Eq)

data DagArg = DagArg SimpleValue (Maybe VarName)
            -- ^ Ideally, we could fill in the Maybe here.  In
            -- practice, it is ambiguous while parsing, since : seems
            -- to appear in identifier names (even though it
            -- shouldn't).
            | DagVarRef VarName
  deriving (Show, Generic, NFData, Eq)

-- | A reference to a variable name - the string does not include the $
data VarName = VarName String
  deriving (Show, Generic, NFData, Eq)

data BitRef = ExpectedBit !Bool
            | FieldBit String Int
            | FieldVarRef String
            deriving (Show, Generic, NFData, Eq)

data DeclType = TGBit
              | TGBits !Int
              | TGString
              | TGInt
              | TGDag
              | TGFieldBits !Int
              | TGList DeclType
              | TGClass String
  deriving (Show, Generic, NFData, Eq)

data BangOperator = BangOperator String
  deriving (Show, Generic, NFData, Eq)

data SimpleValue = Identifier String
                 | VString String
                 | VNum !Int
                 | VUnset
                 | VList [SimpleValue] (Maybe String)
                 -- ^ A [] list with an optional type specifier
                 | VSequence [SimpleValue]
                 -- ^ A {} list
                 | VAnonRecord String [SimpleValue]
                 | VDag DagArg [DagArg]
                 | VBang BangOperator (Maybe String) [SimpleValue]
  deriving (Show, Generic, NFData, Eq)
