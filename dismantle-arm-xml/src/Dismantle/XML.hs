{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}

module Dismantle.XML
  ( loadXML
  , XMLException(..)
  , DT.ISA(..)
  , DT.Endianness(..)
  , DT.OperandPayload(..)
  , DT.FormOverride(..)
  , DT.InstFieldDescriptor(..)
  , DT.UnusedBitsPolicy(..)
  ) where

import qualified Control.Exception as E
import           Control.Monad (forM)
import qualified Control.Monad.State.Strict as MS
import           Data.List (stripPrefix)
import           Data.List.Split as LS
import qualified Data.Map as M
import           Data.Maybe (catMaybes, fromMaybe)
import qualified Data.Set as S
import           Data.Void (Void)
import qualified System.IO.Strict as SIO
import qualified Text.Megaparsec as P
import           Text.Printf (printf)
import qualified Text.XML.Light as X

import qualified Dismantle.Tablegen as DT
import qualified Dismantle.Tablegen.ByteTrie as BT
import qualified Dismantle.Tablegen.Parser.Types as PT

data XMLException = InvalidXML
                  | MissingRegDiagram X.Element
                  | MultipleRegDiagrams X.Element
                  | MissingPSName X.Element
                  | MnemonicParseError FilePath String
                  | InvalidBoxColumn X.Element X.Element
                  | InvalidBoxWidth X.Element
                  | InvalidConstraint String
                  | InvalidBoxHibit X.Element
                  | InvalidBoxName X.Element
  deriving Show

instance E.Exception XMLException

-- | Monad for keeping track of how many times we've seen a particular mnemonic;
-- since there are duplicates, we need to add qualifiers to the names to distinguish
-- them from each other.
newtype XML a = XML (MS.StateT XMLState IO a)
  deriving ( Functor
           , Applicative
           , Monad
           , MS.MonadState XMLState
           , MS.MonadIO
           )

data XMLState = XMLState { usedNames :: M.Map String Int
                         , currentFileName :: Maybe FilePath
                         }

runXML :: XML a -> IO a
runXML (XML a) = MS.evalStateT a (XMLState M.empty Nothing)

qname :: String -> X.QName
qname str = X.QName str Nothing Nothing

-- | Given a path to the directory containing all XML instruction files, build an ISA
-- descriptor.
loadXML :: (X.Element -> Bool) -> String -> FilePath -> IO DT.ISADescriptor
loadXML fltr arch dirPath = runXML undefined
  -- instrs <- concat <$> mapM (loadXMLInstruction fltr arch) fps
  -- return $ DT.ISADescriptor { DT.isaInstructions = instrs
  --                           , DT.isaOperands = S.toList (S.fromList (concatMap instrOperandTypes instrs))
  --                           , DT.isaErrors = []
  --                           }

xmlEncodingIndex :: X.Element -> XML DT.ISADescriptor
xmlEncodingIndex encIx = do
  let iclass_sects = X.findElements (qname "iclass_sect") encIx
  undefined

xmlIclassSect :: X.Element -> XML [DT.InstructionDescriptor]
xmlIclassSect iclass_sect = do
  let matchPattern = iclassMatchPatterns iclass_sect
      negPatterns = iclassNegPatterns iclass_sect
  undefined

iclassMatchPattern :: X.Element -> [BT.Bit]
iclassMatchPattern = undefined

-- NOTE: all decode_constraints have op="!=".
iclassNegPatterns :: X.Element -> [[BT.Bit]]
iclassNegPatterns = undefined

data Field = Field { fieldName :: String
                   , fieldHibit :: Int
                   , fieldWidth :: Int
                   }

iclassFields :: X.Element -> [Field]
iclassFields = undefined

xmlLeaf :: [BT.Bit]
           -- ^ bit pattern to match iclass
        -> [[BT.Bit]]
           -- ^ list of negative bit patterns to rule out iclass
        -> [Field]
           -- ^ list of fields
        -> X.Element
           -- ^ instruction table entry ("leaf")
        -> XML DT.InstructionDescriptor
xmlLeaf matchPattern negPatterns leaf = do
  undefined

-- | Given a leaf element, open up the referenced file to discover the correct
-- mnemonic.
leafMnemonic :: X.Element -> XML String
leafMnemonic = undefined
