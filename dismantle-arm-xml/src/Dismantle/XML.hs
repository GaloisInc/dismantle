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
import           Data.List (stripPrefix, find)
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
                  | MissingDecodeConstraints X.Element
                  | MissingName X.Element
                  | MissingVal X.Element
                  | MissingField X.Element String
                  | InvalidPattern String
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
loadXML fltr arch dirPath = runXML $ do
  fileStr <- MS.liftIO $ readFile (dirPath ++ "/a32_encindex.xml")
  let xmlContent = X.parseXML fileStr
  undefined
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
  matchPattern <- iclassMatchPattern iclass_sect
  fields <- iclassFields iclass_sect
  negPatterns <- iclassNegPatterns fields iclass_sect
  undefined

-- | Given an iclass_sect, process the regdiagram child to obtain a bitpattern to
-- match.
iclassMatchPattern :: X.Element -> XML [BT.Bit]
iclassMatchPattern iclass_sect = do
  rd <- case X.findChild (qname "regdiagram") iclass_sect of
          Just rd -> return rd
          Nothing -> E.throw (MissingRegDiagram iclass_sect)
  let boxes = X.findChildren (qname "box") rd
  mask <- fmap concat $ forM boxes $ \box -> do
    let width = read $ fromMaybe "1" (X.findAttr (qname "width") box)
        cs = X.findElements (qname "c") box
    boxMask <- fmap concat $ forM cs $ \c -> do
      let colspan = read $ fromMaybe "1" (X.findAttr (qname "colspan") c)
      case X.strContent c of
        "1" -> return $ [BT.ExpectedBit True]
        "(1)" -> return $ [BT.ExpectedBit True]
        "0" -> return $ [BT.ExpectedBit False]
        "(0)" -> return $ [BT.ExpectedBit False]
        _ -> return $ replicate colspan BT.Any
    if length boxMask == width
      then return boxMask
      else E.throw $ InvalidBoxWidth box
  return mask

-- | Get all the named fields from an iclass. This includes some bits that are
-- operand bits (things like "opc"), and some that are used to disambiguate the
-- instruction ("Rn", "cond", "S").
iclassFields :: X.Element -> XML [Field]
iclassFields iclass_sect = do
  rd <- case X.findChild (qname "regdiagram") iclass_sect of
    Just rd -> return rd
    Nothing -> E.throw (MissingRegDiagram iclass_sect)
  let boxes = X.findChildren (qname "box") rd
  fields <- fmap catMaybes $ forM boxes $ \box -> do
    let width = read $ fromMaybe "1" (X.findAttr (qname "width") box)
    case X.findAttr (qname "usename") box of
      Just "1" -> do
        name <- case X.findAttr (qname "name") box of
          Just name -> return name
          Nothing -> E.throw (InvalidBoxName box)
        hibit <- case X.findAttr (qname "hibit") box of
          Just hibit -> return $ read hibit
          Nothing -> E.throw (InvalidBoxHibit box)
        return $ Just $ Field { fieldName = name
                              , fieldHibit = hibit
                              , fieldWidth = width
                              }
      Nothing -> return Nothing
  return fields

-- NOTE: all decode_constraints have op="!=".
-- | Given a precomputed field list and an iclass, constraint all the negative bit
-- patterns from the <decode_constraint> element of the iclass.
--
-- Each <decode_constraint> looks like this:
--   <decode_constraint name="fld_1:...:fld_n" op="!=" val="a0a1...am" />
-- where each ai is a single digit, and m is equal to the sum of all the field widths
-- of fld_1...fld_n. We compute the overall bit pattern by starting with an open bit
-- pattern (all "Any"s) and for each field we encounter, setting that bit pattern
-- according to the corresponding sequence of "val"s.
iclassNegPatterns :: [Field]
                     -- ^ List of all fields from regdiagram
                  -> X.Element
                     -- ^ iclass
                  -> XML [[BT.Bit]]
iclassNegPatterns flds iclass_sect = do
  -- Find all decode_constraint elements
  constraints <- case X.findChild (qname "decode_constraints") iclass_sect of
    Just constraints -> return $ X.findChildren (qname "decode_constraint") constraints
    Nothing -> return [] -- no constraints
  -- For each one, compute the corresponding bit pattern
  forM constraints $ \constraint -> do
    names <- case X.findAttr (qname "name") constraint of
      Just name -> return $ LS.splitOn ":" name
      Nothing -> E.throw (MissingName constraint)
    constraintFields <- traverse lookupField names
    valPattern <- case X.findAttr (qname "val") constraint of
      Just valStr -> case traverse charToBit valStr of
        Nothing -> E.throw (InvalidPattern valStr)
        Just pat -> return pat
      Nothing -> E.throw (MissingVal constraint)
    computeConstraintPattern constraintFields valPattern (replicate 32 BT.Any)
  where lookupField :: String -> XML Field
        lookupField name = case find (\fld -> fieldName fld == name) flds of
          Nothing -> E.throw $ MissingField iclass_sect name
          Just fld -> return fld

        computeConstraintPattern :: [Field] -> [BT.Bit] -> [BT.Bit] -> XML [BT.Bit]
        computeConstraintPattern [] [] fullPat = return fullPat
        computeConstraintPattern (fld:flds) pat fullPat = do
          let (fldPat, rstPat) = splitAt (fieldWidth fld) pat
              fullPat' = placeAt (31 - fieldHibit fld) fldPat fullPat
          computeConstraintPattern flds rstPat fullPat'

placeAt :: Int -> [a] -> [a] -> [a]
placeAt ix subList l =
  let (prefix, rst) = splitAt ix l
      suffix = drop (length subList) rst
  in prefix ++ subList ++ suffix

charToBit :: Char -> Maybe BT.Bit
charToBit '1' = Just $ BT.ExpectedBit True
charToBit '0' = Just $ BT.ExpectedBit False
charToBit 'x' = Just $ BT.Any
charToBit _   = Nothing

data Constraint = Constraint { constraintName :: String
                             , constraintPattern :: [BT.Bit]
                             }

data Field = Field { fieldName :: String
                   , fieldHibit :: Int
                   , fieldWidth :: Int
                   }
  deriving (Eq, Show)

xmlLeaf :: [BT.Bit]
           -- ^ bit pattern to match iclass
        -> [[BT.Bit]]
           -- ^ list of negative bit patterns to rule out iclass
        -> [Field]
           -- ^ list of fields we are case-ing over
        -> X.Element
           -- ^ instruction table entry ("leaf")
        -> XML DT.InstructionDescriptor
xmlLeaf matchPattern negPatterns leaf = do
  undefined

-- | Given a leaf element, open up the referenced file to discover the correct
-- mnemonic.
leafMnemonic :: X.Element -> XML String
leafMnemonic = undefined
