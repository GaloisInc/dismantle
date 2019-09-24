{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}

module Dismantle.XML
  ( DT.ISA(..)
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
import           Data.Void (Void)
import qualified Text.Megaparsec as P
import           Text.Printf (printf)
import qualified Text.XML.Light as X

import qualified Dismantle.Tablegen as DT
import qualified Dismantle.Tablegen.ByteTrie as BT
import qualified Dismantle.Tablegen.Parser.Types as PT

data XMLException = InvalidXML
                  | MissingRegDiagram X.Element
                  | MissingPSName X.Element
                  | MnemonicParseError (P.ParseErrorBundle String Void)
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

data XMLState = XMLState { usedNames :: M.Map String Int }

runXML :: XML a -> IO a
runXML (XML a) = MS.evalStateT a (XMLState M.empty)

qname :: String -> X.QName
qname str = X.QName str Nothing Nothing

-- | Given a path to an XML instruction file, extract a list of instruction
-- descriptors for each encoding in the file.
loadXMLInstruction :: (X.Element -> Bool) -> String -> FilePath -> XML [DT.InstructionDescriptor]
loadXMLInstruction fltr arch fp = do
  s <- MS.liftIO $ readFile fp -- FIXME: file read error?
  case X.parseXMLDoc s of
    Nothing -> E.throw InvalidXML
    Just xmlContent -> do
      let iclasses = filter fltr (X.findElements (qname "iclass") xmlContent)
      descriptors <- mapM (iclassToInsnDesc arch) iclasses
      return descriptors

iclassToInsnDesc :: String -> X.Element -> XML DT.InstructionDescriptor
iclassToInsnDesc arch iclass = do
  mnemonic <- encodingMnemonic iclass
  idMask <- fixedEncodingMask iclass
  idNegMasks <- negativeEncodingMasks iclass
  idInputOperands <- operandDescriptors iclass
  let desc = DT.InstructionDescriptor
             { DT.idMask = concat (reverse (LS.chunksOf 8 idMask))
             , DT.idNegMasks = concat <$> reverse <$> LS.chunksOf 8 <$> idNegMasks
             , DT.idMnemonic = mnemonic
             , DT.idInputOperands = idInputOperands
             , DT.idOutputOperands = undefined
             , DT.idNamespace = undefined
             , DT.idDecoderNamespace = undefined
             , DT.idAsmString = undefined
             , DT.idPseudo = undefined
             , DT.idDefaultPrettyVariableValues = undefined
             , DT.idPrettyVariableOverrides = undefined
             }
  return desc

regdiagram :: X.Element -> XML X.Element
regdiagram iclass = case X.findElement (qname "regdiagram") iclass of
  Nothing -> E.throw $ MissingRegDiagram iclass
  Just regdiagram -> return regdiagram

type Parser = P.Parsec Void String

-- | Given the iclass of an encoding, construct the mnemonic.
encodingMnemonic :: X.Element -> XML String
encodingMnemonic iclass = do
  rd <- regdiagram iclass
  case X.findAttr (qname "psname") rd of
    Nothing -> E.throw $ MissingPSName rd
    Just nm -> case P.runParser nameParser "" nm of
      Left err -> E.throw $ MnemonicParseError err
      Right nm -> do
        names <- MS.gets usedNames
        case M.lookup nm names of
          Nothing -> do
            MS.modify' $ \s -> s { usedNames = M.insert nm 0 (usedNames s) }
            return nm
          Just nUses -> do
            MS.modify' $ \s -> s { usedNames = M.insertWith (\_ oldCount -> oldCount + 1) nm 0 (usedNames s) }
            return (printf "%s_%d" nm nUses)

nameParser :: Parser String
nameParser = do
  _ <- P.chunk "aarch32/instrs/"
  instrName <- P.takeWhileP Nothing (/= '/')
  _ <- P.chunk "/"
  encName <- P.takeWhileP Nothing (/= '.')
  _ <- P.chunk ".txt"
  return $ instrName <> "_" <> encName

-- | Given the iclass of an encoding, construct the fixed encoding mask.
fixedEncodingMask :: X.Element -> XML [BT.Bit]
fixedEncodingMask iclass = do
  rd <- regdiagram iclass
  let boxes = X.findElements (qname "box") rd
  mask <- fmap concat $ forM boxes $ \box -> do
    let width = read $ fromMaybe "1" (X.findAttr (qname "width") box)
        cs = X.findElements (qname "c") box
    boxMask <- fmap concat $ forM cs $ \c -> do
      let colspan = read $ fromMaybe "1" (X.findAttr (qname "colspan") c)
      case X.strContent c of
        "1" -> return $ [BT.ExpectedBit True]
        "0" -> return $ [BT.ExpectedBit False]
        _   -> return $ replicate colspan BT.Any
    if length boxMask == width
      then return boxMask
      else E.throw $ InvalidBoxWidth box
  return mask

-- | Given the iclass of an encoding, construct the negated encoding masks.
negativeEncodingMasks :: X.Element -> XML [[BT.Bit]]
negativeEncodingMasks iclass = do
  rd <- regdiagram iclass
  let boxes = X.findElements (qname "box") rd
  constraintMasks <- fmap catMaybes $ forM boxes $ \box -> case X.findAttr (qname "constraint") box of
    Nothing -> return Nothing
    Just constraintStr -> case stripPrefix "!= " constraintStr of
      Nothing -> E.throw $ InvalidConstraint constraintStr
      Just bitString -> do
        hibit <- case X.findAttr (qname "hibit") box of
          Nothing -> E.throw $ InvalidBoxHibit box
          Just s -> return $ read s
        let boxWidth = read $ fromMaybe "1" (X.findAttr (qname "width") box)
            constraintBits = readBit <$> bitString
            leftBits = replicate (31 - hibit) BT.Any
            rightBits = replicate (hibit - boxWidth + 1) BT.Any
        return $ Just $ leftBits ++ constraintBits ++ rightBits
  return constraintMasks
  where readBit '0' = BT.ExpectedBit False
        readBit '1' = BT.ExpectedBit True

operandDescriptors :: X.Element -> XML [DT.OperandDescriptor]
operandDescriptors iclass = do
  rd <- regdiagram iclass
  let boxes = X.findElements (qname "box") rd
  descs <- fmap catMaybes $ forM boxes $ \box -> case X.findAttr (qname "usename") box of
    Just "1" -> do
      name <- case X.findAttr (qname "name") box of
        Nothing -> E.throw $ InvalidBoxName box
        Just name -> return name
      hibit <- case X.findAttr (qname "hibit") box of
        Nothing -> E.throw $ InvalidBoxHibit box
        Just s -> return $ read s
      let boxWidth = read $ fromMaybe "1" (X.findAttr (qname "width") box)
      let desc = DT.OperandDescriptor { DT.opName = name
                                      , DT.opChunks = [( DT.IBit (hibit - boxWidth + 1)
                                                       , PT.OBit 0
                                                       , fromIntegral boxWidth)]
                                      , DT.opType = DT.OperandType (printf "bv%d" boxWidth)
                                      }
      return $ Just desc
    Nothing -> return Nothing
  return descs
