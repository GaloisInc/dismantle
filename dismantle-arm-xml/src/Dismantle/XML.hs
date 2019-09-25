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

-- | Given a list of XML instruction files, build an ISA descriptor.
loadXML :: (X.Element -> Bool) -> String -> [FilePath] -> IO DT.ISADescriptor
loadXML fltr arch fps = runXML $ do
  instrs <- concat <$> mapM (loadXMLInstruction fltr arch) fps
  return $ DT.ISADescriptor { DT.isaInstructions = instrs
                            , DT.isaOperands = S.toList (S.fromList (concatMap instrOperandTypes instrs))
                            , DT.isaErrors = []
                            }

instrOperandTypes :: DT.InstructionDescriptor -> [DT.OperandType]
instrOperandTypes idesc = map DT.opType (DT.idInputOperands idesc ++ DT.idOutputOperands idesc)

-- | Given a path to an XML instruction file, extract a list of instruction
-- descriptors for each encoding in the file.
loadXMLInstruction :: (X.Element -> Bool) -> String -> FilePath -> XML [DT.InstructionDescriptor]
loadXMLInstruction fltr arch fp = do
  s <- MS.liftIO $ SIO.readFile fp -- FIXME: file read error?
  MS.liftIO $ putStr $ "processing XML file: " ++ fp
  case X.parseXMLDoc s of
    Nothing -> E.throw InvalidXML
    Just xmlContent -> do
      let mType = X.findAttr (qname "type") xmlContent
      case mType of
        Just "instruction" -> do
          MS.modify' $ \s -> s { currentFileName = Just fp }
          let iclasses = filter fltr (X.findElements (qname "iclass") xmlContent)
          descriptors <- mapM (iclassToInsnDesc arch) iclasses
          MS.modify' $ \s -> s { currentFileName = Nothing }
          MS.liftIO $ putStrLn " good"
          return descriptors
        _ -> MS.liftIO (putStrLn " bad") >> return []

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
             , DT.idOutputOperands = []
             , DT.idNamespace = arch
             -- FIXME: Is this ok for decoder namespace? (compare with ASL)
             , DT.idDecoderNamespace = arch
             , DT.idAsmString = asmString iclass mnemonic
             , DT.idPseudo = False
             , DT.idDefaultPrettyVariableValues = []
             , DT.idPrettyVariableOverrides = []
             }
  return desc

-- | FIXME: We should be able to generate the asm string from the XML, but I'm
-- punting on that for now.
asmString :: X.Element -> String -> String
asmString _iclass mn = mn

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
      Left err -> mnemonicErr nm
        -- case X.findChildren (qname "encoding") iclass of
        --   [enc] -> case X.findAttr (qname "name") enc of
        --     Just nm' -> processName nm'
        --     Nothing -> mnemonicErr nm
        --   _ -> mnemonicErr nm
      Right nm -> processName nm
  where mnemonicErr :: String -> XML a
        mnemonicErr nm = do fileName <- MS.gets currentFileName
                            E.throw $ MnemonicParseError (fromMaybe "" fileName) nm
        processName :: String -> XML String
        processName nm = do
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
