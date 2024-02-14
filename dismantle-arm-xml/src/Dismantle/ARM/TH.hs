{-# LANGUAGE GADTs #-}
{-|
Module           : Dismantle.ARM.TH
Copyright        : (c) Galois, Inc 2019-2020
Maintainer       : Daniel Matichuk <dmatichuk@galois.com>

Backend for "Dismantle.ARM.A32" and "Dismantle.ARM.T32".
Generates a disassembler from the ARM XML specification
and emits a map from each generated opcode to a corresponding
'ASL.Encoding'.

-}


{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveLift #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE DataKinds #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Dismantle.ARM.TH
  ( parseMask
  , genISA
  , armISADesc
  )
  where

import qualified Codec.Compression.GZip as CCG
import qualified Control.Monad.Fail as Fail
import qualified Data.Binary as DB
import qualified Data.BitMask as BM
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as LBS
import qualified Data.List as List
import qualified Data.Map as M
import           Data.Maybe ( fromJust )
import qualified Data.Parameterized.NatRepr as NR
import           Data.Parameterized.Some ( Some(..) )

import qualified Language.Haskell.TH as TH
import qualified Language.Haskell.TH.Syntax as TH

import           System.IO ( withFile, IOMode(..), hPutStrLn, Handle )
import           System.FilePath.Glob ( namesMatching )
import           System.FilePath ( (</>), (<.>) )

import qualified Dismantle.ARM.ASL as ASL
import qualified Dismantle.ARM.XML as ARM ( encodingOpToInstDescriptor, instDescriptorsToISA )
import qualified Dismantle.ARM.XML as XML
import qualified Dismantle.Tablegen as DT
import qualified Dismantle.Tablegen.LinearizedTrie as DTL
import qualified Dismantle.Tablegen.Patterns as BT
import qualified Dismantle.Tablegen.TH as DTH

-- | Top-level function for generating the template haskell for a given ISA.
genISA :: DT.ISA
       -- ^ the ISA for this disassembler (either A32 or T32)
       -> FilePath
       -- ^ the directory containing all of the XML specification files
       -> FilePath
       -- ^ the "encindex" XML file (either "t32_encindex.xml" or "a32_encindex.xml")
       -> FilePath
       -- ^ the full (relative) path to the parsed arm s-expression file: "arm_instrs.sexpr"
       -> FilePath
       -- ^ The path to pre-computed parse tables
       -> FilePath
       -- ^ file to write out logs to
       -> TH.DecsQ
genISA isa xmldirPath encIndexFile aslInstrs parseTables logFile = do
  -- The rest of the files are added by the base TH library
  TH.qAddDependentFile parseTables
  (desc, encodingops, files) <- TH.runIO $ withFile logFile WriteMode $ \handle -> do
    putStrLn $ "Dismantle.ARM.TH log: " ++ logFile
    armISADesc isa xmldirPath encIndexFile aslInstrs handle
  TH.runIO $ putStrLn "Successfully generated ISA description."
  aslMapDesc <- mkASLMap encodingops
  -- The full ASL specification consists of ~590 files, so we avoid using lazy
  -- IO to open each file. If we did, we would have all files open
  -- simultaneously, which would exceed the maximum number of file descriptors
  -- allowed by default on many operating systems (e.g., macOS, which has a
  -- default maximum of 256).
  --
  -- Instead of using LBS.readFile (which uses lazy IO), we use BS.readFile,
  -- which reads the entire contents of each file into memory and then closes
  -- the file. This ensures that we only have one of the ~590 files open at a
  -- time. This comes at the expense of loading more things into memory
  -- upfront, but each of these files are relatively small (~15K or so).
  inputBytes <- TH.runIO $ mapM (fmap LBS.fromStrict . BS.readFile) files
  let hash = DTL.computeHash inputBytes
  let asMaybeString :: DB.Get (Maybe String)
      asMaybeString = DB.get
  tableBytes <- TH.runIO $ LBS.readFile parseTables
  case DTL.deserialize asMaybeString hash (CCG.decompress tableBytes) of
    Left msg -> Fail.fail msg
    Right lt -> do
      isaDesc <- DTH.genISADesc isa desc files (Just lt)
      return $ isaDesc ++ aslMapDesc

-- | This function reads all of the ASL data and constructs the ISADescription
--
-- This is separated out from 'genISA' so that it can be called from helper
-- utilities
armISADesc :: DT.ISA
           -> FilePath
           -> FilePath
           -> FilePath
           -> Handle
           -> IO (DT.ISADescriptor, [(XML.Encoding, ASL.Encoding)], [FilePath])
armISADesc isa xmldirPath encIndexFile aslInstrs handle = do
  let doLog msg = hPutStrLn handle msg
  xmlFiles <- namesMatching (xmldirPath </> "*" <.> "xml")
  encodings <- XML.loadEncodings (DT.isaName isa) xmlFiles (xmldirPath </> encIndexFile) doLog
  pairedEncodings <- ASL.loadASL (DT.isaName isa) aslInstrs encodings doLog
  let xmlEncodings = map fst pairedEncodings
  let isaDesc = ARM.instDescriptorsToISA (map ARM.encodingOpToInstDescriptor xmlEncodings)
  -- NOTE: We sort the filenames to keep things consistent in case
  -- 'namesMatching' can exhibit non-determinism
  return (isaDesc, pairedEncodings, List.sort (aslInstrs : xmlFiles))

mkASLMap :: [(XML.Encoding, ASL.Encoding)] -> TH.DecsQ
mkASLMap encodings = do
  pairsTy <- [t| M.Map (Some ($(TH.conT (TH.mkName "Opcode")) $(TH.conT (TH.mkName "Operand")))) ASL.Encoding |]
  pairs <- mapM mkEncPair encodings
  let pairsExprName = TH.mkName "aslEncodingMap"
      mkTuple (opExpr, fun) =
          [e| (Some $(return opExpr), $(return fun)) |]

  pairsBody <- TH.ListE <$> mapM mkTuple pairs
  mapExpr <- [e| M.fromList $(return pairsBody) |]

  let pairsExpr = [ TH.SigD pairsExprName pairsTy
                  , TH.ValD (TH.VarP pairsExprName) (TH.NormalB mapExpr) []
                  ]

  return $ pairsExpr

mkEncPair :: (XML.Encoding, ASL.Encoding) -> TH.Q (TH.Exp, TH.Exp)
mkEncPair (xmlEncoding, aslEncoding) = do
  let conName = TH.mkName (XML.encName xmlEncoding)
  body <- [e| aslEncoding |]
  return $ (TH.ConE conName, body)

printMask :: BM.SomeBitMask BM.QuasiBit -> String
printMask (BM.SomeBitMask mask) = map printQBit $ BM.toList mask
  where
    printQBit :: BM.QuasiBit -> Char
    printQBit qb = case BM.unQuasiBit qb of
      (BT.ExpectedBit True, True) -> 'I'
      (BT.ExpectedBit False, True) -> 'O'
      (BT.Any, True) -> 'X'
      (BT.ExpectedBit True, False) -> '1'
      (BT.ExpectedBit False, False) -> '0'
      (BT.Any, False) -> 'x'

parseMask :: String -> BM.SomeBitMask BM.QuasiBit
parseMask maskStr |
  Just (Some nr) <- NR.someNat (length maskStr)
  , Just NR.LeqProof <- NR.testLeq (NR.knownNat @1) nr
  = BM.SomeBitMask $ fromJust $ BM.fromList nr $ map parseQBit maskStr
  where
    parseQBit :: Char -> BM.QuasiBit
    parseQBit c = BM.mkQuasiBit $ case c of
      'I' -> (BT.ExpectedBit True, True)
      'O' -> (BT.ExpectedBit False, True)
      'X' -> (BT.Any, True)
      'x' -> (BT.Any, False)
      '1' -> (BT.ExpectedBit True, False)
      '0' -> (BT.ExpectedBit False, False)
      _ -> error $ "parseMask: parseQBit: unexpected char: " ++ [c]
parseMask str = error $ "parseMask: unexpected string: " ++ show str

instance TH.Lift (BM.SomeBitMask BM.QuasiBit) where
  lift smask = do
    let str = printMask smask
    [e| parseMask str |]

deriving instance TH.Lift ASL.FieldConstraint
deriving instance TH.Lift ASL.Encoding
