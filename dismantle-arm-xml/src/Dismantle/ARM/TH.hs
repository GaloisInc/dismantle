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
  )
  where

import           Data.Maybe ( fromJust )
import qualified Data.Map as M
import qualified Data.BitMask as BM
import qualified Data.Parameterized.NatRepr as NR
import           Data.Parameterized.Some ( Some(..) )

import qualified Language.Haskell.TH as TH
import qualified Language.Haskell.TH.Syntax as TH

import           System.IO ( withFile, IOMode(..), hPutStrLn )
import           System.FilePath.Glob ( namesMatching )
import           System.FilePath ( (</>), (<.>) )

import qualified Dismantle.Tablegen as DT
import qualified Dismantle.ARM.XML as XML
import qualified Dismantle.ARM.ASL as ASL
import qualified Dismantle.ARM.XML as ARM ( encodingOpToInstDescriptor, instDescriptorsToISA )
import qualified Dismantle.Tablegen.TH as DTH
import qualified Dismantle.Tablegen.Patterns as BT

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
       -- ^ file to write out logs to
       -> TH.DecsQ
genISA isa xmldirPath encIndexFile aslInstrs logFile = do
  xmlFiles <- TH.runIO $ namesMatching (xmldirPath </> "*" <.> "xml")
  (desc, encodingops) <- TH.runIO $ withFile logFile WriteMode $ \handle -> do
    let doLog msg = hPutStrLn handle msg
    putStrLn $ "Dismantle.ARM.TH log: " ++ logFile
    encodings <- XML.loadEncodings (DT.isaName isa) xmlFiles (xmldirPath </> encIndexFile) doLog
    pairedEncodings <- ASL.loadASL (DT.isaName isa) aslInstrs encodings doLog
    let xmlEncodings = map fst pairedEncodings
    return $ (ARM.instDescriptorsToISA $ map ARM.encodingOpToInstDescriptor xmlEncodings, pairedEncodings)
  TH.runIO $ putStrLn "Successfully generated ISA description."
  let files = aslInstrs : xmlFiles
  aslMapDesc <- mkASLMap encodingops
  isaDesc <- DTH.genISADesc isa desc files
  return $ isaDesc ++ aslMapDesc

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
