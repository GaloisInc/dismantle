{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
module Dismantle.ASL (
  loadASL,
  DT.ISA(..),
  DT.Endianness(..),
  DT.OperandPayload(..),
  DT.FormOverride(..),
  DT.InstFieldDescriptor(..),
  DT.UnusedBitsPolicy(..)
  ) where

import           Control.Monad ( when )
import qualified Control.Monad.State.Strict as MS
import           Data.Either ( either )
import qualified Data.Foldable as F
import qualified Data.List.Split as L
import qualified Data.Map as M
import           Data.Maybe ( catMaybes, fromMaybe )
import qualified Data.Set as S
import qualified Data.Text as T
import           Data.Void ( Void )
import           Data.Word ( Word8 )
import qualified Text.Megaparsec as P
import           Text.Printf ( printf )
import qualified Language.ASL.Syntax as AS

import qualified Dismantle.Tablegen as DT
import qualified Dismantle.Tablegen.ByteTrie as BT
import qualified Dismantle.Tablegen.Parser.Types as PT

data ASLState = ASLState { usedNames :: M.Map String Int
                         }

newtype ASL a = ASL (MS.State ASLState a)
  deriving ( Functor
           , Applicative
           , Monad
           , MS.MonadState ASLState
           )

runASL :: ASL a -> a
runASL (ASL a) = MS.evalState a (ASLState M.empty)

loadASL :: (AS.InstructionEncoding -> Bool) -> String -> [AS.Instruction] -> DT.ISADescriptor
loadASL fltr arch insns = runASL $ do
  instrs <- concat <$> mapM (aslToInsnDesc fltr arch) insns
  return $ DT.ISADescriptor { DT.isaInstructions = instrs
                            , DT.isaOperands = S.toList (S.fromList (concatMap instrOperandTypes instrs))
                            , DT.isaErrors = []
                            }

instrOperandTypes :: DT.InstructionDescriptor -> [DT.OperandType]
instrOperandTypes idesc = map DT.opType (DT.idInputOperands idesc ++ DT.idOutputOperands idesc)

aslToInsnDesc :: (AS.InstructionEncoding -> Bool) -> String -> AS.Instruction -> ASL [DT.InstructionDescriptor]
aslToInsnDesc fltr arch i =
  catMaybes <$> mapM (encodingToInstDesc fltr arch) (AS.instEncodings i)

encodingToInstDesc :: (AS.InstructionEncoding -> Bool)
                   -> String
                   -> AS.InstructionEncoding
                   -> ASL (Maybe DT.InstructionDescriptor)
encodingToInstDesc fltr arch e
  | not (fltr e) = return Nothing
  | otherwise = do
      mnemonic <- encodingMnemonic e
      let desc = DT.InstructionDescriptor
                 { DT.idMask = concat (reverse (L.chunksOf 8 (fixedEncodingMask e)))
                 , DT.idNegMask = concat (reverse (L.chunksOf 8 (negativeEncodingMask e)))
                 , DT.idMnemonic = mnemonic
                 , DT.idInputOperands = map toOperandDescriptor (AS.encFields e)
                 , DT.idOutputOperands = [] -- See Note [Output Operands]
                 , DT.idNamespace = arch
                 , DT.idDecoderNamespace = show (AS.encInstrSet e)
                 , DT.idAsmString = asmString e mnemonic
                 , DT.idPseudo = False
                 , DT.idDefaultPrettyVariableValues = []
                 , DT.idPrettyVariableOverrides = []
                 }

      return (Just desc)

type Parser = P.Parsec Void T.Text

encodingMnemonic :: AS.InstructionEncoding -> ASL String
encodingMnemonic e = do
  let mn0 = T.unpack (either (const (AS.encName e)) id (P.runParser nameParser "" (AS.encName e)))
  names <- MS.gets usedNames
  case M.lookup mn0 names of
    Nothing -> do
      MS.modify' $ \s -> s { usedNames = M.insert mn0 0 (usedNames s) }
      return mn0
    Just nUses -> do
      MS.modify' $ \s -> s { usedNames = M.insertWith (\_ oldCount -> oldCount + 1) mn0 0 (usedNames s) }
      return (printf "%s_%d" mn0 nUses)

nameParser :: Parser T.Text
nameParser = do
  _ <- P.chunk (T.pack "aarch32_")
  P.takeRest

-- | The ASL specs don't have any information about textual encodings of
-- instructions.  We'll have to make something up here.
--
-- Ideally, our made up encodings will be close enough and just have one
-- variable slot for each field, in the order presented.  We'll also have to
-- build a translator from the fully elaborated names into shorter mnemonics
-- (i.e., dropping the aarch_ prefix and any suffixes)
--
-- FIXME: Add in field format slots
asmString :: AS.InstructionEncoding -> String -> String
asmString _ mn = mn

-- | Negative masks come from guards on fields.
negativeEncodingMask :: AS.InstructionEncoding -> [BT.Bit]
negativeEncodingMask e =
  case AS.encGuard e of
    Nothing -> trivial
    Just g ->
      case g of
        AS.ExprLitBin [True] -> trivial
        AS.ExprVarRef (AS.QualifiedIdentifier AS.ArchQualAny "TRUE") -> trivial
        AS.ExprBinOp AS.BinOpNEQ (AS.ExprVarRef (AS.QualifiedIdentifier _ ident)) (AS.ExprLitBin bv) ->
          fromMaybe err $ do
            (begin, off) <- lookupVarField ident e
            when (off /= length bv) $
              error ("Mismatch between field length and mask: " ++ show g)
            let chunk = map BT.ExpectedBit bv
            let suffix = replicate (32 - (begin + off)) BT.Any
            let prefix = replicate begin BT.Any
            let guardBits = concat [ suffix, chunk, prefix ]
            when (length guardBits /= 32) $ error "Invalid guard length"
            return guardBits
        _ -> err
      where
        err = error ("Unrecognized guard: " ++ show g)
  where
    trivial = replicate 32 BT.Any

lookupVarField :: T.Text -> AS.InstructionEncoding -> Maybe (Int, Int)
lookupVarField name e = do
  fld <- F.find ((==name) . AS.instFieldName) (AS.encFields e)
  return (fromIntegral (AS.instFieldBegin fld), fromIntegral (AS.instFieldOffset fld))

fixedEncodingMask :: AS.InstructionEncoding -> [BT.Bit]
fixedEncodingMask e = map toBTBit (AS.encOpcodeMask e)
  where
    toBTBit mb =
      case mb of
        AS.MaskBitSet -> BT.ExpectedBit True
        AS.MaskBitUnset -> BT.ExpectedBit False
        AS.MaskBitEither -> BT.Any

-- | For ASL, all fields are of type BV[N] (i.e., bitvectors of length N)
toOperandDescriptor :: AS.InstructionField -> DT.OperandDescriptor
toOperandDescriptor fld =
  DT.OperandDescriptor { DT.opName = T.unpack (AS.instFieldName fld)
                       , DT.opChunks = fieldToChunks fld
                       , DT.opType = fieldToType fld
                       }

-- | The operand descriptor allows operand bits to be scattered arbitrarily into
-- "chunks" within an encoded instruction.  The ARM ISAs don't use that (unless
-- they explicitly break a larger operand into multiple fields), so every field
-- will be converted into a single chunk.
fieldToChunks :: AS.InstructionField -> [(DT.IBit, PT.OBit, Word8)]
fieldToChunks fld = [( DT.IBit (fromIntegral (AS.instFieldBegin fld))
                     , PT.OBit 0
                     , fromIntegral (AS.instFieldOffset fld)
                     )]

fieldToType :: AS.InstructionField -> DT.OperandType
fieldToType fld = DT.OperandType (printf "bv%d" (AS.instFieldOffset fld))

{- Note [Output Operands]

Unlike the Tablegen data, the ASL specs don't have an explicit notion of output
operands (i.e., operands that are written but not read).  We don't really need
that in the decoder, though we could compute it by looking at the semantics.
For now, we are just treating all inputs as input operands.

-}
