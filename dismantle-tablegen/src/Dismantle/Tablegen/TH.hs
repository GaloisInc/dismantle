{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE ViewPatterns #-}
module Dismantle.Tablegen.TH (
  genISA
  ) where

import GHC.TypeLits ( Symbol )

import qualified Codec.Compression.GZip as Z
import Control.Arrow ( (&&&) )
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as LBS
import qualified Data.ByteString.Unsafe as BS
import Data.Char ( toUpper )
import qualified Data.Foldable as F
import qualified Data.Text.Lazy.Encoding as LE
import qualified Data.Text.Lazy.IO as TL
import Data.Word ( Word32 )
import Language.Haskell.TH
import Language.Haskell.TH.Syntax ( qAddDependentFile )
import System.IO.Unsafe ( unsafePerformIO )
import qualified Text.PrettyPrint as PP

import Dismantle.Tablegen
import qualified Dismantle.Tablegen.ByteTrie as BT
import Dismantle.Tablegen.Instruction
import Dismantle.Tablegen.TH.Bits ( parseOperand )
import Dismantle.Tablegen.TH.Pretty ( prettyInstruction, PrettyOperand(..) )

genISA :: ISA -> Name -> FilePath -> DecsQ
genISA isa isaValName path = do
  desc <- runIO $ loadISA isa path
  case isaErrors desc of
    [] -> return ()
    errs -> reportWarning ("Unhandled instruction definitions for ISA: " ++ show (length errs))
  operandType <- mkOperandType desc
  opcodeType <- mkOpcodeType desc
  instrTypes <- mkInstructionAliases
  ppDef <- mkPrettyPrinter desc
  parserDef <- mkParser isaValName path
  return $ concat [ operandType
                  , opcodeType
                  , instrTypes
                  , ppDef
                  , parserDef
                  ]

-- | Load the instructions for the given ISA
loadISA :: ISA -> FilePath -> IO ISADescriptor
loadISA isa path = do
  txt <- TL.readFile path
  case parseTablegen path txt of
    Left err -> fail (show err)
    Right defs -> return $ filterISA isa defs

opcodeName :: Name
opcodeName = mkName "Opcode"

operandName :: Name
operandName = mkName "Operand"

mkParser :: Name -> FilePath -> Q [Dec]
mkParser isaValName path = do
  qAddDependentFile path
  (blen, dataLit) <- runIO $ do
    bs <- Z.compress <$> LBS.readFile path
    let len = LBS.length bs
    return (len, LitE $ StringPrimL $ LBS.unpack bs)
  dataExpr <- [| LE.decodeUtf8 $ Z.decompress $ LBS.fromStrict $ unsafePerformIO $ BS.unsafePackAddressLen blen $(return dataLit) |]
  trie <- [| case parseTablegen "<data>" $(return dataExpr) of
               Left err1 -> error ("Error while parsing embedded data: " ++ show err1)
               Right defs ->
                 let mkByteParser :: InstructionDescriptor -> Parser $(return (ConT (mkName "Instruction")))
                     mkByteParser i = undefined
                     parsable = parsableInstructions $(varE isaValName) (filterISA $(varE isaValName) defs)
                 in case BT.byteTrie Nothing (map (idMask &&& Just . mkByteParser) parsable) of
                   Left err2 -> error ("Error while building parse tables for embedded data: " ++ show err2)
                   Right tbl -> tbl
           |]
  parser <- [| parseInstruction $(return trie) |]
  return [ ValD (VarP (mkName "parseInstruction")) (NormalB parser) []
         ]

{-

Basically, for each InstructionDescriptor, we need to generate a
function that parses a bytestring

Goal (for lazy bytestrings):

with TH, make a function from InstructionDescriptor -> Parser (Instruction)

We can then use that function inside of something like
'makeParseTables' to generate a 'BT.ByteTrie (Maybe (Parser
Instruction)).  Then, we can just use the generic 'parseInstruction'.

There are only two steps for the TH, then:

1) convert from InstructionDescriptor to Parser

2) make an expression that is essentially a call to that + makeParseTables

-}

mkInstructionAliases :: Q [Dec]
mkInstructionAliases =
  return [ TySynD (mkName "Instruction") [] ity
         , TySynD (mkName "AnnotatedInstruction") [PlainTV annotVar] aty
         ]
  where
    annotVar = mkName "a"
    ity = ConT ''GenericInstruction `AppT` ConT opcodeName `AppT` ConT operandName
    aty = ConT ''GenericInstruction `AppT`
          ConT opcodeName `AppT`
          (ConT ''Annotated `AppT` VarT annotVar `AppT` ConT operandName)

mkOpcodeType :: ISADescriptor -> Q [Dec]
mkOpcodeType isa =
  return [ DataD [] opcodeName tyVars Nothing cons []
         , StandaloneDerivD [] (ConT ''Show `AppT` (ConT opcodeName `AppT` VarT opVarName `AppT` VarT shapeVarName))
         ]
  where
    opVarName = mkName "o"
    shapeVarName = mkName "sh"
    tyVars = [PlainTV opVarName, PlainTV shapeVarName]
    cons = map mkOpcodeCon (isaInstructions isa)

mkOpcodeCon :: InstructionDescriptor -> Con
mkOpcodeCon i = GadtC [n] [] ty
  where
    strName = toTypeName (idMnemonic i)
    n = mkName strName
    ty = ConT opcodeName `AppT` ConT operandName `AppT` opcodeShape i

opcodeShape :: InstructionDescriptor -> Type
opcodeShape i = foldr addField PromotedNilT (canonicalOperands i)
  where
    addField f t =
      case opType f of
        OperandType (toTypeName -> fname) -> PromotedConsT `AppT` LitT (StrTyLit fname) `AppT` t

-- | Generate a type to represent operands for this ISA
--
-- The type is always named @Operand@ and has a single type parameter
-- of kind 'Symbol'.
--
-- FIXME: We'll definitely need a mapping from string names to
-- suitable constructor names, as well as a description of the type
-- structure.
--
-- String -> (String, Q Type)
mkOperandType :: ISADescriptor -> Q [Dec]
mkOperandType isa =
  return [ DataD [] operandName [] (Just ksig) cons []
         , StandaloneDerivD [] (ConT ''Show `AppT` (ConT operandName `AppT` VarT (mkName "tp")))
         ]
  where
    ksig = ArrowT `AppT` ConT ''Symbol `AppT` StarT
    cons = map mkOperandCon (isaOperands isa)

mkOperandCon :: OperandType -> Con
mkOperandCon (OperandType (toTypeName -> name)) = GadtC [n] [argTy] ty
  where
    argTy = (Bang NoSourceUnpackedness NoSourceStrictness, ConT ''Word32)
    n = mkName name
    ty = ConT (mkName "Operand") `AppT` LitT (StrTyLit name)

toTypeName :: String -> String
toTypeName s =
  case s of
    [] -> error "Empty names are not allowed"
    c:rest -> toUpper c : rest

mkPrettyPrinter :: ISADescriptor -> Q [Dec]
mkPrettyPrinter desc = do
  iname <- newName "i"
  patterns <- mapM mkOpcodePrettyPrinter (isaInstructions desc)
  let ex = CaseE (VarE iname) patterns
      body = Clause [VarP iname] (NormalB ex) []
      pp = FunD ppName [body]
  return [sig, pp]
  where
    ppName = mkName "ppInstruction"
    ty = ArrowT `AppT` ConT (mkName "Instruction") `AppT` ConT ''PP.Doc
    sig = SigD ppName ty

-- | This returns the operands of an instruction in canonical order.
--
-- For now, it just concatenates them - in the future, it will
-- deduplicate (with a bias towards the output operands).
--
-- It may end up needing the ISA as input to deal with quirks
canonicalOperands :: InstructionDescriptor -> [OperandDescriptor]
canonicalOperands i = idOutputOperands i ++ idInputOperands i

mkOpcodePrettyPrinter :: InstructionDescriptor -> Q Match
mkOpcodePrettyPrinter i = do
  (opsPat, prettyOps) <- F.foldrM addOperand ((ConP 'Nil []), []) (canonicalOperands i)
  let pat = ConP 'Instruction [ConP (mkName (toTypeName (idMnemonic i))) [], opsPat]
      body = VarE 'prettyInstruction `AppE` LitE (StringL (idAsmString i)) `AppE` ListE prettyOps
  return $ Match pat (NormalB body) []
  where
    addOperand op (pat, pret) = do
      vname <- newName "operand"
      let oname = opName op
      prettyOp <- [| PrettyOperand oname $(return (VarE vname)) show |]
      return (InfixP (VarP vname) '(:>) pat, prettyOp : pret)

{-

For each ISA, we have to generate:

1) A datatype representing all possible operands (along with an
associated tag type, one tag for each operand type).  There may be
some sub-types (e.g., a separate Register type to be a parameter to a
Reg32 operand).

2) An ADT representing all possible instructions - this is a simple
GADT with no parameters, but the return types are lists of the types
of the operands for the instruction represented by the tag.

3) A type alias instantiating the underlying Instruction type with the
Tag and Operand types.

4) A pretty printer

5) A parser

6) An unparser

-}

{-
data Operand :: Symbol -> * where
  OImm32 :: Int -> Operand "Imm32"
  OReg32 :: Int -> Operand "Reg32"

deriving instance Show (Operand tp)

s2 :: OperandList Operand '["Imm32", "Reg32"]
s2 = OImm32 5 :> OReg32 0 :> Nil

s3 = case s2 of
  OImm32 _ :> l -> l

insn = Instruction Add s2

-- data ISATag o sh where
data ISATag :: (Symbol -> *) -> [Symbol] -> * where
  Add :: ISATag Operand '["Imm32", "Reg32"]
  Sub :: ISATag Operand '["Imm32", "Reg32"]

type Instruction = GenericInstruction ISATag Operand
type AnnotatedInstruction = GenericInstruction ISATag (Annotated () Operand)

foo :: Instruction -> Int
foo i =
  case i of
    Instruction Add (OImm32 imm :> OReg32 regNo :> Nil) -> imm + regNo
-}
