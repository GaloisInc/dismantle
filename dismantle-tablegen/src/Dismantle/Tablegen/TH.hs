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

import Control.Arrow ( (&&&) )
import qualified Data.ByteString.Lazy as LBS
import Data.Char ( toUpper )
import qualified Data.Foldable as F
import qualified Data.Map as M
import Data.Maybe ( fromMaybe )
import qualified Data.Text.Lazy.IO as TL
import Language.Haskell.TH
import Language.Haskell.TH.Syntax ( lift, qAddDependentFile )
import qualified Text.PrettyPrint.HughesPJClass as PP

import Dismantle.Tablegen
import qualified Dismantle.Tablegen.ByteTrie as BT
import Dismantle.Tablegen.Instruction
import Dismantle.Tablegen.TH.Bits ( assembleBits, fieldFromWord )
import Dismantle.Tablegen.TH.Pretty ( prettyInstruction, PrettyOperand(..) )

genISA :: ISA -> Name -> FilePath -> DecsQ
genISA isa isaValName path = do
  desc <- runIO $ loadISA isa path
  case isaErrors desc of
    [] -> return ()
    errs -> reportWarning ("Unhandled instruction definitions for ISA: " ++ show (length errs))
  operandType <- mkOperandType isa desc
  opcodeType <- mkOpcodeType desc
  instrTypes <- mkInstructionAliases
  ppDef <- mkPrettyPrinter desc
  parserDef <- mkParser isa desc isaValName path
  asmDef <- mkAssembler isa desc
  return $ concat [ operandType
                  , opcodeType
                  , instrTypes
                  , ppDef
                  , parserDef
                  , asmDef
                  ]

-- | Load the instructions for the given ISA
loadISA :: ISA -> FilePath -> IO ISADescriptor
loadISA isa path = do
  txt <- TL.readFile path
  case parseTablegen path txt of
    Left err -> fail (show err)
    Right defs -> return $ filterISA isa defs

opcodeTypeName :: Name
opcodeTypeName = mkName "Opcode"

operandTypeName :: Name
operandTypeName = mkName "Operand"

mkParser :: ISA -> ISADescriptor -> Name -> FilePath -> Q [Dec]
mkParser isa desc isaValName path = do
  qAddDependentFile path
  -- Build up all of the AST fragments of the parsers into a [(String,
  -- ExprQ)] outside of TH.  In the quasi quote, turn that into a
  -- value-level Map, and have mkByteParser just be a lookup in that
  -- map.
  parseExprs <- mapM (mkParserExpr isa) (parsableInstructions isa desc)
  mapExpr <- [| M.fromList $(listE (map (\(s, e) -> tupE [return s, return e]) parseExprs)) |]
  trie <- [|
            let mkByteParser :: InstructionDescriptor -> Parser $(conT (mkName "Instruction"))
                mkByteParser i =
                  let err = error ("No parser defined for instruction: " ++ idMnemonic i)
                      parserMap = $(return mapExpr)
                  in case M.lookup (idMnemonic i) parserMap of
                    Nothing -> err
                    Just p -> p
                parsable = parsableInstructions $(varE isaValName) desc
            in case BT.byteTrie Nothing (map (idMask &&& Just . mkByteParser) parsable) of
              Left err2 -> error ("Error while building parse tables for embedded data: " ++ show err2)
              Right tbl -> tbl
           |]
  parser <- [| parseInstruction $(return trie) |]
  parserTy <- [t| LBS.ByteString -> (Int, Maybe $(conT (mkName "Instruction"))) |]
  return [ SigD parserName parserTy
         , ValD (VarP parserName) (NormalB parser) []
         ]

parserName :: Name
parserName = mkName "disassembleInstruction"

mkParserExpr :: ISA -> InstructionDescriptor -> Q (Exp, Exp)
mkParserExpr isa i
  | null (canonicalOperands i) = do
    -- If we have no operands, make a much simpler constructor (so we
    -- don't have an unused bytestring parameter)
    e <- [| Parser $ \_ -> $(return con) $(return tag) Nil |]
    return (LitE (StringL (idMnemonic i)), e)
  | otherwise = do
    bsName <- newName "bytestring"
    wordName <- newName "w"
    opList <- F.foldrM (addOperandExpr wordName) (ConE 'Nil) (canonicalOperands i)
    let insnCon = con `AppE` tag `AppE` opList
    e <- [| Parser $ \ $(varP bsName) ->
             case $(varE (isaInsnWordFromBytes isa)) $(varE bsName) of
               $(varP wordName) -> $(return insnCon)
          |]
    return (LitE (StringL (idMnemonic i)), e)
  where
    tag = ConE (mkName (toTypeName (idMnemonic i)))
    con = ConE 'Instruction
    addOperandExpr wordName od e =
      let OperandType tyname = opType od
          otyname = toTypeName tyname
          err = error ("No operand descriptor payload for operand type: " ++ otyname)
          operandPayload = fromMaybe err $ lookup otyname (isaOperandPayloadTypes isa)
          operandCon = ConE (mkName otyname)
          intE = litE . integerL . fromIntegral
          startBit = opStartBit od
          numBits = opNumBits od
          -- FIXME: Need to write some helpers to handle making the
          -- right operand constructor
      in case opConE operandPayload of
         Nothing -> [| $(return operandCon) (fieldFromWord $(varE wordName) $(intE startBit) $(intE numBits)) :> $(return e) |]
         Just conExp -> [| $(return operandCon) ($(conExp) (fieldFromWord $(varE wordName) $(intE startBit) $(intE numBits))) :> $(return e) |]

unparserName :: Name
unparserName = mkName "assembleInstruction"

mkAssembler :: ISA -> ISADescriptor -> Q [Dec]
mkAssembler isa desc = do
  insnName <- newName "insn"
  unparserTy <- [t| $(conT (mkName "Instruction")) -> LBS.ByteString |]
  cases <- mapM (mkAsmCase isa) (isaInstructions desc)
  let body = CaseE (VarE insnName) cases
  return [ SigD unparserName unparserTy
         , FunD unparserName [Clause [VarP insnName] (NormalB body) []]
         ]

mkAsmCase :: ISA -> InstructionDescriptor -> Q Match
mkAsmCase isa i = do
  lbits <- lift (idMaskRaw i)
  (opsPat, operands) <- F.foldrM addOperand ((ConP 'Nil []), []) (canonicalOperands i)
  let pat = ConP 'Instruction [ConP (mkName (toTypeName (idMnemonic i))) [], opsPat]
  body <- [| $(varE (isaInsnWordToBytes isa)) (assembleBits $(return lbits) $(return (ListE operands))) |]
  return $ Match pat (NormalB body) []
  where
    addOperand op (pat, operands) = do
      let OperandType tyname = opType op
          otyname = toTypeName tyname
          err = error ("No operand descriptor payload for operand type: " ++ otyname)
          operandPayload = fromMaybe err $ lookup otyname (isaOperandPayloadTypes isa)
          opToBits = fromMaybe [| id |] (opWordE operandPayload)
--      obits <- lift (opBits op)
      startBit <- lift (opStartBit op)
      vname <- newName "operand"
--      asmOp <- [| OperandWrapper $(varE vname) $(return obits) |]
      asmOp <- [| ( $(opToBits) $(varE vname),  $(return startBit) ) |]
      return (InfixP (ConP (mkName otyname) [VarP vname]) '(:>) pat, asmOp : operands)

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
    ity = ConT ''GenericInstruction `AppT` ConT opcodeTypeName `AppT` ConT operandTypeName
    aty = ConT ''GenericInstruction `AppT`
          ConT opcodeTypeName `AppT`
          (ConT ''Annotated `AppT` VarT annotVar `AppT` ConT operandTypeName)

mkOpcodeType :: ISADescriptor -> Q [Dec]
mkOpcodeType isa =
  return [ DataD [] opcodeTypeName tyVars Nothing cons []
         , StandaloneDerivD [] (ConT ''Show `AppT` (ConT opcodeTypeName `AppT` VarT opVarName `AppT` VarT shapeVarName))
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
    ty = ConT opcodeTypeName `AppT` ConT operandTypeName `AppT` opcodeShape i

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
mkOperandType :: ISA -> ISADescriptor -> Q [Dec]
mkOperandType isa desc =
  return [ DataD [] operandTypeName [] (Just ksig) cons []
         , StandaloneDerivD [] (ConT ''Show `AppT` (ConT operandTypeName `AppT` VarT (mkName "tp")))
         ]
  where
    ksig = ArrowT `AppT` ConT ''Symbol `AppT` StarT
    cons = map (mkOperandCon isa) (isaOperands desc)

mkOperandCon :: ISA -> OperandType -> Con
mkOperandCon isa (OperandType (toTypeName -> name)) = GadtC [n] [argTy] ty
  where
    Just payloadDesc = lookup name (isaOperandPayloadTypes isa)
    argTy = (Bang SourceUnpack SourceStrict, ConT (opTypeName payloadDesc))
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
          OperandType otyname = opType op
      prettyOp <- [| PrettyOperand oname $(return (VarE vname)) PP.pPrint |]
      return (InfixP (ConP (mkName (toTypeName otyname)) [(VarP vname)]) '(:>) pat, prettyOp : pret)

{-

For each ISA, we have to generate:

1) A datatype representing all possible operands (along with an
associated tag type, one tag for each operand type).  There may be
some sub-types (e.g., a separate Register type to be a parameter to a
Reg32 operand).

2) An ADT representing all possible instructions - this is a simple
GADT with no parameters, but the return types are lists of the types
of the operands for the instruction represented by the tag. [done]

3) A type alias instantiating the underlying Instruction type with the
Tag and Operand types. [done]

4) A pretty printer [done]

5) A parser

6) An unparser

-}

