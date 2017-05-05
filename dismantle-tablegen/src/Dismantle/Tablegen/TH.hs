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

import Data.Bits
import qualified Data.ByteString.Unsafe as UBS
import qualified Data.ByteString.Lazy as LBS
import Data.Char ( toUpper )
import qualified Data.Foldable as F
import qualified Data.List.Split as L
import Data.Maybe ( fromMaybe )
import Data.Word ( Word8 )
import qualified Data.Text.Lazy.IO as TL
import Language.Haskell.TH
import Language.Haskell.TH.Syntax ( lift, qAddDependentFile )
import System.IO.Unsafe ( unsafePerformIO )
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
  -- Build up a table of AST fragments that are parser expressions.
  -- They are associated with the bit masks required to build the
  -- trie.  The trie is constructed at run time for now.
  parseExprs <- mapM (mkParserExpr isa) (parsableInstructions isa desc)
  trie <- [|
           let parserData = $(return (ListE parseExprs))
           in case BT.byteTrie Nothing parserData of
             Left err -> error ("Error while building parse tables for embedded data: " ++ show err)
             Right tbl -> tbl

           |]
  parser <- [| parseInstruction $(return trie) |]
  parserTy <- [t| LBS.ByteString -> (Int, Maybe $(conT (mkName "Instruction"))) |]
  return [ SigD parserName parserTy
         , ValD (VarP parserName) (NormalB parser) []
         ]

parserName :: Name
parserName = mkName "disassembleInstruction"

-- | Convert a required bit specification into two masks
--
-- 1) The mask of required bits (both 1 and 0)
--
-- 2) The mask of bits required to be 1
--
-- The [Word8] forms are suitable for constructing Addr# literals,
-- which we can turn into bytestrings efficiently (i.e., without
-- parsing)
bitSpecAsBytes :: [BT.Bit] -> ([Word8], [Word8])
bitSpecAsBytes bits = (map setRequiredBits byteGroups, map setTrueBits byteGroups)
  where
    byteGroups = L.chunksOf 8 bits
    setRequiredBits byteBits = foldr setRequiredBit 0 (zip [7,6..0] byteBits)
    setTrueBits byteBits = foldr setTrueBit 0 (zip [7,6..0] byteBits)
    setRequiredBit (ix, b) w =
      case b of
        BT.ExpectedBit _ -> w `setBit` ix
        BT.Any -> w
    setTrueBit (ix, b) w =
      case b of
        BT.ExpectedBit True -> w `setBit` ix
        _ -> w

-- | For a parsable instruction, return three expressions ready to be
-- spliced into the AST:
--
-- 1) The instruction mnemonic
--
-- 2) The mask (as a bytestring) of required bits
--
-- 3) The bits required to be 1 among the required bits
--
-- 4) The actual parser function wrapped in a 'Parser' constructor to
-- existentially quantify out type parameters and shapes and whatnot.
-- The parser is actually wrapped in a Just to satisfy the trie
-- constructor.
--
-- The masks are Addr# literals wrapped in an 'unsafePackAddresLen' to
-- create bytestrings cheaply.
--
-- Note that we use the endian-corrected bit specs here
mkParserExpr :: ISA -> InstructionDescriptor -> Q Exp
mkParserExpr isa i
  | null (canonicalOperands i) = do
    -- If we have no operands, make a much simpler constructor (so we
    -- don't have an unused bytestring parameter)
    e <- [| Just (Parser (\_ -> $(return con) $(return tag) Nil)) |]
    (requiredMaskE, trueMaskE) <- mkMaskByteStringExprs requiredMask trueMask
    return $ TupE [nameE, requiredMaskE, trueMaskE, e]
  | otherwise = do
    bsName <- newName "bytestring"
    wordName <- newName "w"
    opList <- F.foldrM (addOperandExpr wordName) (ConE 'Nil) (canonicalOperands i)
    let insnCon = con `AppE` tag `AppE` opList
    e <- [| Just (Parser (\ $(varP bsName) ->
             case $(varE (isaInsnWordFromBytes isa)) $(varE bsName) of
               $(varP wordName) -> $(return insnCon)))
          |]
    (requiredMaskE, trueMaskE) <- mkMaskByteStringExprs requiredMask trueMask
    return $ TupE [nameE, requiredMaskE, trueMaskE, e]
  where
    (requiredMask, trueMask) = bitSpecAsBytes (idMask i)
    nameE = LitE $ stringL $ idMnemonic i
    tag = ConE (mkName (toTypeName (idMnemonic i)))
    con = ConE 'Instruction
    addOperandExpr wordName od e =
      let OperandType tyname = opType od
          otyname = toTypeName tyname
          err = error ("No operand descriptor payload for operand type: " ++ otyname)
          operandPayload = fromMaybe err $ lookup otyname (isaOperandPayloadTypes isa)
          operandCon = ConE (mkName otyname)
          -- FIXME: Need to write some helpers to handle making the
          -- right operand constructor
      in case opConE operandPayload of
         Nothing -> [| $(return operandCon) (fieldFromWord $(varE wordName) $(lift (opChunks od))) :> $(return e) |]
         Just conExp -> [| $(return operandCon) ($(conExp) (fieldFromWord $(varE wordName) $(lift (opChunks od)))) :> $(return e) |]

-- | Convert the bytestrings representing the required bits and true bits masks into TH expressions.
--
-- The TH expressions are of type 'ByteString'
mkMaskByteStringExprs :: [Word8] -> [Word8] -> Q (Exp, Exp)
mkMaskByteStringExprs requiredMask trueMask = do
  requiredMaskE <- [| unsafePerformIO (UBS.unsafePackAddressLen $(litE (integerL (fromIntegral (length requiredMask)))) $(litE (stringPrimL requiredMask))) |]
  trueMaskE <- [| unsafePerformIO (UBS.unsafePackAddressLen $(litE (integerL (fromIntegral (length trueMask)))) $(litE (stringPrimL trueMask))) |]
  return (requiredMaskE, trueMaskE)

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
  -- We use the byte-swapped version of the mask here because the call
  -- to the 'isaInsnWordFromBytes' to convert the mask into a word
  -- will re-byte swap.
  let (_, trueMask) = bitSpecAsBytes (idMask i)
  trueMaskE <- [| $(varE (isaInsnWordFromBytes isa)) (LBS.fromStrict (unsafePerformIO (UBS.unsafePackAddressLen $(litE (integerL (fromIntegral (length trueMask)))) $(litE (stringPrimL trueMask))))) |]
  (opsPat, operands) <- F.foldrM addOperand ((ConP 'Nil []), []) (canonicalOperands i)
  let pat = ConP 'Instruction [ConP (mkName (toTypeName (idMnemonic i))) [], opsPat]
  body <- [| $(varE (isaInsnWordToBytes isa)) (assembleBits $(return trueMaskE) $(return (ListE operands))) |]
  return $ Match pat (NormalB body) []
  where
    addOperand op (pat, operands) = do
      let OperandType tyname = opType op
          otyname = toTypeName tyname
          err = error ("No operand descriptor payload for operand type: " ++ otyname)
          operandPayload = fromMaybe err $ lookup otyname (isaOperandPayloadTypes isa)
          opToBits = fromMaybe [| id |] (opWordE operandPayload)
      chunks <- lift (opChunks op)
      vname <- newName "operand"
      asmOp <- [| ( $(opToBits) $(varE vname),  $(return chunks) ) |]
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

