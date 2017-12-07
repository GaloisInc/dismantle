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
  genISA,
  genInstances,
  genISARandomHelpers
  ) where

import           GHC.TypeLits ( Symbol )

import           Control.Monad ( forM )
import           Data.Monoid ((<>))
import           Data.Bits
import qualified Data.ByteString as BS
import qualified Data.ByteString.Unsafe as UBS
import qualified Data.ByteString.Lazy as LBS
import qualified Data.Foldable as F
import           Data.List ( isSuffixOf )
import qualified Data.List.Split as L
import           Data.Maybe ( fromMaybe, catMaybes )
import qualified Data.Map.Strict as M
import qualified Data.Set as S
import qualified Data.Type.Equality as E
import           Data.Word ( Word8 )
import qualified Data.Text.Lazy.IO as TL
import           Language.Haskell.TH
import           Language.Haskell.TH.Datatype
import           Language.Haskell.TH.Syntax ( Lift(..), qAddDependentFile, Name(..) )
import           System.IO.Unsafe ( unsafePerformIO )
import           System.Directory ( doesFileExist, doesDirectoryExist, getDirectoryContents )
import           System.FilePath ( (</>) )
import qualified Text.PrettyPrint.HughesPJClass as PP

import           Data.Parameterized.Some ( Some(..) )
import           Data.Parameterized.Lift ( LiftF(..) )
import           Data.Parameterized.HasRepr ( HasRepr(..) )
import           Data.Parameterized.ShapedList ( ShapedList(..), ShapeRepr )
import qualified Data.Parameterized.TH.GADT as PTH
import           Data.EnumF ( EnumF(..), enumCompareF )
import qualified Data.Set.NonEmpty as NES
import           Data.Parameterized.Classes ( OrdF(..), ShowF(..), KnownRepr(..) )
import           Dismantle.Arbitrary as A
import           Dismantle.Instruction
import           Dismantle.Instruction.Random ( ArbitraryOperands(..), ArbitraryOperand(..), arbitraryShapedList )
import           Dismantle.Tablegen
import           Dismantle.Tablegen.Parser.Types
import qualified Dismantle.Tablegen.ByteTrie as BT
import           Dismantle.Tablegen.TH.Bits ( assembleBits, fieldFromWord )
import           Dismantle.Tablegen.TH.Pretty ( prettyInstruction, PrettyOperand(..) )

-- | Load an ISA from a base path and an optional collection of override
-- paths. The resulting descriptor will have been filtered by the
-- instruction filter of the provided ISA.
loadISA :: ISA
        -- ^ The ISA to use to filter the resulting instructions.
        -> FilePath
        -- ^ The path to the base ".tgen" file to parse.
        -> [FilePath]
        -- ^ A list of directories in which any ".tgen" files will be
        -- used to override entries in the base tablegen data.
        -> Q ISADescriptor
loadISA isa path overridePaths = do
  initialRecs <- runIO $ loadTablegen path
  overrides <- loadOverrides overridePaths
  return $ filterISA isa $ applyOverrides overrides initialRecs

genISA :: ISA -> FilePath -> [FilePath] -> DecsQ
genISA isa path overridePaths = do
  desc <- loadISA isa path overridePaths

  case isaErrors desc of
    [] -> return ()
    errs -> reportWarning ("Unhandled instruction definitions for ISA: " ++ show (length errs))

  operandType <- mkOperandType isa desc >>= sequence
  opcodeType <- mkOpcodeType desc >>= sequence
  instrTypes <- mkInstructionAliases
  ppDef <- mkPrettyPrinter desc
  parserDef <- mkParser isa desc path
  asmDef <- mkAssembler isa desc
  return $ concat [ operandType
                  , opcodeType
                  , instrTypes
                  , ppDef
                  , parserDef
                  , asmDef
                  ]

loadTablegen :: FilePath -> IO Records
loadTablegen path = do
  txt <- TL.readFile path
  case parseTablegen path txt of
    Left err -> fail err
    Right defs -> return defs

tgenOverrideExtension :: String
tgenOverrideExtension = ".tgen"

emptyRecords :: Records
emptyRecords = Records [] []

loadOverrides :: [FilePath] -> Q Records
loadOverrides [] = return emptyRecords
loadOverrides (path:rest) = do
    override <- loadOverridesFromDir path
    desc <- loadOverrides rest
    return $ applyOverrides override desc

loadOverridesFromDir :: FilePath -> Q Records
loadOverridesFromDir path = do
    dirEx <- runIO $ doesDirectoryExist path

    case dirEx of
        False -> return emptyRecords
        True -> do
            allEntries <- runIO $ getDirectoryContents path
            mFiles <- forM allEntries $ \e -> do
                ex <- runIO $ doesFileExist $ path </> e
                return $ if ex && tgenOverrideExtension `isSuffixOf` e
                         then Just $ path </> e
                         else Nothing

            let go [] = return emptyRecords
                go (f:rest) = do
                    overrides <- runIO $ loadTablegen f
                    qAddDependentFile f
                    desc <- go rest
                    return $ applyOverrides overrides desc

            go $ catMaybes mFiles

-- | Given two tablegen record collections A and B, return B with any
-- content overriden by content provided in A. Content provided by A but
-- not by B will be added to B. Defs and classes in B are replaced with
-- versions from A based on their mnemonics.
applyOverrides :: Records
               -- ^ The override record set (A).
               -> Records
               -- ^ The record set to be augmented/extended (B).
               -> Records
applyOverrides overrideRecs oldRecs = new
    where
        new = Records { tblDefs = overrideWith matchDef
                                               (tblDefs overrideRecs)
                                               (tblDefs oldRecs)
                      , tblClasses = overrideWith matchClass
                                                  (tblClasses overrideRecs)
                                                  (tblClasses oldRecs)
                      }

        matchDef a b = defName a == defName b
        matchClass a b = classDeclName a == classDeclName b

        overrideWith :: (a -> a -> Bool) -> [a] -> [a] -> [a]
        overrideWith matches overrides old =
            let unmatched = filter (\val -> not $ any (matches val) overrides) old
            in overrides <> unmatched

opcodeTypeName :: Name
opcodeTypeName = mkName "Opcode"

operandTypeName :: Name
operandTypeName = mkName "Operand"

mkParser :: ISA -> ISADescriptor -> FilePath -> Q [Dec]
mkParser isa desc path = do
  qAddDependentFile path
  -- Build up a table of AST fragments that are parser expressions.
  -- They are associated with the bit masks required to build the
  -- trie.  The trie is constructed at run time for now.
  parserData <- mapM (mkTrieInput isa) (parsableInstructions isa desc)
  let (trieInputs, decls) = unzip parserData
  case BT.byteTrie Nothing trieInputs of
    Left err -> reportError ("Error while building parse tables: " ++ show err) >> return []
    Right bt0 -> do
      let (parseTableBytes, parseTableSize, parseTableStartIndex) = BT.unsafeByteTrieParseTableBytes bt0
          payloads0 :: [Maybe Name]
          payloads0 = BT.unsafeByteTriePayloads bt0
          toParserExpr Nothing = [| Nothing |]
          toParserExpr (Just name) = [| Just $(varE name) |]
          parseTableExprPayloads :: [Q Exp]
          parseTableExprPayloads = map toParserExpr payloads0
      trie <- [|
                 let parseTableLit = $(litE (stringPrimL parseTableBytes))
                     payloads = $(listE parseTableExprPayloads)
                 in BT.unsafeFromAddr payloads parseTableLit $(lift parseTableSize) $(lift parseTableStartIndex)
               |]
      parser <- [| parseInstruction $(return trie) |]
      parserTy <- [t| LBS.ByteString -> (Int, Maybe $(conT (mkName "Instruction"))) |]
      return (decls ++ [ SigD parserName parserTy, ValD (VarP parserName) (NormalB parser) []])

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

-- | Note that the 'Maybe Name' is always a 'Just' value.
mkTrieInput :: ISA -> InstructionDescriptor -> Q ((String, BS.ByteString, BS.ByteString, Maybe Name), Dec)
mkTrieInput isa i = do
  pname <- newName ("insnParser" ++ mnemonic)
  let pexp = mkParserExpr isa i
  pdec <- valD (varP pname) (normalB pexp) []
  return ((mnemonic, BS.pack requiredMask, BS.pack trueMask, Just pname), pdec)
  where
    mnemonic = idMnemonic i
    (requiredMask, trueMask) = bitSpecAsBytes (idMask i)

-- | Return a TH expression that defines an instruction parser
--
-- The parser contains an expected bytecount and a function from bytestring to
-- instruction.
mkParserExpr :: ISA -> InstructionDescriptor -> Q Exp
mkParserExpr isa i
  | null (canonicalOperands i) = do
    -- If we have no operands, make a much simpler constructor (so we
    -- don't have an unused bytestring parameter)
    [| Parser reqBytes (\_ -> $(return con) $(return tag) Nil) |]
  | otherwise = do
    bsName <- newName "bytestring"
    wordName <- newName "w"
    opList <- F.foldrM (addOperandExpr wordName) (ConE 'Nil) (canonicalOperands i)
    let insnCon = con `AppE` tag `AppE` opList
    [| Parser reqBytes (\ $(varP bsName) ->
                 case $(varE (isaInsnWordFromBytes isa)) $(varE bsName) of
                   $(varP wordName) -> $(return insnCon))
     |]
  where
    reqBytes = length (idMask i) `div` 8
    tag = ConE (mkName (toTypeName (idMnemonic i)))
    con = ConE 'Instruction
    addOperandExpr wordName od e = do
      operandPayload <- lookupAndValidateOperand isa (opType od)
      let OperandType tyname = opType od
          otyname = toTypeName tyname
          operandCon = ConE (mkName otyname)
          -- FIXME: Need to write some helpers to handle making the
          -- right operand constructor
      case opConE operandPayload of
         Nothing -> [| $(return operandCon) (fieldFromWord $(varE wordName) $(lift (opChunks od))) :> $(return e) |]
         Just conExp -> [| $(return operandCon) ($(conExp) (fieldFromWord $(varE wordName) $(lift (opChunks od)))) :> $(return e) |]

unparserName :: Name
unparserName = mkName "assembleInstruction"

mkAssembler :: ISA -> ISADescriptor -> Q [Dec]
mkAssembler isa desc = do
  insnName <- newName "insn"
  unparserTy <- [t| $(conT (mkName "Instruction")) -> LBS.ByteString |]
  insTagTy <- [t| $(conT (mkName "Instruction")) -> Some $(conT (mkName "Opcode") `appT` (conT (mkName "Operand"))) |]
  pairsTy <- [t| [(Some $(conT (mkName "Opcode") `appT` (conT (mkName "Operand"))), $(conT (mkName "Instruction")) -> LBS.ByteString)] |]
  cases <- mapM (mkAsmCase isa) (isaInstructions desc)

  let (pairs, declLists) = unzip cases
      decls = concat declLists

  let pairsExprName = mkName "instructionAssemblyHandlers"
      insnTagName = mkName "getInstructionTag"
      mkTuple (opExpr, fun) =
          -- TupE [ConE (mkName "Some") `AppE` opExpr, VarE fun]
          [e| (Some $(return opExpr), $(return $ VarE fun)) |]

  pairsBody <- ListE <$> mapM mkTuple pairs
  tagExpr <- [e| Some $(varE $ mkName "t") |]

  -- maybe (error "") ($ i) lookup (getOpcode isnName) (Data.Map.fromList pairs)
  let body = VarE 'maybe `AppE` (VarE 'error `AppE` (LitE $ StringL "BUG: unhandled instruction in assembler"))
                         `AppE` (InfixE Nothing (VarE (mkName "$")) (Just $ VarE insnName))
                         `AppE` (VarE 'lookup `AppE` (VarE insnTagName `AppE` (VarE insnName))
                                              `AppE` (VarE pairsExprName))
      pairsExpr = [ SigD pairsExprName pairsTy
                  , ValD (VarP pairsExprName) (NormalB pairsBody) []
                  ]
      insTagExpr = [ SigD insnTagName insTagTy
                   , FunD insnTagName [Clause [ConP 'Instruction [VarP (mkName "t"), WildP]] (NormalB tagExpr) []]
                   ]


  return $ decls <>
           pairsExpr <>
           insTagExpr <>
           [ SigD unparserName unparserTy
           , FunD unparserName [Clause [VarP insnName] (NormalB body) []]
           ]

mkAsmCase :: ISA -> InstructionDescriptor -> Q ((Exp, Name), [Dec])
mkAsmCase isa i = do
  let fName = mkName ("assemble" ++ idMnemonic i)
  fTy <- [t| $(conT (mkName "Instruction")) -> LBS.ByteString |]

  -- We use the byte-swapped version of the mask here because the call
  -- to the 'isaInsnWordFromBytes' to convert the mask into a word
  -- will re-byte swap.
  let (_, trueMask) = bitSpecAsBytes (idMask i)
  trueMaskE <- [| $(varE (isaInsnWordFromBytes isa)) (LBS.fromStrict (unsafePerformIO (UBS.unsafePackAddressLen $(litE (integerL (fromIntegral (length trueMask)))) $(litE (stringPrimL trueMask))))) |]
  (opsPat, operands) <- F.foldrM addOperand ((ConP 'Nil []), []) (canonicalOperands i)
  let pat = ConP 'Instruction [ConP (mkName (toTypeName (idMnemonic i))) [], opsPat]
  body <- [| $(varE (isaInsnWordToBytes isa)) (assembleBits $(return trueMaskE) $(return (ListE operands))) |]

  let decls = [ SigD fName fTy
              , FunD fName [ Clause [pat] (NormalB body) []
                           , Clause [WildP] (NormalB $ VarE 'error `AppE` (LitE $ StringL "BUG: assembly function called with wrong instruction")) []
                           ]
              ]

  return ((ConE (mkName (toTypeName (idMnemonic i))), fName), decls)
  where
    addOperand op (pat, operands) = do
      operandPayload <- lookupAndValidateOperand isa (opType op)
      let OperandType tyname = opType op
          otyname = toTypeName tyname
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

mkOpcodeType :: ISADescriptor -> Q [DecQ]
mkOpcodeType isa = do
  return [ dataDCompat (cxt []) opcodeTypeName tyVars cons []
         , standaloneDerivD (cxt []) [t| Show ($(conT opcodeTypeName) $(varT opVarName) $(varT shapeVarName)) |]
         , standaloneDerivD (cxt []) [t| Eq ($(conT opcodeTypeName) $(varT opVarName) $(varT shapeVarName)) |]
         , standaloneDerivD (cxt []) [t| Ord ($(conT opcodeTypeName) $(varT opVarName) $(varT shapeVarName)) |]
         , standaloneDerivD (cxt []) [t| Lift ($(conT opcodeTypeName) $(varT opVarName) $(varT shapeVarName)) |]
         , instanceD (cxt []) [t| LiftF ($(conT opcodeTypeName) $(varT opVarName)) |] []
         , mkEnumFInstance isa
         , mkOpcodeShowFInstance
         , mkOpcodeOrdFInstance
         , mkTestEqualityInstance isa
         , mkHasReprInstance isa
         ]
  where
    opVarName = mkName "o"
    shapeVarName = mkName "sh"
    tyVars = [ KindedTV opVarName (ArrowT `AppT` ConT ''Symbol `AppT` StarT)
             , KindedTV shapeVarName (ListT `AppT` ConT ''Symbol)
             ]
    cons = map mkOpcodeCon (isaInstructions isa)

genISARandomHelpers :: ISA -> FilePath -> [FilePath] -> Q [Dec]
genISARandomHelpers isa path overridePaths = do
  desc <- loadISA isa path overridePaths
  genName <- newName "gen"
  opcodeName <- newName "opcode"
  let caseBody = caseE (varE opcodeName) (map (mkOpListCase genName) (isaInstructions desc))
  let f = funD 'arbitraryOperands [clause [varP genName, varP opcodeName] (normalB caseBody) []]
  arbOperandsInst <- instanceD (return []) [t| ArbitraryOperands $(conT opcodeTypeName) $(conT operandTypeName) |] [f]
  arbitraryInstances <- mapM mkArbitraryInstanceForOperand (isaOperands desc)
  arbOperandInst <- mkArbitraryOperandInstance desc
  return (arbOperandsInst : arbOperandInst : arbitraryInstances)
  where
    mkOpListCase genName i =
      let conName = mkName (toTypeName (idMnemonic i))
      in match (conP conName []) (normalB [| arbitraryShapedList $(varE genName) |]) []

    mkArbitraryInstanceForOperand (OperandType origOperandName) = do
      let symbol = toTypeName origOperandName
          name = mkName symbol
      genName <- newName "gen"
      let ty = [t| A.Arbitrary ($(conT operandTypeName) $(litT (strTyLit symbol))) |]
          body = [| $(conE name) <$> A.arbitrary $(varE genName) |]
          fun = funD 'A.arbitrary [clause [varP genName] (normalB body) []]
      instanceD (return []) ty [fun]

mkArbitraryOperandInstance :: ISADescriptor -> Q Dec
mkArbitraryOperandInstance desc = do
  genName <- newName "gen"
  operandName <- newName "operand"
  let caseBody = caseE (varE operandName) (map (mkOpListCase genName) (isaOperands desc))
  let f = funD 'arbitraryOperand [clause [varP genName, varP operandName] (normalB caseBody) []]
  instanceD (return []) [t| ArbitraryOperand $(conT operandTypeName) |] [f]
  where
    mkOpListCase genName (OperandType origOperandName) = do
      let symbol = toTypeName origOperandName
          name = mkName symbol
      match (recP name []) (normalB [| A.arbitrary $(varE genName) |]) []

mkEnumFInstance :: ISADescriptor -> Q Dec
mkEnumFInstance desc = do
  enumfTy <- [t| EnumF ($(conT opcodeTypeName) $(varT =<< newName "o")) |]
  enumfArgName <- newName "o"
  let enumfCase = caseE (varE enumfArgName) (zipWith mkEnumFMatch [0..] (isaInstructions desc))
  enumfDec <- funD 'enumF [clause [varP enumfArgName] (normalB enumfCase) []]

  let congruentfCase = caseE (varE enumfArgName) [ mkCongruentFCase elt eltsList
                                                 | (_shape, elts) <- M.toList congruenceClasses
                                                 , let eltsList = F.toList elts
                                                 , elt <- eltsList
                                                 ]
  congruentfDec <- funD 'congruentF [clause [varP enumfArgName] (normalB congruentfCase) []]
  return (InstanceD Nothing [] enumfTy [enumfDec, congruentfDec])
  where
    congruenceClasses :: M.Map [OperandType] (S.Set Name)
    congruenceClasses = F.foldl' classifyInstruction M.empty (isaInstructions desc)

    classifyInstruction m i =
      let conName = mkName (toTypeName (idMnemonic i))
      in M.insertWith S.union (instructionShape i) (S.singleton conName) m

    mkEnumFMatch i insn = do
      let conName = mkName (toTypeName (idMnemonic insn))
      match (conP conName []) (normalB (litE (integerL i))) []

    mkCongruentFCase eltName eltNames =
      match (conP eltName []) (normalB [| NES.fromList $(conE eltName) $(listE (map conE eltNames)) |]) []

instructionShape :: InstructionDescriptor -> [OperandType]
instructionShape i = [ opType op | op <- canonicalOperands i ]

-- | Create a 'E.TestEquality' instance for the opcode type
mkTestEqualityInstance :: ISADescriptor -> Q Dec
mkTestEqualityInstance desc = do
  testEqTy <- [t| E.TestEquality ($(conT opcodeTypeName) $(varT =<< newName "o")) |]
  let clauses = map mkTestEqualityCase (isaInstructions desc)
  let fallthrough = clause [wildP, wildP] (normalB [| Nothing |]) []
  dec <- funD 'E.testEquality (clauses ++ [fallthrough])
  return (InstanceD Nothing [] testEqTy [dec])
  where
    mkTestEqualityCase i = do
      let conName = mkName (toTypeName (idMnemonic i))
      clause [conP conName [], conP conName []] (normalB [| Just E.Refl |]) []

-- | Create an instance of 'OrdF' for the opcode type
mkOpcodeOrdFInstance :: Q Dec
mkOpcodeOrdFInstance = do
  [ordf] <- [d|
            instance OrdF ($(conT opcodeTypeName) $(varT =<< newName "o")) where
              compareF = enumCompareF
            |]
  return ordf

-- | Create an instance of 'ShowF' for the opcode type
mkOpcodeShowFInstance :: Q Dec
mkOpcodeShowFInstance = do
  [showf] <- [d|
             instance ShowF ($(conT opcodeTypeName) $(varT =<< newName "o")) where
               showF = show
             |]
  return showf

-- | Create an instance of 'KnownParameter ShapeRepr' for the opcode type
mkHasReprInstance :: ISADescriptor -> Q Dec
mkHasReprInstance desc = do
  operandTyVar <- newName "o"
  hasReprTy <- [t| HasRepr ($(conT opcodeTypeName) $(varT operandTyVar)) ShapeRepr |]
  let clauses = map mkHasReprCase (isaInstructions desc)
  dec <- funD 'typeRepr clauses
  return (InstanceD Nothing [] hasReprTy [dec])
  where
    mkHasReprCase i = do
      let conName = mkName (toTypeName (idMnemonic i))
      clause [conP conName []] (normalB [| knownRepr |]) []

mkOpcodeCon :: InstructionDescriptor -> Q Con
mkOpcodeCon i = return (GadtC [n] [] ty)
  where
    strName = toTypeName (idMnemonic i)
    n = mkName strName
    ty = ConT opcodeTypeName `AppT` VarT (mkName "o") `AppT` opcodeShape i

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
mkOperandType :: ISA -> ISADescriptor -> Q [DecQ]
mkOperandType isa desc = do
  -- Don't care about payloads here, just validation.
  mapM_ (lookupAndValidateOperand isa) (isaOperands desc)
  let cons = map mkOperandCon (isaOperandPayloadTypes isa)
  return [ dataDCompat (cxt []) operandTypeName [KindedTV (mkName "tp") (ConT ''Symbol)] cons []
         , standaloneDerivD (cxt []) [t| Show ($(conT operandTypeName) $(varT (mkName "tp"))) |]
         , mkOperandShowFInstance
         ]

lookupAndValidateOperand :: ISA -> OperandType -> Q OperandPayload
lookupAndValidateOperand isa (OperandType opUseName) =
  maybe err return $ lookup opTyName (isaOperandPayloadTypes isa)
  where
    opTyName = toTypeName opUseName
    err = error ("No operand descriptor payload for operand type: " <>
                 opUseName)

mkOperandCon :: (String, OperandPayload) -> Q Con
mkOperandCon (opTyName, payloadDesc) = do
  argBaseTy <- opTypeT payloadDesc
  let argTy = (Bang SourceUnpack SourceStrict, argBaseTy)
  return $ GadtC [n] [argTy] ty
  where
    n = mkName opTyName
    ty = ConT (mkName "Operand") `AppT` LitT (StrTyLit opTyName)

genInstances :: Q [Dec]
genInstances = do
  teq <- mkOperandTestEqInstance
  ordf <- mkOperandOrdFInstance
  return [teq, ordf]

mkOperandOrdFInstance :: Q Dec
mkOperandOrdFInstance = do
  [ordf] <- [d|
             instance OrdF $(conT operandTypeName) where
               compareF = $(PTH.structuralTypeOrd (conT operandTypeName) [])
              |]
  return ordf

mkOperandTestEqInstance :: Q Dec
mkOperandTestEqInstance = do
  [teq] <- [d|
            instance E.TestEquality $(conT operandTypeName) where
              testEquality = $(PTH.structuralTypeEquality (conT operandTypeName) [])
             |]
  return teq

-- | Create an instance of 'ShowF' for the operand type
mkOperandShowFInstance :: Q Dec
mkOperandShowFInstance = do
  [showf] <- [d|
             instance ShowF ($(conT operandTypeName)) where
               showF = show
             |]
  return showf

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
      body = VarE 'prettyInstruction `AppE` ListE defaults `AppE` LitE (StringL (idAsmString i)) `AppE` ListE prettyOps
      defaults = (mkDefault <$> idPrettyVariableOverrides i) <>
                 (mkDefault <$> idDefaultPrettyVariableValues i)
      mkDefault (varName, pretty) = TupE [LitE $ StringL varName, LitE $ StringL pretty]
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

