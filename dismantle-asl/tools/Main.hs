{-# LANGUAGE GADTs #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NamedFieldPuns #-}

module Main ( main ) where

import qualified Control.Exception as X
import           Data.Monoid
import           Control.Monad.Identity
import           Control.Monad (forM_, when)
import qualified Control.Monad.State.Lazy as MSS
import qualified Control.Monad.Except as E
import           Control.Monad.IO.Class
import           Data.Map ( Map )
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import           Data.Maybe ( catMaybes, mapMaybe )
import           Data.Parameterized.Nonce
-- import qualified Data.Parameterized.Ctx as Ctx
import qualified Data.Parameterized.Context as Ctx
import           Data.Bits( (.|.) )
import           Data.Parameterized.Some ( Some(..) )
import qualified Data.Text as T
import qualified Data.Text.IO as T

import qualified Data.List as List
import qualified Data.List.Split as List
import           Data.List.Index (imap)
import qualified Lang.Crucible.Backend.Online as CBO
import qualified Lang.Crucible.FunctionHandle as CFH
import qualified Language.ASL.Parser as AP
import qualified Language.ASL.Syntax as AS
import System.Exit (exitFailure)

import qualified System.Environment as IO
import qualified System.IO as IO
import           System.IO (hPutStrLn, stderr)
import           System.Console.GetOpt
import           Panic hiding (panic)
import           Lang.Crucible.Panic ( Crucible )

import qualified Dismantle.ASL.SyntaxTraverse as AS  ( pattern VarName )
import qualified Dismantle.ASL.SyntaxTraverse as TR

import qualified Dismantle.ASL as ASL

import qualified Dismantle.ASL.Crucible as AC

import Dismantle.ASL.Translation
import Dismantle.ASL.Translation.Preprocess
import Dismantle.ASL.Signature
import Dismantle.ASL.StaticExpr
import Dismantle.ASL.Exceptions

import qualified What4.Expr.Builder as B
import qualified What4.Interface as WI
import qualified What4.Solver.Yices as Yices
import qualified What4.Config as WC
import           What4.ProblemFeatures

import qualified What4.Serialize.Printer as WP
import qualified What4.Serialize.Parser as WP
import qualified What4.Serialize.Normalize as WN
import qualified What4.Utils.Log as U
import qualified What4.Utils.Util as U

import qualified Text.PrettyPrint.HughesPJClass as PP
import qualified Text.PrettyPrint.ANSI.Leijen as LPP

data TranslatorOptions = TranslatorOptions
  { optVerbosity :: Integer
  , optStartIndex :: Int
  , optNumberOfInstructions :: Maybe Int
  , optFilters :: Filters
  , optCollectAllExceptions :: Bool
  , optCollectExpectedExceptions :: Bool
  , optASLSpecFilePath :: FilePath
  , optTranslationDepth :: TranslationDepth
  , optCheckSerialization :: Bool
  , optFormulaOutputFilePath :: FilePath
  }


data TranslationDepth = TranslateRecursive
                      | TranslateShallow

instsFilePath :: FilePath
instsFilePath = "arm_instrs.sexpr"

defsFilePath :: FilePath
defsFilePath = "arm_defs.sexpr"

regsFilePath :: FilePath
regsFilePath = "arm_regs.sexpr"

supportFilePath :: FilePath
supportFilePath = "support.sexpr"

extraDefsFilePath :: FilePath
extraDefsFilePath = "extra_defs.sexpr"

defaultOptions :: TranslatorOptions
defaultOptions = TranslatorOptions
  { optVerbosity = 1
  , optStartIndex = 0
  , optNumberOfInstructions = Nothing
  , optFilters = translateArch32 noFilter
  , optCollectAllExceptions = False
  , optCollectExpectedExceptions = True
  , optASLSpecFilePath = "./data/Parsed/"
  , optTranslationDepth = TranslateRecursive
  , optCheckSerialization = False
  , optFormulaOutputFilePath = "./output/formulas.what4"
  }

data StatOptions = StatOptions
  { reportKnownExceptions :: Bool
  , reportSucceedingInstructions :: Bool
  , reportAllExceptions :: Bool
  , reportKnownExceptionFilter :: ExpectedException -> Bool
  , reportFunctionDependencies :: Bool
  }

defaultStatOptions :: StatOptions
defaultStatOptions = StatOptions
  { reportKnownExceptions = False
  , reportSucceedingInstructions = False
  , reportAllExceptions = False
  , reportKnownExceptionFilter = (\_ -> True)
  , reportFunctionDependencies = False
  }

arguments :: [OptDescr (Either (TranslatorOptions -> Maybe TranslatorOptions) (StatOptions -> Maybe StatOptions))]
arguments =
  [ Option "a" ["asl-spec"] (ReqArg (\f -> Left (\opts -> Just $ opts { optASLSpecFilePath = f })) "PATH")
    ("Path to parsed ASL specification. Requires: " ++ instsFilePath ++ " " ++ defsFilePath
      ++ " " ++ regsFilePath ++ " " ++ supportFilePath ++ " " ++ extraDefsFilePath)

  , Option "c" ["collect-exceptions"] (NoArg (Left (\opts -> Just $ opts { optCollectAllExceptions = True })))
    "Handle and collect all exceptions thrown during translation"

  , Option [] ["collect-expected-exceptions"] (NoArg (Left (\opts -> Just $ opts { optCollectExpectedExceptions = True })))
    "Handle and collect exceptions for known issues thrown during translation"

  , Option "v" ["verbosity"] (ReqArg (\f -> (Left (\opts -> Just $ opts { optVerbosity = read f }))) "INT")
    ("Output verbosity during translation:\n" ++
    "0 - minimal output.\n" ++
    "-1 - no translation output.\n" ++
    "1 (default) - translator/simulator log.\n" ++
    "2 - translator trace (on exception).\n" ++
    "3 - instruction post-processing trace (on exception).\n4 - globals collection trace.\n" ++
    "6 - translator and globals collection trace (always).")

  , Option [] ["offset"] (ReqArg (\f -> Left (\opts -> Just $ opts {optStartIndex = read f})) "INT")
    "Start processing instructions at the given offset"

  , Option [] ["report-success"] (NoArg (Right (\opts -> Just $ opts { reportSucceedingInstructions = True })))
    "Print list of successfully translated instructions"

  , Option [] ["report-deps"] (NoArg (Right (\opts -> Just $ opts { reportFunctionDependencies = True })))
    "Print ancestors of functions when reporting exceptions"

  , Option [] ["report-exceptions"] (NoArg (Right (\opts -> Just $ opts { reportAllExceptions = True })))
    "Print all collected exceptions thrown during translation (requires collect-exceptions or collect-expected-exceptions)"

  , Option "o" ["output-formulas"] (ReqArg (\f -> Left (\opts -> Just $ opts { optFormulaOutputFilePath = f })) "PATH")

   "Path to serialized formulas."
  , Option [] ["report-expected-exceptions"] (NoArg (Right (\opts -> Just $ opts {reportKnownExceptions = True })))
    "Print collected exceptions for known issues thrown during translation (requires collect-exceptions or collect-expected-exceptions)"

  , Option [] ["translation-mode"] (ReqArg (\mode -> Left (\opts -> do
      task <- case mode of
        "all" -> return $ translateAll
        "noArch64" -> return $ translateNoArch64
        "Arch32" -> return $ translateArch32
        _ -> case List.splitOn "/" mode of
          [instr, enc] -> return $ translateOnlyInstr (T.pack instr, T.pack enc)
          _ -> fail ""
      return $ opts { optFilters = task (optFilters opts) })) "TRANSLATION_MODE")
    ("Filter instructions according to TRANSLATION_MODE: \n" ++
     "all - translate all instructions from " ++ instsFilePath ++ ".\n" ++
     "noArch64 - translate T16, T32 and A32 instructions.\n" ++
     "Arch32 (default) - translate T32 and A32 instructions.\n" ++
     "<INSTRUCTION>/<ENCODING> - translate a single instruction/encoding pair.")

  , Option [] ["simulation-mode"] (ReqArg (\mode -> Left (\opts -> do
      task <- case mode of
        "all (default)" -> return $ simulateAll
        "instructions" -> return $ simulateInstructions
        "none" -> return $ simulateNone
        _ -> fail ""
      return $ opts { optFilters = task (optFilters opts) })) "SIMULATION_MODE")
    ("Filter instructions and functions for symbolic simulation according to SIMULATION_MODE: \n" ++
     "all (default) - simulate all successfully translated instructions and functions. \n" ++
     "instructions - simulate only instructions. \n" ++
     "none - do not perform any symbolic execution.")

  , Option [] ["no-dependencies"] (NoArg (Left (\opts -> Just $ opts { optTranslationDepth = TranslateShallow } )))
    "Don't recursively translate function dependencies."

  , Option [] ["check-serialization"] (NoArg (Left (\opts -> Just $ opts { optCheckSerialization = True } )))
    "Check that serialization/deserialization for any processed formulas is correct."
  ]

usage :: IO ()
usage = do
  pn <- IO.getProgName
  let msg = "Usage: " <> pn <> " [options]"
  putStrLn $ usageInfo msg arguments

data BuilderData t = NoBuilderData

main :: IO ()
main = do
  stringArgs <- IO.getArgs
  let (args, rest, errs) = getOpt Permute arguments stringArgs

  when (not $ null $ errs <> rest) $ do
    usage
    exitFailure

  case foldl applyOption (Just (defaultOptions, defaultStatOptions)) args of
    Nothing -> do
      usage
      exitFailure
    Just (opts, statOpts) -> do
      SomeSigMap sm <- runWithFilters opts
      reportStats statOpts sm
      logMsgIO opts 1 $ T.pack $ "Writing formulas to: " ++ show (optFormulaOutputFilePath opts)
      T.writeFile (optFormulaOutputFilePath opts) (WP.printSymFnEnv (reverse (sFormulas sm)))

      when (optCheckSerialization opts) $ do
        Some r <- liftIO $ newIONonceGenerator
        sym <- liftIO $ B.newExprBuilder B.FloatRealRepr NoBuilderData r
        lcfg <- U.mkLogCfg "check serialization"
        U.withLogCfg lcfg $
          WP.readSymFnEnvFromFile (WP.defaultParserConfig sym) (optFormulaOutputFilePath opts) >>= \case
            Left err -> X.throw $ SimulationDeserializationFailure err ""
            Right _ -> do
              logMsgIO opts 1 $ T.pack "Deserialization successful."
              return ()
  where
    applyOption (Just (opts, statOpts)) arg = case arg of
      Left f -> do
        opts' <- f opts
        return $ (opts', statOpts)
      Right f -> do
        statOpts' <- f statOpts
        return $ (opts, statOpts')
    applyOption Nothing _ = Nothing

runWithFilters :: TranslatorOptions -> IO (SomeSigMap)
runWithFilters opts = do
  spec <- getASL opts
  logMsgIO opts 0 $ T.pack $ "Loaded "
    ++ show (length $ aslInstructions spec) ++ " instructions and "
    ++ show (length $ aslDefinitions spec) ++ " definitions and "
    ++ show (length $ aslSupportDefinitions spec) ++ " support definitions and "
    ++ show (length $ aslExtraDefinitions spec) ++ " extra definitions and "
    ++ show (length $ aslRegisterDefinitions spec) ++ " register definitions."
  let (sigEnv, sigState) = buildSigState spec
  runWithFilters' opts spec sigEnv sigState

collectInstructions :: [AS.Instruction] -> [(AS.Instruction, T.Text, AS.InstructionSet)]
collectInstructions aslInsts = do
  List.concat $
    map (\instr -> map (\(AS.InstructionEncoding {AS.encName=encName, AS.encInstrSet=iset}) -> (instr, encName, iset)) (AS.instEncodings instr)) aslInsts

runWithFilters' :: TranslatorOptions
                -> ASLSpec
                -> SigEnv
                -> SigState
                -> IO (SomeSigMap)
runWithFilters' opts spec sigEnv sigState = do
  let startidx = optStartIndex opts
  let numInstrs = optNumberOfInstructions opts
  let getInstr (instr, enc, iset) = do
        let test = instrFilter $ optFilters $ opts
        let ident = instrToIdent instr enc iset
        if test ident then Just (ident, instr) else Nothing
  let allInstrs = imap (\i -> \nm -> (i,nm)) $ mapMaybe getInstr (collectInstructions (aslInstructions spec))
  let instrs = case numInstrs of {Just i -> take i (drop startidx allInstrs); _ -> drop startidx allInstrs}
  execSigMapWithScope opts sigState sigEnv $ do
    addMemoryUFs
    forM_ instrs $ \(i, (ident, instr)) -> do
      logMsg 1 $ T.pack $ "Processing instruction: " ++ show i ++ "/" ++ show (length allInstrs)
      runTranslation instr ident



runTranslation :: AS.Instruction
               -> InstructionIdent
               -> SigMapM sym arch ()
runTranslation instruction@AS.Instruction{..} instrIdent = do
  logMsg 1 $ "Computing instruction signature for: " <> T.pack (show instrIdent)
  result <- liftSigM (KeyInstr instrIdent) $
    computeInstructionSignature instruction (iEnc instrIdent) (iSet instrIdent)
  case result of
    Left err -> do
      logMsg 0 $ "Error computing instruction signature: " <> T.pack (show err)
    Right (Some (SomeFunctionSignature iSig), instStmts) -> do
      liftSigM (KeyInstr instrIdent) getDefinitions >>= \case
        Left err -> do
          logMsg 0 $ "Error computing ASL definitions: " <> T.pack (show err)
        Right defs -> do
          logMsg 1 $ "Translating instruction: " <> T.pack (prettyIdent instrIdent)
          logMsg 1 $ T.pack $ (show iSig)
          mfunc <- translateFunction (KeyInstr instrIdent) iSig instStmts defs
          let deps = maybe Set.empty AC.funcDepends mfunc
          MSS.gets (optTranslationDepth . sOptions) >>= \case
            TranslateRecursive -> do
              logMsg 1 $ "--------------------------------"
              logMsg 1 $ "Translating functions: "
              alldeps <- mapM (translationLoop instrIdent [] defs) (Set.toList deps)
              let alldepsSet = Set.union (Set.unions alldeps) (finalDepsOf deps)
              MSS.modify' $ \s -> s { instrDeps = Map.insert instrIdent alldepsSet (instrDeps s) }
            TranslateShallow -> return ()
          maybeSimulateFunction instrIdent (KeyInstr instrIdent) mfunc

translationLoop :: InstructionIdent
                -> [T.Text]
                -> Definitions arch
                -> (T.Text, StaticValues)
                -> SigMapM sym arch (Set.Set T.Text)
translationLoop fromInstr callStack defs (fnname, env) = do
  let finalName = (mkFinalFunctionName env fnname)
  fdeps <- MSS.gets funDeps
  case Map.lookup finalName fdeps of
    Just deps -> return deps
    _ -> do
      filteredOut <- isFunFilteredOut fromInstr finalName
      if filteredOut
      then return Set.empty
      else do
        result <- liftSigM (KeyFun finalName) $ do
          case Map.lookup fnname (defSignatures defs) of
             Just (ssig, stmts) -> do
               sig <- mkSignature env ssig
               return (sig, stmts)
             Nothing -> E.throwError $ MissingSigFunctionDefinition finalName
        case result of
          Left _ -> do
            return Set.empty
          Right (Some ssig@(SomeFunctionSignature sig), stmts) -> do
            MSS.modify' $ \s -> s { sMap = Map.insert finalName (Some ssig) (sMap s) }
            logMsg 1 $ T.pack $ "Translating function: " ++ show finalName ++ " for instruction: "
               ++ prettyIdent fromInstr
               ++ "\n CallStack: " ++ show callStack
               ++ "\n" ++ show sig ++ "\n"
            mfunc <- translateFunction (KeyFun finalName) sig stmts defs
            let deps = maybe Set.empty AC.funcDepends mfunc
            logMsg 2 $ T.pack $ "Function Dependencies: " ++ show deps
            MSS.modify' $ \s -> s { funDeps = Map.insert finalName Set.empty (funDeps s) }
            alldeps <- mapM (translationLoop fromInstr (finalName : callStack) defs) (Set.toList deps)
            let alldepsSet = Set.union (Set.unions alldeps) (finalDepsOf deps)
            MSS.modify' $ \s -> s { funDeps = Map.insert finalName alldepsSet (funDeps s) }
            maybeSimulateFunction fromInstr (KeyFun finalName) mfunc
            return alldepsSet

execSigMapWithScope :: TranslatorOptions
                    -> SigState
                    -> SigEnv
                    -> (forall sym. SigMapM sym arch ())
                    -> IO (SomeSigMap)
execSigMapWithScope opts sigState sigEnv action = do
  handleAllocator <- CFH.newHandleAllocator
  let nonceGenerator = globalNonceGenerator
  let sigMap =
        SigMap {
          sMap = Map.empty
          , instrExcepts = Map.empty
          , funExcepts = Map.empty
          , sigState = sigState
          , sigEnv = sigEnv
          , instrDeps = Map.empty
          , funDeps = Map.empty
          , sFormulas = []
          , sOptions = opts
          , sNonceGenerator = nonceGenerator
          , sHandleAllocator = handleAllocator
          }
  SomeSigMap <$> execSigMapM action sigMap

withOnlineBackend :: forall scope arch a
                        . ElemKey
                       -> (CBO.YicesOnlineBackend scope (B.Flags B.FloatReal) -> IO a)
                       -> SigMapM scope arch (Maybe a)
withOnlineBackend key action = do
  let feat =     useIntegerArithmetic
             .|. useBitvectors
             .|. useStructs
  gen <- MSS.gets sNonceGenerator
  catchIO key $ CBO.withOnlineBackend B.FloatRealRepr gen feat $ \sym -> do
    WC.extendConfig Yices.yicesOptions (WI.getConfiguration sym)
    action sym

memoryUFSigs :: [(T.Text, ((Some (Ctx.Assignment WI.BaseTypeRepr), Some WI.BaseTypeRepr)))]
memoryUFSigs = concatMap mkUF [1,2,4,8,16]
  where
    ramRepr = WI.BaseArrayRepr (Ctx.empty Ctx.:> WI.BaseBVRepr (WI.knownNat @32)) (WI.BaseBVRepr (WI.knownNat @8))
    mkUF :: Integer -> [(T.Text, (Some (Ctx.Assignment WI.BaseTypeRepr), Some WI.BaseTypeRepr))]
    mkUF sz
      | Just (Some szRepr) <- WI.someNat sz
      , Just WI.LeqProof <- WI.knownNat @1 `WI.testLeq` szRepr
      , bvSize <- (WI.knownNat @8) `WI.natMultiply` szRepr
      , WI.LeqProof <- WI.leqMulPos (WI.knownNat @8) szRepr =
        [( "write_mem_" <> (T.pack (show sz))
         , ( Some (Ctx.empty Ctx.:> ramRepr Ctx.:> (WI.BaseBVRepr (WI.knownNat @32)) Ctx.:> WI.BaseBVRepr bvSize)
           , Some ramRepr))
        ,( "read_mem_" <> (T.pack (show sz))
         , ( Some (Ctx.empty Ctx.:> ramRepr Ctx.:> (WI.BaseBVRepr (WI.knownNat @32)))
           , Some (WI.BaseBVRepr bvSize)))
        ]
    mkUF _ = error "unreachable"

addMemoryUFs :: SigMapM sym arch ()
addMemoryUFs = do
  Just ufs <- withOnlineBackend (KeyFun "memory") $ \sym -> do
    forM memoryUFSigs $ \(name, (Some argTs, Some retT)) -> do
      let symbol = U.makeSymbol (T.unpack name)
      symFn <- WI.freshTotalUninterpFn sym symbol argTs retT
      return $ ("uf." <> name, U.SomeSome symFn)
  MSS.modify $ \s -> s { sFormulas = ufs ++ (sFormulas s) }

-- Extremely vague measure of function body size
measureStmts :: [AS.Stmt] -> Int
measureStmts stmts = getSum $ runIdentity $ mconcat <$> traverse (TR.collectSyntax doCollect) stmts
  where
    doCollect :: TR.KnownSyntaxRepr t => t -> Identity (Sum Int)
    doCollect _ = return 1

-- | Translate an ASL instruction or function into a crucible CFG.
-- Optionally we may return Nothing if translation fails and we are capturing
-- the resulting exception instead of throwing it.
translateFunction :: ElemKey
                  -> FunctionSignature globalReads globalWrites init tps
                  -> [AS.Stmt]
                  -> Definitions arch
                  -> SigMapM sym arch (Maybe (AC.Function arch globalReads globalWrites init tps))
translateFunction key sig stmts defs = do
  logMsg 1 $ T.pack $ "Rough function body size:" ++ show (measureStmts stmts)
  handleAllocator <- MSS.gets sHandleAllocator
  logLvl <- MSS.gets (optVerbosity . sOptions)
  catchIO key $ AC.functionToCrucible defs sig handleAllocator stmts logLvl


-- | Simulate a function if we have one, and it is not filtered out.
-- If we are skipping translation, then simply emit an uninterpreted function with
-- the correct signature
maybeSimulateFunction :: InstructionIdent
                      -> ElemKey
                      -> Maybe (AC.Function arch globalReads globalWrites init tps)
                      -> SigMapM sym arch ()
maybeSimulateFunction _ _ Nothing = return ()
maybeSimulateFunction fromInstr key (Just func) =
 isKeySimFilteredOut fromInstr key >>= \case
   False -> simulateFunction key func
   True -> do
     Just symFn <- withOnlineBackend key $ \sym -> do
        let sig = AC.funcSig func
        let symbol = U.makeSymbol (T.unpack (funcName sig))
        let retT = funcSigBaseRepr sig
        let argTs = funcSigAllArgsRepr sig
        WI.freshTotalUninterpFn sym symbol argTs retT
     addFormula key symFn

addFormula :: ElemKey -> B.ExprSymFn scope args ret -> SigMapM scope arch ()
addFormula key symFn = MSS.modify $ \s -> s { sFormulas = (T.pack $ "uf." ++ prettyKey key, U.SomeSome symFn) : (sFormulas s)}


data SimulationException where
  SimulationDeserializationFailure :: String -> T.Text -> SimulationException
  SimulationDeserializationMismatch :: forall t ret ret'
                                     . T.Text
                                    -> (B.Expr t ret)
                                    -> (B.Expr t ret')
                                    -> [(Some (B.Expr t), Some (B.Expr t))]
                                    -> SimulationException
  SimulationFailure :: SimulationException

instance Show SimulationException where
  show se = case se of
    SimulationDeserializationFailure err formula ->
      "SimulationDeserializationFailure:\n" ++ err ++ "\n" ++ T.unpack formula
    SimulationDeserializationMismatch _sexpr expr1 expr2 env ->
     PP.render $ PP.vcat $
      [ PP.text "SimulationDeserializationMismatch" ]
      ++ showExprPair (Some expr1, Some expr2)
      ++ showExprContext 3 env
    SimulationFailure -> "SimulationFailure"
instance X.Exception SimulationException

showExprPair :: (Some (B.Expr t), Some (B.Expr t)) -> [PP.Doc]
showExprPair (Some expr1, Some expr2) =
  [PP.text "Original Expression:", showExpr expr1, PP.text "Deserialized Expression:", showExpr expr2]

showExprContext :: Int -> [(Some (B.Expr t), Some (B.Expr t))] -> [PP.Doc]
showExprContext _ [] = []
showExprContext count env = [PP.text "With context:"] ++ concat (map showExprPair (take count env))

showExpr :: B.Expr t ret -> PP.Doc
showExpr e = PP.text (LPP.displayS (LPP.renderPretty 0.4 80 (WI.printSymExpr e)) "")


mkParserConfig :: forall sym scope
                . sym ~ CBO.YicesOnlineBackend scope (B.Flags B.FloatReal)
               => sym
               -> WP.SymFnEnv sym
               -> WP.ParserConfig sym
mkParserConfig sym fenv =
  WP.ParserConfig { pSymFnEnv = fenv
                  , pGlobalLookup = \_ -> return Nothing
                  , pOverrides = \_ -> Nothing
                  , pSym = sym
                  }

-- | Simulate the given crucible CFG, and if it is a function add it to
-- the formula map.
simulateFunction :: forall arch sym globalReads globalWrites init tps
                  . ElemKey
                 -> AC.Function arch globalReads globalWrites init tps
                 -> SigMapM sym arch ()
simulateFunction key p = do
  logMsg 1 $ T.pack $ "Simulating: " ++ prettyKey key
  checkSerialization <- MSS.gets (optCheckSerialization . sOptions)
  opts <- MSS.gets sOptions
  handleAllocator <- MSS.gets sHandleAllocator
  mresult <- withOnlineBackend key $ \backend -> do
    let cfg = ASL.SimulatorConfig { simOutputHandle = IO.stdout
                                  , simHandleAllocator = handleAllocator
                                  , simSym = backend
                                  }
    let nm = prettyKey key
    when checkSerialization $ B.startCaching backend
    symFn <- ASL.simulateFunction cfg p

    ex <- if checkSerialization then do
      let (serializedSymFn, fenv) = WP.printSymFn' symFn
      lcfg <- U.mkLogCfg "check serialization"
      res <- U.withLogCfg lcfg $
        WP.readSymFn (mkParserConfig backend fenv) serializedSymFn
      case res of
        Left err -> do
          return $ Just $ SimulationDeserializationFailure err serializedSymFn
        Right (U.SomeSome symFn') -> do
          logMsgIO opts 1 $ "Serialization/Deserialization succeeded."
          WN.testEquivSymFn backend symFn symFn' >>= \case
            WN.ExprUnequal e1 e2 env -> do
              logMsgIO opts 1 $ "Mismatch in deserialized function."
              return $ Just $ SimulationDeserializationMismatch serializedSymFn e1 e2 env
            _ -> do
              logMsgIO opts 1 $ "Deserialized function matches."
              return Nothing
      else return Nothing
    return $ (nm, symFn, ex)
  case mresult of
    Just (_, symFn, mex) -> do
      logMsg 1 "Simulation succeeded!"
      addFormula key symFn
      case mex of
        Just ex -> void $ catchIO key $ X.throw ex
        _ -> return ()
    _ -> X.throw $ SimulationFailure

getASL :: TranslatorOptions -> IO (ASLSpec)
getASL opts = do
  let getPath file = (optASLSpecFilePath opts ++ file)
  eAslDefs <- AP.parseAslDefsFile (getPath defsFilePath)
  eAslSupportDefs <- AP.parseAslDefsFile (getPath supportFilePath)
  eAslExtraDefs <- AP.parseAslDefsFile (getPath extraDefsFilePath)
  eAslInsts <- AP.parseAslInstsFile (getPath instsFilePath)
  eAslRegs <- AP.parseAslRegsFile (getPath regsFilePath)
  case (eAslInsts, eAslDefs, eAslRegs, eAslExtraDefs, eAslSupportDefs) of
    (Left err, _, _, _, _) -> do
      hPutStrLn stderr $ "Error loading ASL instructions: " ++ show err
      exitFailure
    (_, Left err, _, _, _) -> do
      hPutStrLn stderr $ "Error loading ASL definitions: " ++ show err
      exitFailure
    (_, _, Left err ,_, _) -> do
      hPutStrLn stderr $ "Error loading ASL registers: " ++ show err
      exitFailure
    (_, _, _ , Left err, _) -> do
      hPutStrLn stderr $ "Error loading extra ASL definitions: " ++ show err
      exitFailure
    (_, _, _ , _, Left err) -> do
      hPutStrLn stderr $ "Error loading ASL support definitions: " ++ show err
      exitFailure
    (Right aslInsts, Right aslDefs, Right aslRegs, Right aslExtraDefs, Right aslSupportDefs) -> do
      return $ prepASL $ ASLSpec aslInsts aslDefs aslSupportDefs aslExtraDefs aslRegs

logMsgIO :: TranslatorOptions -> Integer -> T.Text -> IO ()
logMsgIO opts logLvl msg = do
  let verbosity = (optVerbosity opts)
  E.when (verbosity >= logLvl) $ liftIO $ putStrLn (T.unpack msg)

isKeySimFilteredOut :: InstructionIdent -> ElemKey -> SigMapM sym arch Bool
isKeySimFilteredOut fromInstr key = case key of
  KeyFun fnm -> do
    test <- MSS.gets (funSimFilter . optFilters . sOptions)
    return $ not $ test fromInstr fnm
  KeyInstr instr -> do
    test <- MSS.gets (instrSimFilter . optFilters . sOptions)
    return $ not $ test instr

isFunFilteredOut :: InstructionIdent -> T.Text -> SigMapM sym arch Bool
isFunFilteredOut inm fnm = do
  test <- MSS.gets (funFilter . optFilters . sOptions)
  return $ not $ test inm fnm

data ExpectedException =
    RealValueUnsupported
  | InsufficientStaticTypeInformation
  | CruciblePanic
  | ASLSpecMissingZeroCheck
  | BVLengthFromGlobalState
  | PrinterParserError
   deriving (Eq, Ord, Show)

expectedExceptions :: ElemKey -> TranslatorException -> Maybe ExpectedException
expectedExceptions k ex = case ex of
  -- SimExcept _ (SimulationDeserializationMismatch _ _) -> Just $ PrinterParserError
  SExcept _ (TypeNotFound "real") -> Just $ RealValueUnsupported
  -- TExcept _ (CannotMonomorphizeFunctionCall _ _) -> Just $ InsufficientStaticTypeInformation
  -- TExcept _ (CannotStaticallyEvaluateType _ _) -> Just $ InsufficientStaticTypeInformation
  -- TExcept _ (CannotDetermineBVLength _ _) -> Just $ InsufficientStaticTypeInformation
  TExcept _ (UnsupportedType (AS.TypeFun "bits" (AS.ExprLitInt 0)))
    | KeyInstr (InstructionIdent nm _ _) <- k
    , nm `elem` ["aarch32_USAT16_A", "aarch32_USAT_A"] ->
      Just $ ASLSpecMissingZeroCheck
  TExcept _ (CannotStaticallyEvaluateType (AS.TypeFun "bits" (AS.ExprCall (AS.VarName fnm) _)) _)
    | fnm `elem` ["BAREGETTER_PL", "BAREGETTER_VL"] ->
      Just $ BVLengthFromGlobalState
  SomeExcept e
    | Just (Panic (_ :: Crucible) _ _ _) <- X.fromException e
    , KeyInstr (InstructionIdent nm _ _) <- k
    , nm `elem` ["aarch32_WFE_A", "aarch32_WFI_A", "aarch32_VTBL_A", "aarch32_VTST_A"] ->
      Just $ CruciblePanic
  SomeExcept e
    | Just (Panic (_ :: Crucible) _ _ _) <- X.fromException e
    , KeyFun nm <- k
    , nm `elem` ["AArch32_ExclusiveMonitorsPass_2"] ->
      Just $ CruciblePanic
  SomeExcept e
    | Just (ASL.SimulationAbort _ _) <- X.fromException e
    , KeyInstr (InstructionIdent nm _ _) <- k
    , nm `elem` [ "aarch32_VTBL_A" ] ->
      Just $ CruciblePanic
  _ -> Nothing

isUnexpectedException :: ElemKey -> TranslatorException -> Bool
isUnexpectedException k e = expectedExceptions k e == Nothing

forMwithKey_ :: Applicative t => Map k a -> (k -> a -> t b) -> t ()
forMwithKey_ m f = void $ Map.traverseWithKey f m

reportStats :: StatOptions -> SigMap sym arch -> IO ()
reportStats sopts sm = do
  let expectedInstrs = Map.foldrWithKey (addExpected . KeyInstr) Map.empty (instrExcepts sm)
  let expected = Map.foldrWithKey (addExpected . KeyFun) expectedInstrs (funExcepts sm)
  let unexpectedElems =
        Set.union (Set.map KeyInstr $ Map.keysSet (instrExcepts sm)) (Set.map KeyFun $ Map.keysSet (funExcepts sm))
        Set.\\ (Set.unions $ Map.elems expectedInstrs ++ Map.elems expected)

  when (not (Set.null unexpectedElems)) $ do
    putStrLn $ "Unexpected exceptions:"
    forMwithKey_ (instrExcepts sm) $ \ident -> \e ->
      E.when (unexpected (KeyInstr ident) e) $ do
        putStrLn $ prettyIdent ident ++ " failed to translate:"
        putStrLn $ show e
    putStrLn "----------------------"
    forMwithKey_ (instrDeps sm) $ \ident -> \deps -> do
      let errs' = catMaybes $ (\dep -> (\x -> (dep,x)) <$> Map.lookup dep (funExcepts sm)) <$> (Set.toList deps)
      let errs = filter (\(dep, x) -> unexpected (KeyFun dep) x) errs'
      if null errs then return ()
      else do
        putStrLn $ prettyIdent ident ++ " has failing dependencies:"
        mapM_ (\(dep, err) -> putStrLn $ show dep <> ":" <> show err) errs
    putStrLn "----------------------"
  when (reportKnownExceptions sopts) $ do


    forMwithKey_ expected $ \ex -> \ks -> do
      putStrLn $ "Failures due to known exception: " <> show ex
      putStrLn "----------------------"
      mapM_ printKey ks
      putStrLn ""
    return ()

  putStrLn $ "Total instructions inspected: " <> show (Map.size $ instrDeps sm)
  putStrLn $ "Total functions inspected: " <> show (Map.size $ funDeps sm)
  putStrLn $ "Number of instructions which raised exceptions: " <> show (Map.size $ instrExcepts sm)
  putStrLn "----------------------"
  when (reportSucceedingInstructions sopts) $
    putStrLn $ "Instructions with no errors in any dependent functions:"
  r <- Map.traverseMaybeWithKey (\ident -> \deps -> do
    if not (Map.member ident (instrExcepts sm)) &&
       Set.null (Set.filter (\dep -> Map.member dep (funExcepts sm)) deps)
    then do
      E.when (reportSucceedingInstructions sopts) $ putStrLn $ prettyIdent ident
      return $ Just ident
    else return Nothing) (instrDeps sm)
  putStrLn $ "Number of successfully translated functions: " <> show (Map.size $ r)
  where
    reverseDependencyMap =
        Map.fromListWith (++) $ concat $ map (\(instr, funs) -> map (\fun -> (fun, [instr])) (Set.toList funs))
           (Map.assocs (instrDeps sm))
    printKey k = case k of
      KeyInstr ident -> putStrLn $ "Instruction: " <> prettyIdent ident
      KeyFun nm -> do
        putStrLn $ "Function: " <> show nm
        E.when (reportFunctionDependencies sopts) $ do
          putStrLn $ "Which is depended on by: "
          case Map.lookup nm reverseDependencyMap of
            Just instrs -> mapM_ (\ident -> putStrLn $  "    " <> prettyIdent ident) instrs
            _ -> return ()
    unexpected k err =
      if reportAllExceptions sopts then True else isUnexpectedException k err
    addExpected nm err = case expectedExceptions nm err of
      Just e -> if (reportKnownExceptionFilter sopts e)
                then Map.insertWith Set.union e (Set.singleton nm)
                else id
      Nothing -> id

prettyIdent :: InstructionIdent -> String
prettyIdent (InstructionIdent nm enc iset) = T.unpack $ nm <> "_" <> enc <> "_" <> (T.pack $ show iset)


prettyKey :: ElemKey -> String
prettyKey (KeyInstr ident) = prettyIdent ident
prettyKey (KeyFun fnName) = T.unpack fnName

data TranslatorException =
    TExcept ElemKey TranslationException
  | SExcept ElemKey SigException
  | SimExcept ElemKey SimulationException
  | BadTranslatedInstructionsFile
  | SomeExcept X.SomeException

instance Show TranslatorException where
  show e = case e of
    TExcept k te -> "Translator exception in: " ++ prettyKey k ++ "\n" ++ show te
    SExcept k se -> "Signature exception in:" ++ prettyKey k ++ "\n" ++ show se
    SimExcept k se -> "Simulation exception in:" ++ prettyKey k ++ "\n" ++ show se
    SomeExcept err -> "General exception:\n" ++ show err
    BadTranslatedInstructionsFile -> "Failed to load translated instructions."

instance X.Exception TranslatorException

data InstructionIdent =
  InstructionIdent { iName :: T.Text, iEnc :: T.Text, iSet :: AS.InstructionSet }
  deriving (Eq, Ord, Show)

instrToIdent :: AS.Instruction -> T.Text -> AS.InstructionSet -> InstructionIdent
instrToIdent AS.Instruction{..} enc iset = InstructionIdent instName enc iset

finalDepsOf :: Set.Set (T.Text, StaticValues) -> Set.Set T.Text
finalDepsOf deps = Set.map (\(nm, env) -> mkFinalFunctionName env nm) deps


data Filters = Filters { funFilter :: InstructionIdent -> T.Text -> Bool
                       , instrFilter :: InstructionIdent -> Bool
                       , funSimFilter :: InstructionIdent -> T.Text -> Bool
                       , instrSimFilter :: InstructionIdent -> Bool
                       }

data ElemKey =
   KeyInstr InstructionIdent
 | KeyFun T.Text
 deriving (Eq, Ord, Show)


-- FIXME: Seperate this into RWS
data SigMap scope arch where
  SigMap :: { sMap :: Map.Map T.Text (Some (SomeFunctionSignature))
            , instrExcepts :: Map.Map InstructionIdent TranslatorException
            , funExcepts :: Map.Map T.Text TranslatorException
            , sigState :: SigState
            , sigEnv :: SigEnv
            , instrDeps :: Map.Map InstructionIdent (Set.Set T.Text)
            , funDeps :: Map.Map T.Text (Set.Set T.Text)
            , sFormulas :: [(T.Text, (U.SomeSome (B.ExprSymFn scope)))]
            , sOptions :: TranslatorOptions
            , sNonceGenerator :: NonceGenerator IO scope
            , sHandleAllocator :: CFH.HandleAllocator
            } -> SigMap scope arch

data SomeSigMap where
  SomeSigMap :: forall scope arch. SigMap scope arch -> SomeSigMap

type SigMapM scope arch a = MSS.StateT (SigMap scope arch) IO a

execSigMapM :: SigMapM scope arch a -> SigMap scope arch -> IO (SigMap scope arch)
execSigMapM m = MSS.execStateT m

--instance TR.MonadLog SigMapM where
logMsg :: Integer -> T.Text -> SigMapM scope arch ()
logMsg logLvl msg = do
  verbosity <- MSS.gets (optVerbosity . sOptions)
  when (verbosity >= logLvl) $ liftIO $ putStrLn $ T.unpack $ msg


printLog :: [T.Text] -> SigMapM scope arch ()
printLog [] = return ()
printLog log' = liftIO $ putStrLn (T.unpack $ T.unlines log')

liftSigM :: ElemKey -> SigM ext f a -> SigMapM scope arch (Either SigException a)
liftSigM k f = do
  state <- MSS.gets sigState
  env <- MSS.gets sigEnv
  logLvl <- MSS.gets (optVerbosity . sOptions)
  let ((result, state'), log') = runSigM env state logLvl f
  case result of
    Right a -> do
      when (logLvl >= 5) $ printLog log'
      MSS.modify' $ \s -> s { sigState = state' }
      return $ Right a
    Left err -> do
      printLog log'
      collectExcept k (SExcept k err)
      return $ Left err

collectExcept :: ElemKey -> TranslatorException -> SigMapM scope arch ()
collectExcept k e = do
  collectAllExceptions <- MSS.gets (optCollectAllExceptions . sOptions)
  collectExpectedExceptions <- MSS.gets (optCollectExpectedExceptions . sOptions)
  if (collectAllExceptions || ((not $ isUnexpectedException k e) && collectExpectedExceptions))
  then case k of
    KeyInstr ident -> MSS.modify' $ \s -> s { instrExcepts = Map.insert ident e (instrExcepts s) }
    KeyFun fun -> MSS.modify' $ \s -> s { funExcepts = Map.insert fun e (funExcepts s) }
  else X.throw e

catchIO :: ElemKey -> IO a -> SigMapM scope arch (Maybe a)
catchIO k f = do
  a <- liftIO ((Left <$> f)
                  `X.catch` (\(e :: TranslationException) -> return $ Right $ TExcept k e)
                  `X.catch` (\(e :: SimulationException) -> return $ Right $ SimExcept k e)
                  `X.catch` (\(e :: X.SomeException) -> return $ Right (SomeExcept e)))
  case a of
    Left r -> return (Just r)
    Right err -> (\_ -> Nothing) <$> collectExcept k err

noFilter :: Filters
noFilter = Filters
  (\_ -> \_ -> True)
  (\_ -> True)
  (\_ -> \_ -> True)
  (\_ -> True)

translateAll :: Filters -> Filters
translateAll f =
  f { funFilter = (\_ -> \_ -> True)
    , instrFilter = (\_ -> True)
    }

translateOnlyInstr :: (T.Text, T.Text) -> Filters -> Filters
translateOnlyInstr inm f =
  f { funFilter = (\(InstructionIdent nm enc _) -> \_ -> inm == (nm, enc))
    , instrFilter = (\(InstructionIdent nm enc _) -> (nm, enc) == inm)
    }


translateNoArch64 :: Filters -> Filters
translateNoArch64 f =
  f { funFilter = (\(InstructionIdent _ _ iset) -> \_ -> iset /= AS.A64 )
    , instrFilter = (\(InstructionIdent _ _ iset) -> iset /= AS.A64)
    }


translateArch32 :: Filters -> Filters
translateArch32 f =
  f { funFilter = (\(InstructionIdent _ _ iset) -> \_ -> iset `elem` [AS.A32, AS.T32] )
    , instrFilter = (\(InstructionIdent _ _ iset) -> iset `elem` [AS.A32, AS.T32])
    }

simulateAll :: Filters -> Filters
simulateAll f =
  f { funSimFilter = \_ _ -> True
    , instrSimFilter = \_ -> True
    }

simulateInstructions :: Filters -> Filters
simulateInstructions f =
  f { funSimFilter = \_ _ -> False
    , instrSimFilter = \_ -> True
    }

simulateNone :: Filters -> Filters
simulateNone f =
  f { funSimFilter = \_ _ -> False
    , instrSimFilter = \_ -> False
    }
