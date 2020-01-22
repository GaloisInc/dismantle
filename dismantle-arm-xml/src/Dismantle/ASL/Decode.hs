{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE PatternSynonyms #-}

module Dismantle.ASL.Decode
  where

import           Debug.Trace

import           Prelude hiding ( fail )

import           GHC.Stack

import           Control.Exception ( try, assert )
import qualified Control.Exception as E

import           Control.Monad ( forM, forM_ )
import           Control.Monad.Fail
import           Control.Monad.Identity
import           Control.Monad.Trans ( lift, liftIO, MonadIO )
import           Control.Monad.Trans.RWS.Strict ( RWST )
import qualified Control.Monad.Trans.RWS.Strict as RWS
import qualified Control.Monad.State as MS
import qualified Control.Monad.Reader as MR
import qualified Control.Monad.Writer as MW
import           Control.Monad.Except ( throwError, ExceptT, MonadError )
import qualified Control.Monad.Except as ME

import qualified Data.Text as T

import           System.IO ( withFile, IOMode(..), hPutStrLn )

import           Text.PrettyPrint.HughesPJClass ( (<+>), ($$), ($+$) )
import qualified Text.PrettyPrint.HughesPJClass as PP

import           Language.ASL.Parser as ASL
import           Language.ASL.Syntax as ASL

import qualified Dismantle.Tablegen.ByteTrie as BT

data DecodeEnv = DecodeEnv { envLogFn :: String -> IO ()
                           , envContext :: DecodeContext
                           -- ^ the current context for error reporting
                           }

data DecodeContext = DecodeContext { ctxInstruction :: Maybe ASL.Instruction
                                   , ctxEncoding :: Maybe ASL.InstructionEncoding
                                   , ctxFileName :: String
                                   , ctxSourceLoc :: Maybe SrcLoc
                                   }

data DecodeState = DecodeState { stMaskTree :: !(MaskTree (ASL.Instruction, ASL.InstructionEncoding)) }

type DecodeOut = ()

data DecodeException =
    DecodeMFail String
  | MissingExpectedMaskTreeEntry ASL.Instruction ASL.InstructionEncoding
  | MultipleMaskTreeEntries [(ASL.Instruction, ASL.InstructionEncoding)]
  | NotImplementedYet String
  | ASLParserError String
  deriving (Eq, Show)

data OuterDecodeException = OuterDecodeException DecodeContext DecodeException

instance E.Exception OuterDecodeException

newtype DecodeM a = DecodeM (RWST DecodeEnv DecodeOut DecodeState (ExceptT OuterDecodeException IO) a)
  deriving ( Functor
           , Applicative
           , Monad
           , MS.MonadState DecodeState
           , MR.MonadReader DecodeEnv
           , MW.MonadWriter DecodeOut
           , MonadIO
           )

instance MonadFail DecodeM where
  fail msg = throwError $ DecodeMFail msg

instance MonadError DecodeException DecodeM where
  throwError e = do
    ctx <- MR.asks envContext
    DecodeM (lift $ throwError $ OuterDecodeException ctx e)

  catchError (DecodeM m) handler = do
    st <- MS.get
    env <- MR.ask
    (liftIO $ ME.runExceptT $ RWS.runRWST m env st) >>= \case
      Left (OuterDecodeException _ e) -> handler e
      Right (a, st', out) -> do
        MS.put st'
        MW.tell out
        return a

throwErrorHere :: HasCallStack => DecodeException -> DecodeM a
throwErrorHere e = do
  let (_, src): _ = getCallStack callStack
  ctx <- MR.asks envContext
  DecodeM (lift $ throwError $ OuterDecodeException (ctx { ctxSourceLoc = Just src }) e)

instance PP.Pretty OuterDecodeException where
  pPrint (OuterDecodeException ctx e) =
    PP.text "Dismantle.ASL.Decode: Error encountered while processing:" <+> PP.text (ctxFileName ctx)
    <+> case ctxSourceLoc ctx of
          Just loc -> PP.text "at" <+> PP.text (prettySrcLoc loc)
          Nothing -> PP.empty
    $$ (PP.nest 1 $ PP.pPrint e)

instance Show OuterDecodeException where
  show e = PP.render (PP.pPrint e)

instance PP.Pretty DecodeException where
  pPrint e = case e of
    MultipleMaskTreeEntries tes ->
      PP.text "MultipleMaskTreeEntries:"
      $$ PP.nest 1 (PP.vcat (map (uncurry prettyInstructionEncoding) tes))
    MissingExpectedMaskTreeEntry instr enc ->
      PP.text "MissingExpectedMaskTreeEntry" <+> prettyInstructionEncoding instr enc
    _ -> PP.text (show e)

prettyInstructionEncoding :: ASL.Instruction -> ASL.InstructionEncoding -> PP.Doc
prettyInstructionEncoding instr enc = PP.hcat $ PP.punctuate (PP.text "/") $
   [ PP.text (T.unpack $ ASL.instName instr)
   , PP.text (T.unpack $ ASL.encName enc)
   , PP.text (show $ ASL.encInstrSet enc)
   ]

logMsg :: String -> DecodeM ()
logMsg msg = do
  logFn <- MR.asks envLogFn
  liftIO $ logFn msg

warnError :: DecodeException -> DecodeM ()
warnError e = do
  ctx <- MR.asks envContext
  let pretty = PP.nest 1 (PP.text "WARNING:" $$ (PP.pPrint $ OuterDecodeException ctx e))
  logMsg $ PP.render pretty

runDecodeM :: FilePath -> (String -> IO ()) -> DecodeM a -> IO (Either OuterDecodeException (a, DecodeState, DecodeOut))
runDecodeM aslfile logFn (DecodeM m) =
  let
    initCtx = DecodeContext { ctxFileName = aslfile
                            , ctxInstruction = Nothing
                            , ctxSourceLoc = Nothing
                            , ctxEncoding = Nothing
                            }
    initEnv = DecodeEnv { envLogFn = logFn
                        , envContext = initCtx
                        }
    initState = DecodeState { stMaskTree = MaskNil }

  in ME.runExceptT $ RWS.runRWST m initEnv initState

execDecodeM ::  FilePath -> (String -> IO ()) -> DecodeM a -> IO (Either OuterDecodeException a)
execDecodeM aslfile logFn m = do
  eresult <- runDecodeM aslfile logFn m
  return $ do
    (a, _, _) <- eresult
    return a

loadASL :: FilePath -> (String -> IO ()) -> IO ()
loadASL aslFile logFn = do
  logFn "Dismantle.ASL.Decode: loadASL"
  result <- runDecodeM aslFile logFn $ do
    instrs <- parseInstsFile aslFile
    forM_ instrs $ \instr -> do
      forEncodings instr loadEncoding
    forM_ instrs $ \instr -> do
      forEncodings instr checkEncoding
  case result of
    Left err -> do
      logFn (show err)
      E.throw err
    Right (a, _st, _out) -> return a

parseInstsFile :: FilePath -> DecodeM [ASL.Instruction]
parseInstsFile aslFile = do
  result <- liftIO $ do
    try (ASL.parseAslInstsFile aslFile) >>= \case
      Left (err :: IOError) -> return $ Left (show err)
      Right (Left errMsg) -> return $ Left (T.unpack errMsg)
      Right (Right result) -> return $ Right result
  case result of
    Left err -> do
      throwErrorHere $ ASLParserError err
    Right insts -> return insts

withContext :: (DecodeContext -> DecodeContext) -> DecodeM a -> DecodeM a
withContext f = MR.local (\env -> env { envContext = f (envContext env) })

forEncodings :: ASL.Instruction -> (ASL.Instruction -> ASL.InstructionEncoding -> DecodeM a) -> DecodeM [a]
forEncodings instr m = withContext (\ctx -> ctx { ctxInstruction = Just instr } ) $ do
  forM (instEncodings instr) $ \enc -> withContext (\ctx -> ctx { ctxEncoding = Just enc } ) $ do
    m instr enc

loadEncoding :: ASL.Instruction -> ASL.InstructionEncoding -> DecodeM ()
loadEncoding instr enc = do
  let mask = maskToBits $ encOpcodeMask enc
  tree <- MS.gets stMaskTree
  MS.modify' $ \st -> st { stMaskTree = addMaskToTree mask (instr, enc) tree }

checkEncoding :: ASL.Instruction -> ASL.InstructionEncoding -> DecodeM ()
checkEncoding instr enc = do
  let mask = maskToBits $ encOpcodeMask enc
  tree <- MS.gets stMaskTree
  return ()
  case lookupMaskFromTree mask tree of
    [] -> throwErrorHere $ MissingExpectedMaskTreeEntry instr enc
    _ -> return ()

data MaskTree a = MaskLeaf [a] | MaskNode !(MaskTree a) !(MaskTree a) !(MaskTree a) | MaskNil

freshNode :: MaskTree a
freshNode = MaskNode MaskNil MaskNil MaskNil

maskToBits :: ASL.Mask -> [BT.Bit]
maskToBits bits = map go bits
  where
    go :: ASL.MaskBit -> BT.Bit
    go b = case b of
      ASL.MaskBitEither -> BT.Any
      ASL.MaskBitSet -> BT.ExpectedBit True
      ASL.MaskBitUnset -> BT.ExpectedBit False

addMaskToTree :: forall a. [BT.Bit] -> a -> MaskTree a -> MaskTree a
addMaskToTree bits e tree = case (bits, tree) of
  ([], MaskNil) -> MaskLeaf [e]
  ([], MaskLeaf es) -> MaskLeaf (e : es)
  (bits, MaskNil) -> addMaskToTree bits e freshNode
  (bit : rst, MaskNode unset set either) -> case bit of
    BT.Any -> MaskNode unset set (go rst either)
    BT.ExpectedBit True -> MaskNode unset (go rst set) either
    BT.ExpectedBit False -> MaskNode (go rst unset) set either
  where
    go :: [BT.Bit] -> MaskTree a -> MaskTree a
    go rst tree' = addMaskToTree rst e tree'

lookupMaskFromTree :: forall a. [BT.Bit]-> MaskTree a -> [a]
lookupMaskFromTree bits tree = case (bits, tree) of
  (_, MaskNil) -> []
  (_, MaskLeaf es) -> es
  (bit : rst, MaskNode unset set either) -> case bit of
    BT.Any -> go rst unset ++ go rst set ++ go rst either
    BT.ExpectedBit True -> go rst set ++ go rst either
    BT.ExpectedBit False -> go rst unset ++ go rst either
  where
    go :: [BT.Bit] -> MaskTree a -> [a]
    go rst tree' = lookupMaskFromTree rst tree'
