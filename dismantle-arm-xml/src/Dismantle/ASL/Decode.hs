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

data DecodeEnv = DecodeEnv { envLogFn :: String -> IO ()
                           , envContext :: DecodeContext
                           -- ^ the current context for error reporting
                           }

data DecodeContext = DecodeContext { ctxInstruction :: Maybe (ASL.Instruction)
                                   , ctxFileName :: String
                                   , ctxSourceLoc :: Maybe SrcLoc
                                   }

type DecodeState = ()
type DecodeOut = ()

data DecodeException =
    DecodeMFail String
  | NotImplementedYet String
  | ASLParserError String
  deriving (Eq, Ord, Show)

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
  pPrint e = PP.text $ show e

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
                            }
    initEnv = DecodeEnv { envLogFn = logFn
                        , envContext = initCtx
                        }
    initState = ()
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
    forM instrs $ \instr -> do
      loadInstruction instr
    return ()
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

loadInstruction :: ASL.Instruction -> DecodeM ()
loadInstruction instr = withContext (\ctx -> ctx { ctxInstruction = Just instr } ) $ do
  warnError $ NotImplementedYet "loadInstruction"
