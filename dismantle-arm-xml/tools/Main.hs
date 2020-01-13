{-# LANGUAGE GADTs #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE LambdaCase #-}

module Main ( main ) where

import           Data.Parameterized.Nonce
import           Data.Parameterized.Some ( Some(..) )
import qualified What4.Serialize.Parser as WP
import qualified What4.Utils.Log as U
import qualified What4.Utils.Util as U
import qualified What4.Expr.Builder as B
import           Control.Monad.IO.Class
import qualified Codec.Compression.GZip as GZip
import           System.IO
import qualified Data.ByteString.Lazy as BS
import qualified Data.Text.Lazy as T
import qualified Data.Text.Lazy.IO as T
import qualified Data.Text.Lazy.Encoding as T

formulasFile :: FilePath
formulasFile = "./formulas.what4"

main :: IO ()
main = return ()

data BuilderData t = NoBuilderData

readFormulas :: FilePath -> IO ()
readFormulas path = do
  Some r <- liftIO $ newIONonceGenerator
  sym <- liftIO $ B.newExprBuilder B.FloatRealRepr NoBuilderData r
  lcfg <- U.mkLogCfg "check serialization"
  U.withLogCfg lcfg $
    WP.readSymFnEnvFromFile (WP.defaultParserConfig sym) path >>= \case
      Left err -> fail err
      Right _ -> return ()
