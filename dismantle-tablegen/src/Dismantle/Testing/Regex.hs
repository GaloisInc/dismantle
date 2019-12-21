{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Dismantle.Testing.Regex
    ( mkRegex
    , mkRegexT
    , RE.Regex
    , countMatches
    , hasMatches
    )
    where

import           Control.Monad.Fail
import           Data.String
import qualified Data.Text as DT
import qualified Text.Regex.TDFA as RE


mkRegex :: String -> Either String RE.Regex
mkRegex = runEitherString . RE.makeRegexM

mkRegexT :: DT.Text -> Either String RE.Regex
mkRegexT = mkRegex . DT.unpack

countMatches :: (IsString source, RE.RegexLike RE.Regex source) =>
                source -> RE.Regex -> Int
countMatches t r = RE.matchCount r t

hasMatches :: (IsString source, RE.RegexLike RE.Regex source) =>
              source -> RE.Regex -> Bool
hasMatches t r = RE.matchTest r t

newtype EitherString a = EitherString { runEitherString :: Either String a }
    deriving (Eq, Ord, Read, Show, Functor, Applicative, Monad)

instance MonadFail EitherString where fail = EitherString . Left

