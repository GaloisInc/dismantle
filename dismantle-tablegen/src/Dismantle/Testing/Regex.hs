{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Dismantle.Testing.Regex
    ( mkRegex
    , mkRegexT
    , RE.Regex
    , countMatches
    , hasMatches
    -- , (*=~)
    -- , (=~)
    )
    where

import           Data.String
import qualified Data.Text as DT
import qualified Data.Text.Lazy as DLT
import qualified Text.Regex.TDFA as RE


mkRegex :: String -> Either String RE.Regex
mkRegex = RE.makeRegexM

mkRegexT :: DT.Text -> Either String RE.Regex
mkRegexT = RE.makeRegexM . DT.unpack

-- (*=~) :: IsString source -> source -> RE.Regex -> RE.Match
-- (*=~) a b = RE.findAll

-- (=~) :: IsString source => source -> RE.Regex -> Bool
-- (=~) i r = isJust $ RE.find r (TL.toStrict $ insnText i)
-- infix 5 =~

countMatches :: (IsString source, RE.RegexLike RE.Regex source) => source -> RE.Regex -> Int
countMatches t r = RE.matchCount r t

hasMatches :: (IsString source, RE.RegexLike RE.Regex source) => source -> RE.Regex -> Bool
hasMatches t r = RE.matchTest r t


-- --------------------------------------------------

instance RE.Extract DT.Text where
    before n s = DT.pack $ RE.before n $ DT.unpack s
    after n s = DT.pack $ RE.after n $ DT.unpack s
    empty = DT.empty
    extract r s = DT.pack $ RE.extract r $ DT.unpack s


instance RE.RegexLike RE.Regex DT.Text where
    matchOnce r = RE.matchOnce r . DT.unpack
    matchAll r = RE.matchAll r . DT.unpack
    matchCount r = RE.matchCount r . DT.unpack
    matchTest r = RE.matchTest r . DT.unpack

    matchAllText r s = let subres = RE.matchAllText r (DT.unpack s)
                           reText (x, y) = (DT.pack x, y)
                       in fmap (fmap reText) subres
    matchOnceText r s = let subres = RE.matchOnceText r (DT.unpack s)
                            reText (x, y) = (DT.pack x, y)
                        in subres >>= \(a,b,c) ->
                            return (DT.pack a, fmap reText b, DT.pack c)


-- --------------------------------------------------

instance RE.Extract DLT.Text where
    before n s = DLT.pack $ RE.before n $ DLT.unpack s
    after n s = DLT.pack $ RE.after n $ DLT.unpack s
    empty = DLT.empty
    extract r s = DLT.pack $ RE.extract r $ DLT.unpack s

instance RE.RegexLike RE.Regex DLT.Text where
    matchOnce r = RE.matchOnce r . DLT.unpack
    matchAll r = RE.matchAll r . DLT.unpack
    matchCount r = RE.matchCount r . DLT.unpack
    matchTest r = RE.matchTest r . DLT.unpack

    matchAllText r s = let subres = RE.matchAllText r $ DLT.unpack s
                           reText (x, y) = (DLT.pack x, y)
                       in fmap (fmap reText) subres
    matchOnceText r s = let subres = RE.matchOnceText r $ DLT.unpack s
                            reText (x, y) = (DLT.pack x, y)
                        in subres >>= \(a,b,c) ->
                            return (DLT.pack a, fmap reText b, DLT.pack c)

-- (=~) :: Instruction -> RE.Regex -> Bool
-- (=~) i r = isJust $ RE.find r (DLT.toStrict $ insnText i)
-- infix 5 =~
