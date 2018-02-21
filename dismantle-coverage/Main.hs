{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Main where

import           Control.Monad
import           Data.List
import           Data.Monoid
import qualified Data.Set            as S
import qualified Data.Text.Lazy.IO   as TL
import qualified Dismantle.PPC.ISA   as PPC
import qualified Dismantle.Tablegen  as D
import           Numeric
import qualified Options.Applicative as O
import           System.Directory

options :: O.Parser Options
options =
  Options
    <$> O.strArgument ( O.metavar "FILE" <> O.help "The tablegen output to parse")
    <*> O.strArgument ( O.metavar "PPC" <> O.help "The directory of semantics files for PPC64")
    <*> O.switch
          ( O.long "Dump undefined PPC64 semantics"
          <> O.short 'd'
          <> O.help "Whether to dump undefined semantics"
          )

-- | Options for command-line tool
data Options
  = Options
  { tableGenFile :: String
  , semanticsDirectory :: String
  , dumpUndefinedBaseInsts :: Bool
  } deriving (Show, Eq)

main :: IO ()
main = O.execParser opts >>= go
  where
    opts = O.info (O.helper <*> options) O.fullDesc

go :: Options -> IO ()
go opts = do
  t <- TL.readFile $ tableGenFile opts
  case D.parseTablegen (tableGenFile opts) t of
    Left e -> fail e
    Right records -> do
      let descriptors = D.parsableInstructions PPC.isa (D.filterISA PPC.isa records)
          ev   = [ d | d <- descriptors, "HasSPE" `elem` D.idPredicates d ]
          alti = [ d | d <- descriptors, "HasAltivec" `elem` D.idPredicates d ]
          vsx  = [ d | d <- descriptors, "HasVSX" `elem` D.idPredicates d ]
          base = [ d | d <- descriptors, null (D.idPredicates d) ]
      files <- fmap stripSuffix <$> do
        (++) <$> listDirectory (semanticsDirectory opts)
             <*> do listDirectory $ toManual (semanticsDirectory opts)
      let definedEV   = getDefined ev files
          definedAlti = getDefined alti files
          definedVSX  = getDefined vsx files
          definedBase = getDefined base files
      showCoverage "Alti" definedAlti alti
      showCoverage "VSX" definedVSX vsx
      showCoverage "Base" definedBase base
      when (dumpUndefinedBaseInsts opts) $ do
        writeFile "unsupported.txt"
          $ intercalate "\n"
          $ S.toList
          $ getUndefined base files
  where
    stripSuffix :: String -> String
    stripSuffix = takeWhile (/='.')

    toManual :: String -> String
    toManual = (++"manual/") . reverse . dropWhile (/='/') . reverse

    retrieve :: Ord a
             => (S.Set String -> S.Set a -> b)
             -> [D.InstructionDescriptor]
             -> [a]
             -> b
    retrieve f descriptor files =
      S.fromList (D.idMnemonic <$> descriptor)
        `f` S.fromList files

    getDefined = retrieve S.intersection
    getUndefined = retrieve S.difference

    showFloat x = showFFloat (Just 2) x ""

    showCoverage :: [Char] -> S.Set String -> [D.InstructionDescriptor] -> IO ()
    showCoverage name defined total = do
      putStrLn $ intercalate "\n" [
          "Extension: " ++ name
        , "Defined: " ++ do showFloat $ fromIntegral (S.size defined)
        , "Total: " ++ do showFloat $ fromIntegral (length total)
        , showFloat ((fromIntegral (S.size defined) / fromIntegral (length total)) * 100) ++ "%"
        ]
      putStrLn ""
