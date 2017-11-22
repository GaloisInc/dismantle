module Main ( main ) where

import Control.Applicative
-- import Data.Maybe ( catMaybes, maybeToList )
import Data.Monoid
-- import qualified Data.Set as S
import qualified Options.Applicative as O
import qualified Data.Text.Lazy.IO as TL
import qualified System.Exit as IO
import qualified System.IO as IO

import Prelude

import qualified Dismantle.Tablegen as D

data Options = Options { inputFile :: FilePath
                       , parseISA :: ISATag
                       }
               deriving (Show)

data ISATag = ARM
            | AVR
            | AArch64
            | Thumb
            | PPC
            | Sparc
            | Mips
            deriving (Eq, Ord, Read, Show)

options :: O.Parser Options
options = Options <$> O.strArgument ( O.metavar "FILE"
                                      <> O.help "The tablegen output to parse"
                                      )
                  <*> O.option O.auto ( O.metavar "ISA"
                                      <> O.short 'i'
                                      <> O.long "isa"
                                      <> O.help "ISA to parse the input file as"
                                      )

lookupISA :: ISATag -> D.ISA
lookupISA t =
  case t of
    ARM -> D.arm
    AVR -> D.avr
    AArch64 -> D.aarch64
    Thumb -> D.thumb
    PPC -> D.ppc
    Sparc -> D.sparc
    Mips -> D.mips

main :: IO ()
main = O.execParser opts >>= dump
  where
    opts = O.info (O.helper <*> options) O.fullDesc

dump :: Options -> IO ()
dump o = do
  let p = inputFile o
  t <- TL.readFile p
  case D.parseTablegen p t of
    Left err -> do
      IO.hPutStrLn IO.stderr ("Error: " ++ err)
      IO.exitFailure
    Right defs -> do
      let isa = lookupISA (parseISA o)
      let summary = D.filterISA isa defs
      putStrLn ("ISA: " ++ D.isaName isa)
      putStrLn ("# Instructions: " ++ show (length (D.isaInstructions summary)))
      putStrLn "Operand types"
      mapM_ (putStrLn . ("  "++) . show) (D.isaOperands summary)
      putStrLn "Instruction mnemonics"
      mapM_ (putStrLn . ("  "++) . D.idMnemonic) (D.isaInstructions summary)
