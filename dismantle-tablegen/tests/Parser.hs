module Parser ( parserTests ) where

import Control.DeepSeq ( deepseq )
import qualified Data.Text.Lazy.IO as TL
import qualified Test.Tasty as T
import qualified Test.Tasty.HUnit as T

import qualified Dismantle.Tablegen as D

parserTests :: IO T.TestTree
parserTests = do
  return (T.testGroup "Parser" (map mkTest isaTable))

mkTest :: (FilePath, D.ISA) -> T.TestTree
mkTest (p, isa) = T.testCase p $ do
  t <- TL.readFile p
  case D.parseTablegen p t of
    Left err -> T.assertFailure (show err)
    Right rs -> do
      let insns = D.filterISA isa rs
      insns `deepseq` return ()

isaTable :: [(FilePath, D.ISA)]
isaTable = [ ("data/AArch64.tgen", D.aarch64)
           , ("data/ARM.tgen", D.arm)
           , ("data/ARM.tgen", D.thumb)
           , ("data/AVR.tgen", D.avr)
           , ("data/Mips.tgen", D.mips)
           , ("data/PPC.tgen", D.ppc)
           , ("data/Sparc.tgen", D.sparc)
           ]
