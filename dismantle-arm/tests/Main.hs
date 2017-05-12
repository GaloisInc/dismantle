module Main ( main ) where

import qualified Data.List as L
import qualified Test.Tasty as T
import qualified Text.RE.TDFA as RE

import Dismantle.Testing

import qualified Dismantle.ARM as ARM

arm :: ArchTestConfig
arm = ATC { archName = "arm"
          , disassemble = ARM.disassembleInstruction
          , assemble = ARM.assembleInstruction
          , prettyPrint = ARM.ppInstruction
          , expectFailure = Just expectedFailures
          , skipPrettyCheck = Just (rx ".*")
          }

main :: IO ()
main = do
  tg <- binaryTestSuite arm "tests/bin"
  T.defaultMain tg

rx :: String -> RE.RE
rx s =
  case RE.compileRegex s of
    Nothing -> error ("Invalid regex: " ++ s)
    Just r -> r

expectedFailures :: RE.RE
expectedFailures = rx (L.intercalate "|" rxes)
  where
    rxes = [ -- The tablegen data for MVN is currently incorrect
             -- w.r.t. unpredictable bits. The only variant of
             -- MVN that we've encountered that actually sets
             -- some unpredictable bits is 'mvnpl'. A bug has been
             -- submitted upstream. In the mean time, for details, see
             -- https://bugs.llvm.org/show_bug.cgi?id=33011
             "^[[:space:]]*mvnpl"
           ]
