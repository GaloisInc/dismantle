module Main ( main ) where

import Control.Applicative
import Data.Maybe ( catMaybes, listToMaybe, maybeToList )
import Data.Monoid
import qualified Data.Set as S
import qualified Options.Applicative as O
import qualified Data.Text.Lazy.IO as TL
import qualified System.Exit as E

import Prelude

import qualified Dismantle.Tablegen as D

data Options = Options { inputFile :: FilePath
                       }
               deriving (Show)

options :: O.Parser Options
options = Options <$> O.strArgument ( O.metavar "FILE"
                                      <> O.help "The tablegen output to parse"
                                      )

main :: IO ()
main = O.execParser opts >>= dump
  where
    opts = O.info (O.helper <*> options) O.fullDesc

dump :: Options -> IO ()
dump o = do
  t <- TL.readFile (inputFile o)
  case D.parseTablegen (inputFile o) t of
    Left err -> do
      print err
      E.exitFailure
    Right r -> do
      let defs = D.tblDefs r
      putStrLn ("# Classes: " ++ show (length (D.tblClasses r)))
      putStrLn ("# Defs: " ++ show (length defs))
      let namespaces = S.fromList $ catMaybes [ namespace d | d <- defs ]
          decoders = S.fromList $ catMaybes [ decoderNamespace d | d <- defs ]
      putStrLn ("Namespaces: " ++ show (S.toList namespaces))
      putStrLn ("Decoders: " ++ show (S.toList decoders))

      let allARM = [ d
                   | d <- defs
                   , "ARM" <- maybeToList $ namespace d
                   ]
          armInsns = [ d
                     | d <- defs, "ARM" <- maybeToList $ decoderNamespace d
                     ]
          thumb1Insns = [ d
                        | d <- defs, "Thumb" <- maybeToList $ decoderNamespace d
                        ]
          thumb2Insns = [ d | d <- defs, "Thumb2" <- maybeToList $ decoderNamespace d ]
      putStrLn ("# Insns: " ++ show (length allARM))
      putStrLn ("# ARM Insns: " ++ show (length armInsns))
      putStrLn ("# Thumb1 Insns: " ++ show (length thumb1Insns))
      putStrLn ("# Thumb2 Insns: " ++ show (length thumb2Insns))

-- | If there are decls in the def for which the predicate is true, return them.
filterDecls :: (D.Named D.DeclItem -> Bool) -> D.Def -> [D.Named D.DeclItem]
filterDecls p = filter p . D.defDecls

-- | If there is a decl with the given name in the def, return it.
--
-- There should be at most one
lookupDecl :: String -> D.Def -> Maybe (D.Named D.DeclItem)
lookupDecl name = listToMaybe . filterDecls ((==name) . D.namedName)

declAsString :: D.DeclItem -> Maybe String
declAsString i =
  case i of
    D.StringItem s -> Just s
    _ -> Nothing

namespace :: D.Def -> Maybe String
namespace def = lookupDecl "Namespace" def >>= (declAsString . D.namedValue)

decoderNamespace :: D.Def -> Maybe String
decoderNamespace def = lookupDecl "DecoderNamespace" def >>= (declAsString . D.namedValue)

