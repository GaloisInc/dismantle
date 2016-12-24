module Dismantle.Tablegen.Combinators (
  filterDecls,
  lookupDecl,
  declAsString,
  namespace,
  decoderNamespace
  ) where

import Data.Maybe ( listToMaybe )

import Dismantle.Tablegen.Parser.Types

-- | If there are decls in the def for which the predicate is true, return them.
filterDecls :: (Named DeclItem -> Bool) -> Def -> [Named DeclItem]
filterDecls p = filter p . defDecls

-- | If there is a decl with the given name in the def, return it.
--
-- There should be at most one
lookupDecl :: String -> Def -> Maybe (Named DeclItem)
lookupDecl name = listToMaybe . filterDecls ((==name) . namedName)

declAsString :: DeclItem -> Maybe String
declAsString i =
  case i of
    StringItem s -> Just s
    _ -> Nothing

namespace :: Def -> Maybe String
namespace def = lookupDecl "Namespace" def >>= (declAsString . namedValue)

decoderNamespace :: Def -> Maybe String
decoderNamespace def =
  lookupDecl "DecoderNamespace" def >>= (declAsString . namedValue)




