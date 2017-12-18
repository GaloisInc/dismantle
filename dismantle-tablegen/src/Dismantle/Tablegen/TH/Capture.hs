{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
module Dismantle.Tablegen.TH.Capture (
  captureDictionaries,
  CaptureInfo(..),
  captureInfo
  ) where

import Data.Char ( toLower )
import qualified Data.Functor.Const as C
import Language.Haskell.TH
import Language.Haskell.TH.Datatype
import Language.Haskell.TH.Syntax
import Text.Printf ( printf )

import qualified Data.Parameterized.List as SL
import qualified Data.Parameterized.Some as S
import Dismantle.Tablegen.TH.CaptureInfo ( CaptureInfo(..) )

-- | For the named data type, generate a list of witnesses for that datatype
-- that capture a dictionary for each constructor of that datatype.  The
-- predicate is a filter that enables control over which constructors are
-- captured.  The predicate is matched against the unqualified constructor name.
--
-- The intended use is to capture witnesses of different operand shape lists.
--
-- Example:
--
-- > captureDictionary (const True) ''Operand
--
-- will generate an expression the type
--
-- > [Some Opcode]
--
--
-- The class for which dictionaries are captured is set by a type signature
-- specified for the expression at the call site by the caller.  Note that
-- 'Opcode' must have kind '[k] -> *' and the class must have kind '[k] ->
-- Constraint'.
captureDictionaries :: (String -> Bool) -> Name -> ExpQ
captureDictionaries p tyName = do
  dti <- reifyDatatype tyName
  let cons = filter (p . unqualifiedName . constructorName) (datatypeCons dti)
  listE (map captureDictionaryFor cons)
  where
    unqualifiedName (Name (OccName s) _) = s

captureDictionaryFor :: ConstructorInfo -> ExpQ
captureDictionaryFor ci = [e| S.Some $(conE (constructorName ci)) |]

-- | This is just like 'captureDictionaries', but it captures a bit more
-- information including some 'Name's and generates a TH function to perform a
-- match for each constructor's operand list.
captureInfo :: (String -> Bool) -> Name -> ExpQ
captureInfo p tyName = do
  dti <- reifyDatatype tyName
  let cons = filter (p . unqualifiedName . constructorName) (datatypeCons dti)
  listE (map captureInfoFor cons)
  where
    unqualifiedName (Name (OccName s) _) = s

captureInfoFor :: ConstructorInfo -> ExpQ
captureInfoFor ci = do
  sh <- constructorShape ci
  (names, namesE) <- allocateMatchNames sh
  let genInputE = listE [ [| ($(litE (StringL s)), mkName $(litE (StringL (show n)))) |] | (s, n) <- names]
  [e| S.Some CaptureInfo { capturedOpcode = $(conE (constructorName ci))
                         , capturedOpcodeName = mkName $(litE (StringL (show (constructorName ci))))
                         , capturedOperandNames = $(return namesE)
                         , genCase = genMatchExpr $(genInputE)
                         }
    |]

-- | Generate a case expression over the operand list with a single case, where
-- the shape is given as the first parameter.  The body of the match is also
-- provided as an argument.
genMatchExpr :: [(String, Name)]
             -- ^ The first element of the pair is the name of the operand
             -- constructor
             --
             -- The second is the name that is to be bound to the operand in the
             -- case
             -> Name
             -- ^ The name of the operand list that will be in scope
             -> Exp
             -- ^ The body to put into the match
             -> Q Exp
genMatchExpr operands operandListName body = do
  let m = match (buildPattern operands) (normalB (return body)) []
  caseE (varE operandListName) [m]
  where
    buildPattern :: [(String, Name)] -> PatQ
    buildPattern ops =
      case ops of
        [] -> [p| SL.Nil |]
        ((opConStr, operandName) : rest) -> do
          let patRest = buildPattern rest
          let p = ConP (mkName opConStr) [VarP operandName]
          [p| $(return p) SL.:< $(patRest) |]

allocateMatchNames :: [String] -> Q ([(String, Name)], Exp)
allocateMatchNames sh = do
  -- We mock up names instead of using 'newName' to ensure that our
  -- serialization of names into strings doesn't break name resolution.  We have
  -- to embed the names as calls to 'mkName'.
  let names = map (\(ix :: Int, s) ->
                     -- We have to downcase the name so that it is a valid
                     -- variable
                     let nameToBind = map toLower (s ++ show ix)
                     in (s, mkName nameToBind)) (zip [0..] sh)
  e <- buildShapedList (map snd names)
  return (names, e)
  where
    buildShapedList names =
      case names of
        [] -> [| SL.Nil |]
        (name:rest) -> do
          restE <- buildShapedList rest
          [| C.Const (mkName $(litE (StringL (show name)))) SL.:< $(return restE) |]

-- | Examine a 'ConstructorInfo' for an opcode constructor (which is a
-- type-level list of symbols) and return the shape as a list of value level
-- strings.  By convention, these strings are also the names of the operand
-- wrappers (e.g., Gprc), so we can use them to construct a TH fragment to match
-- on operands.
--
-- This function will cause a compile error if it can't figure out the shape, so
-- be sure to only call it on constructors with the expected structure.
constructorShape :: ConstructorInfo -> Q [String]
constructorShape ci =
  -- The context is a list of types (which we expect to be type equalities).
  -- One of those reflects the shape.
  tryMatchShape (constructorContext ci)
  where
    tryMatchShape [] =
      fail (printf "Failed to find shape parameter for constructor %s" (show (constructorName ci)))
    tryMatchShape (t:ts) =
      -- There are two parameters we expect: the operand type equality and the
      -- shape type equality.
      case t of
        AppT (AppT EqualityT (VarT _var)) (ConT _) -> tryMatchShape ts
        AppT (AppT EqualityT (VarT _var)) shape ->
          deconstructShape shape
        _ -> fail (printf "Unrecognized constraint structure: %s" (show t))

deconstructShape :: Type -> Q [String]
deconstructShape t =
  case t of
    AppT PromotedConsT (LitT (StrTyLit s)) -> return [s]
    SigT t' _k -> deconstructShape t'
    PromotedNilT -> return []
    AppT hd tl -> do
      hdType <- deconstructShape hd
      tlTypes <- deconstructShape tl
      return (hdType ++ tlTypes)
    _ -> fail ("Unexpected type structure: " ++ show t)
