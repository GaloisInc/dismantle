module Main where

import Control.Monad (forM, forM_)
import Data.Monoid ((<>))
import Data.Maybe (catMaybes)
import Data.List (find, nub, sort)
import Data.List.Split (splitOn)
import qualified Data.Map as M
import Dismantle.Tablegen.Parser
import Dismantle.Tablegen.Parser.Types
import qualified Data.Text.Lazy.IO as TL
import System.Exit

tgenFile :: FilePath
tgenFile = "data/ARM.tgen"

targetNamespace :: String
targetNamespace = "ARM"

isDecl :: String -> Named DeclItem -> Bool
isDecl n (Named n2 _) = n == n2

pseudoPropName :: String
pseudoPropName = "isPseudo"

isPsuedoClass :: ClassDecl -> Bool
isPsuedoClass cls =
    case find (isDecl pseudoPropName) $ classDecls cls of
        Just (Named _ (BitItem v)) -> v
        _ -> False

isPsuedoDef :: Def -> Bool
isPsuedoDef def =
    case find (isDecl pseudoPropName) $ defDecls def of
        Just (Named _ (BitItem v)) -> v
        _ -> False

hasDecl :: String -> DeclItem -> [Named DeclItem] -> Bool
hasDecl name value ds =
    case find (isDecl name) ds of
        Just (Named _ v) -> value == v
        _ -> False

findByName :: String -> [Named DeclItem] -> Maybe DeclItem
findByName n es = namedValue <$> find (isDecl n) es

getIdentifiers :: DeclItem -> [String]
getIdentifiers (DagItem sv) = getIdentifiersSV sv
getIdentifiers (ListItem is) = concat $ getIdentifiers <$> is
getIdentifiers (ExprItem sv) = getIdentifiersSV sv
getIdentifiers (BitItem _) = []
getIdentifiers (IntItem _) = []
getIdentifiers (StringItem _) = []
getIdentifiers (StringExprItem _) = []
getIdentifiers (FieldBits _) = []
getIdentifiers (ExpectedBits _) = []
getIdentifiers (ExpectedUnknownBits _) = []
getIdentifiers (ClassItem _) = []
getIdentifiers (UnknownItem _) = []

getIdentifiersSV :: SimpleValue -> [String]
getIdentifiersSV (Identifier s) = [s]
getIdentifiersSV (VList svs _) = concat $ getIdentifiersSV <$> svs
getIdentifiersSV (VSequence svs) = concat $ getIdentifiersSV <$> svs
getIdentifiersSV (VAnonRecord _ svs) = concat $ getIdentifiersSV <$> svs
getIdentifiersSV (VBang _ _ svs) = concat $ getIdentifiersSV <$> svs
getIdentifiersSV (VDag _ args) = concat $ getIdentifiersDA <$> args
getIdentifiersSV (VString _) = []
getIdentifiersSV (VNum _) = []
getIdentifiersSV VUnset = []

getIdentifiersDA :: DagArg -> [String]
getIdentifiersDA (DagArg sv _) = getIdentifiersSV sv
getIdentifiersDA (DagVarRef _) = []

data OperandType = Concrete String
                 | Indirect String String
                 deriving (Show, Eq, Ord)

forClass :: String -> OperandType -> Bool
forClass s (Indirect n _) = s == n
forClass _ _ = False

getTypes :: [String] -> [OperandType]
getTypes ss = catMaybes $ getType <$> ss
    where
        getType "variable_ops" = Nothing
        getType s =
            case splitOn ":" s of
                [_, "iops"]    -> Nothing
                [_, "oops"]    -> Nothing
                [ty, _]        -> Just $ Concrete ty
                [cls, attr, _] -> Just $ Indirect cls attr
                _              -> error $ "Invalid operand type string: " <> show s

main :: IO ()
main = do
    content <- TL.readFile tgenFile
    case parseTablegen tgenFile content of
        Left e -> do
            putStrLn $ "Error: " <> show e
            exitFailure
        Right result -> do
            -- Create a map of class name to class so we can do
            -- attribute lookups.
            let clsMap = M.fromList $ (\c -> (classDeclName c, c)) <$> (tblClasses result)

            clsTys <- forM (tblClasses result) $ \cls -> do
                case (not $ isPsuedoClass cls) && hasDecl "DecoderNamespace" (StringItem "ARM") (classDecls cls) of
                    False -> return []
                    True -> do
                        let inOps = findByName "InOperandList" $ classDecls cls
                            outOps = findByName "OutOperandList" $ classDecls cls

                        i <- case inOps of
                            Nothing -> return []
                            Just io -> return $ getTypes $ getIdentifiers io

                        o <- case outOps of
                            Nothing -> return []
                            Just oo -> return $ getTypes $ getIdentifiers oo

                        -- Drop any identifiers that belong to the
                        -- current class
                        return $ filter (not . forClass (classDeclName cls)) $ i <> o

            defTys <- forM (tblDefs result) $ \def ->
                case (not $ isPsuedoDef def) && hasDecl "DecoderNamespace" (StringItem "ARM") (defDecls def) of
                    False -> return []
                    True -> do
                        let inOps = findByName "InOperandList" $ defDecls def
                            outOps = findByName "OutOperandList" $ defDecls def

                        i <- case inOps of
                            Nothing -> return []
                            Just io -> return $ getTypes $ getIdentifiers io

                        o <- case outOps of
                            Nothing -> return []
                            Just oo -> return $ getTypes $ getIdentifiers oo

                        -- Drop any identifiers that belong to the
                        -- current def
                        return $ filter (not . forClass (defName def)) $ i <> o

            let allTypes = sort $ nub $ concat defTys <> concat clsTys

            forM_ allTypes $ \ty ->
                case ty of
                    Concrete cty -> putStrLn cty
                    Indirect clsName attrName ->
                        -- Do a lookup
                        case M.lookup clsName clsMap of
                            Nothing -> do
                                die $ "Could not find class " <> show clsName <> " when resolving " <> show ty
                            Just found -> do
                                print $ findByName attrName $ classDecls found
