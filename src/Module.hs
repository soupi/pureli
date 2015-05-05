{-
 ================
    NOT READY
 ================
 -}


{-# LANGUAGE LambdaCase #-}

-- |
-- Create a module before evaluation
module Module where

import Data.Maybe  (fromMaybe)
import Data.Either (partitionEithers)
import Control.Applicative ((<$>),(<*>),(<|>))
import qualified Control.Monad.Trans.Class as MT
import qualified Control.Monad.Trans.Except as MT
import qualified Control.Monad.Trans.State.Strict as MT
import qualified Data.Functor.Identity as MT
import qualified Data.Map as M

import AST
import Utils
import Parser
import Preprocess


-- |
-- read module definitions from file
readModules :: FilePath -> MT.ExceptT Error IO [WithMD ModuleDef]
readModules filepath = do
  fileContent <- MT.lift (readFile filepath)
                 `MT.catchE`
                 \_ -> MT.throwE (Error Nothing $ "Couldn't find file " ++ filepath)
  case parseFile filepath fileContent of
    Left err  -> MT.throwE (Error Nothing err)
    Right res -> return res

-------------------------------------------------------------

-- |
-- Read a source file and return the modules within
loadModules :: FilePath -> MT.ExceptT Error IO Module
loadModules filepath = do
  result <- MT.lift $ flip MT.evalStateT (M.fromList []) $ MT.runExceptT (requireToModule $ Require filepath [] Nothing Nothing)
  case result of
    Right x -> return x
    Left er -> MT.throwE er

-- |
-- converts a list of requires to a list of modules
requiresToModules :: [Require] -> MT.ExceptT Error (MT.StateT (M.Map (Name, Name) Module) IO) [Module]
requiresToModules requires =
  mapM cacheRequire requires

cacheRequire :: Require -> MT.ExceptT Error (MT.StateT (M.Map (Name, Name) Module) IO) Module
cacheRequire modul@(Require mFile mName newName exposedDefs) = do
  MT.lift MT.get >>= \mapping -> case M.lookup (mFile, mName) mapping of
    Just m  -> do
      if getModName m == ";cycle"
        then
          MT.throwE $ Error Nothing $ "cyclic require on module " ++ mName
        else do
          let wantedDefs = case (\x -> (x == [], x)) `fmap` exposedDefs of
                  Just (False, exposedDefs) -> M.fromList $ filter (\x -> fst x `elem` exposedDefs) (M.toList (getModEnv m))
                  _                         -> getModEnv m
          let modResult = m { getModExports = wantedDefs, getModName = (fromMaybe mName newName) }
          MT.lift $ MT.modify (M.insert (mFile, mName) modResult)
          return modResult
    Nothing -> do
      MT.lift $ MT.modify $ M.insert (mFile, mName) $ Module mFile ";cycle" [] (M.fromList []) (M.fromList []) (M.fromList []) (M.fromList [])
      modResult <- requireToModule modul
      -- test test test
      MT.lift $ MT.modify (M.insert (mFile, mName) modResult)
      return $ modResult


lookupModule :: Name -> Name -> [WithMD ModuleDef] -> Either String ModuleDef
lookupModule mFile mName [] = Left $ "Couldn't find module " ++ mName ++ " in file " ++ mFile
lookupModule mFile mName (WithMD _ m:ms) =
  if   mFile == modFile m && mName == modName m
  then return m
  else lookupModule mFile mName ms


-- |converts a require to a module
requireToModule :: Require -> MT.ExceptT Error (MT.StateT (M.Map (Name, Name) Module) IO) Module
requireToModule (Require fileName modName definitions newName) = do
  moduleContent <- MT.lift (MT.lift (readFile modName)) `MT.catchE` \_ -> MT.throwE (Error Nothing $ "Couldn't find file " ++ modName)
  case (lookupModule fileName modName) =<< (parseFile fileName moduleContent) of
    Left err  -> MT.throwE (Error Nothing err)
    Right res -> do
      reqModDefs <- requiresToModules (modRequires res)
      let reqMods = fromDefToModule reqModDefs
      let prp = MT.runIdentity $ MT.runExceptT $ preprocessModule res
      preprocessedModule <- case prp of
        Left err -> MT.throwE err
        Right rs -> return rs
      env <- envIfNoDups modName (getModEnv preprocessedModule)
      let wantedDefs = if definitions == []
          then env
          else M.fromList $ filter (\x -> fst x `elem` definitions) (M.toList env)
      let modResult = Module (fromMaybe modName newName) reqMods wantedDefs env
      return $ modResult


fromDefToModule :: ModuleDef -> Either Name Module
fromDefToModule def = do
  (exposedDefs, exposedMacros) <-
    case modExposes def of
      Nothing      -> return (modDefs def, modMacros def)
      Just exposes -> partitionEithers <$> which (modDefs def) (modMacros def) exposes
  exportedDefs   <- filterListMap (map fst exposedDefs) (modDefs def)
  exportedMacros <- filterListMap (map fst exposedMacros) (modMacros def)
  return $
    Module { getModFile = modFile def
           , getModName = modName def
           , getModImports = []
           , getModExports        = M.fromList exportedDefs
           , getModExportedMacros = M.fromList exportedMacros
           , getModMacros  = M.fromList $ modMacros def
           , getModEnv     = M.fromList $ modDefs def
           }

filterListMap :: (Show a, Eq a) => [a] -> [(a, b)] -> Either String [(a,b)]
filterListMap [] _  = return []
filterListMap _  [] = return []
filterListMap (a:as) list = case lookup a list of
  Nothing -> Left $ "Could not find definition to expose: " ++ show a
  Just x  -> return . ((a,x):) =<< filterListMap as list


which :: Eq a => [(a,b)] -> [(a,b)] -> [a] -> Either a [Either (a,b) (a,b)]
which list1 list2 xs = mapM f xs
  where f x = case maybeToEither x (lookup x list1 >>= return . Left . (,) x) of
                Right r -> Right r
                Left  _ -> maybeToEither x (lookup x list2 >>= Just . Right . (,) x)

-- |
-- converts a list of (Name, WithMD Expr) to Env. fails if there are duplicate names
envIfNoDups :: Monad m => Name -> Env -> MT.ExceptT Error m Env
envIfNoDups modName env = do
  let dups = duplicates $ map fst (M.toList env)
  if length dups > 0
  then MT.throwE $ Error Nothing $ "Duplicate definitions in module: " ++ modName ++ "\n*** " ++ show dups
  else return $ env


