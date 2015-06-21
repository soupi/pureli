
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- |
-- Create a module before evaluation
module Pureli.Module (loadModule, requireToMod) where

import Data.Maybe  (fromMaybe)
import Control.Applicative ((<$>))
import Control.Exception (IOException, catch)
import qualified Control.Monad.Trans.Class as MT
import qualified Control.Monad.Trans.Except as MT
import qualified Control.Monad.Trans.State.Strict as MT
import qualified Data.Map as M

import Pureli.AST
import Pureli.Utils
import Pureli.Parser
import Pureli.Printer()

import Paths_Pureli (getDataFileName)

-- |
-- read module definitions from file
readModules :: (MT.MonadTrans t, Monad (t IO)) =>FilePath -> MT.ExceptT Error (t IO) [WithMD ModuleDef]
readModules filepath = do
  result <- MT.lift $ MT.lift ((Right <$> readFile filepath)
              `catch` (\e -> (Right <$> (readFile =<< getDataFileName filepath))
                `catch` (\(_ :: IOException) -> return $ Left $ show (e :: IOException))))
  case result of
    Left _ ->
      MT.throwE (Error Nothing $ "Couldn't find file " ++ filepath)
    Right fileContent ->
      case parseFile filepath fileContent of
        Left err  -> MT.throwE (Error Nothing err)
        Right res -> return res

-- |
-- Read a source file and return the modules within
loadModule :: FilePath -> Name -> MT.ExceptT Error IO Module
loadModule filepath mName = do
  result <- MT.lift $ flip MT.evalStateT (M.fromList []) $ MT.runExceptT (requireToModule $ Require filepath mName Nothing Nothing)
  case result of
    Right x -> return x
    Left er -> MT.throwE er

-- |
-- converts a list of requires to a list of modules
requiresToModules :: [Require] -> MT.ExceptT Error (MT.StateT (M.Map (Name, Name) Module) IO) [Module]
requiresToModules = mapM cacheRequire

-- |
-- Will make sure there are no cyclic requires and will return a module if already required
cacheRequire :: Require -> MT.ExceptT Error (MT.StateT (M.Map (Name, Name) Module) IO) Module
cacheRequire modul@(Require mFile mName newName exposedDefs) =
  MT.lift MT.get >>= \mapping -> case M.lookup (mFile, mName) mapping of
    Just m  ->
      if getModName m == ";cycle"
        then
          MT.throwE $ Error Nothing $ "cyclic require on module " ++ mName
        else do
          let wantedDefs = case (\x -> (null x, x)) `fmap` exposedDefs of
                  Just (False, exDefs) -> M.fromList $ filter (\x -> fst x `elem` exDefs) (M.toList (getModEnv m))
                  _                         -> getModEnv m
          let modResult = m { getModExports = wantedDefs, getModName = fromMaybe mName newName }
          MT.lift $ MT.modify (M.insert (mFile, mName) (m { getModName = mName }))
          return modResult
    Nothing -> requireToModule modul


-- |
-- try to get a module from a file
lookupModule :: Name -> Name -> [WithMD ModuleDef] -> Either String ModuleDef
lookupModule mFile mName [] = Left $ "Couldn't find module " ++ mName ++ " in file " ++ mFile
lookupModule mFile mName (WithMD _ m:ms) =
  if   mFile == modFile m && mName == modName m
  then return m
  else lookupModule mFile mName ms

-- |converts a require to a module
requireToMod :: Require -> IO (Either Error Module)
requireToMod req = flip MT.evalStateT (M.fromList []) $ MT.runExceptT $ requireToModule req


-- |converts a require to a module
requireToModule :: Require -> MT.ExceptT Error (MT.StateT (M.Map (FilePath, Name) Module) IO) Module
requireToModule (Require filePath mName newName exposing) = do
  modul <- getModuleFromFile filePath mName
  env <- envIfNoDups mName (getModEnv modul)
  let wantedDefs = case exposing of
        Nothing          -> env
        Just definitions ->  M.fromList $ filter (\x -> fst x `elem` definitions) (M.toList env)
  let result = modul { getModName = fromMaybe mName newName, getModExports = wantedDefs }
  return result


-- |get and preprocess a specific module and all its requires from a file
getModuleFromFile :: FilePath -> Name -> MT.ExceptT Error (MT.StateT (M.Map (FilePath, Name) Module) IO) Module
getModuleFromFile fileName mName = do
  state <- MT.lift MT.get
  case M.lookup (fileName, mName) state of
    Just m  -> return m
    Nothing -> do
      modulDefs <- readModules fileName
      case lookupModule fileName mName modulDefs of
        Left err  -> MT.throwE (Error Nothing err)
        Right res -> do
          MT.lift $ MT.modify $ M.insert (fileName, mName) $ Module fileName ";cycle" [] (M.fromList []) (M.fromList [])
          reqMods <- requiresToModules (modRequires res)
          modul   <- case fromDefToModule reqMods res of
            Left err -> MT.throwE (Error Nothing err)
            Right x  -> MT.lift $ return x
          --let prp = MT.runIdentity $ MT.runExceptT $ preprocessModule modul
          --preprocessedModule <- case prp of
          --  Left err -> MT.throwE err
          --  Right rs -> return rs
          MT.lift MT.get >>= MT.lift . MT.put . M.insert (fileName, mName) modul --preprocessedModule
          return modul -- preprocessedModule


-- |
-- try to get a module from a ModuleDef
fromDefToModule :: [Module] -> ModuleDef -> Either String Module
fromDefToModule reqs def = do
  exposedDefs <-
    case modExposes def of
      Nothing      -> return (modDefs def)
      Just exposes -> which (modDefs def) exposes
  exportedDefs   <- filterListMap (map fst exposedDefs) (modDefs def)
  return
    Module { getModFile = modFile def
           , getModName = modName def
           , getModImports = reqs
           , getModExports = M.fromList exportedDefs
           , getModEnv     = M.fromList $ modDefs def
           }

-- |
-- filter assoc list from list
filterListMap :: (Show a, Eq a) => [a] -> [(a, b)] -> Either String [(a,b)]
filterListMap []  _ = return []
filterListMap _  [] = return []
filterListMap (a:as) list = case lookup a list of
  Nothing -> Left $ "Could not find definition to expose: " ++ show a
  Just x  -> return . ((a,x):) =<< filterListMap as list

-- |
-- filter two assoc map from list
which :: Eq a => [(a,b)] -> [a] -> Either a [(a,b)]
which defs = mapM f
  where f x = maybeToEither x ((,) x <$> lookup x defs)

-- |
-- converts a list of (Name, WithMD Expr) to Env. fails if there are duplicate names
envIfNoDups :: Monad m => Name -> Env -> MT.ExceptT Error m Env
envIfNoDups mName env = do
  let dups = duplicates $ map fst (M.toList env)
  if not (null dups)
  then MT.throwE $ Error Nothing $ "Duplicate definitions in module: " ++ mName ++ "\n*** " ++ show dups
  else return env


