-- |a module to preprocess macros
module Pureli.Preprocess (preprocess, preprocessModule) where

import Control.Applicative ((<$>))
import Prelude hiding (mapM)
import Data.Traversable (mapM)
import Control.Monad.Trans.Reader (ask)
import qualified Control.Monad.Trans.Class  as MT (lift)
import qualified Control.Monad.Trans.Reader as MT
import qualified Control.Monad.Trans.Except as MT
import qualified Data.Functor.Identity as MT
import qualified Data.Map as M
import qualified Data.Foldable as F (concatMap)

import Pureli.Utils
import Pureli.AST
import Pureli.Printer()


import Debug.Trace

-- |evaluation type
type Preprocess a = MT.ReaderT (Env, Module) (MT.ExceptT Error MT.Identity) (WithMD a)

getEnv :: Monad m => MT.ReaderT (Env, Module) m Env
getEnv = ask >>= return . fst

getMod :: Monad m => MT.ReaderT (Env, Module) m Module
getMod = ask >>= return . snd

wrapInClosureCall :: WithMD Expr -> Preprocess Expr
wrapInClosureCall (WithMD md expr) = do
  --modul <- getMod
  return $ WithMD md expr
  --return $ WithMD md $ LIST [WithMD md $ PROCEDURE $ Closure modul $ WithMD md $ Fun (FunArgs [] Nothing) expr]

-- |preprocess a module.
preprocessModule :: Module -> MT.ExceptT Error MT.Identity Module
preprocessModule modul = do
  let env    = getModMacros modul
      menv   = F.concatMap (\m -> fmap (\(n,e) -> (getModName m ++ "/" ++ n, e)) (M.toList $ getModExportedMacros m)) (getModImports modul)
  let macros = M.toList env ++ menv
  definitions <- mapM (mapM (\expr -> MT.runReaderT (preprocess expr) (M.fromList macros, modul))) $ M.toList (getModEnv modul)
  return $ modul { getModEnv = M.fromList definitions, getModMacros = M.fromList macros }

-- |preprocess an expression.
preprocess :: WithMD Expr -> Preprocess Expr
preprocess exprWithMD@(WithMD md expr) =
  case expr of
    ATOM a  -> preprocessAtom exprWithMD a
    QUOTE l -> return . WithMD md . QUOTE =<< return l
    LIST ls -> preprocessOp exprWithMD ls
    other   -> return $ WithMD md other

-- |preprocess an Atom.
preprocessAtom :: WithMD Expr -> Atom -> Preprocess Expr
preprocessAtom (WithMD md _) atom = do
  env <- getEnv
  case atom of
    Symbol var -> case M.lookup var env of
      Just v  -> return v
      Nothing -> return $ WithMD md $ ATOM $ Symbol var
    other -> return $ WithMD md (ATOM other)

-- |preprocess an procedure call. might be a macro call.
preprocessOp :: WithMD Expr -> [WithMD Expr] -> Preprocess Expr
preprocessOp exprWithMD [] = return exprWithMD
-- |separate operator from operands
preprocessOp exprWithMD@(WithMD md _) (operatorWithMD@(WithMD _ operator):operands) =
  case operator of
    -- |might be a macro call
    (ATOM (Symbol s)) -> preprocessOpSymbol exprWithMD operands s
    -- |not a macro call. preprocess all elements and return the list.
    _ -> do
      prepOp  <- preprocess operatorWithMD
      prepOps <- mapM preprocess operands
      return $ WithMD md $ LIST (prepOp:prepOps)

-- |check if name is in the environment.
preprocessOpSymbol :: WithMD Expr -> [WithMD Expr] -> Name -> Preprocess Expr
preprocessOpSymbol exprWithMD@(WithMD md _) operands name = do
  env <- getEnv
  if name == "mquote"
  then return exprWithMD
  else
    case M.lookup name env of
      -- |not in environment. preprocess all elements and return the list.
      Nothing -> do
        let prepOp = WithMD md $ ATOM $ Symbol name
        prepOps   <- mapM preprocess operands
        return $ WithMD md $ LIST (prepOp:prepOps)
      -- |in environment and is in the form of a macro, preprocess macro.
      Just v@(WithMD _ (LIST (WithMD _ (LIST [WithMD _ (LIST _), _]):_)))  -> do
        prepOps <- mapM preprocess operands
        preprocessMacroRest v exprWithMD prepOps
      -- |in environment but is not in the form of a macro, preprocess rest and return the list.
      Just v -> WithMD md . LIST <$> mapM preprocess (v:operands)


-- |preprocess macro. tries to fit amount of arguments recursively.
preprocessMacroRest :: WithMD Expr -> WithMD Expr -> [WithMD Expr] -> Preprocess Expr
preprocessMacroRest macro rootExpr operands = do
  env <- getEnv
  case operands of
    [WithMD _ (ATOM (Symbol ('&':var)))] ->
      case M.lookup (';':var) env of
        Just (WithMD _ (LIST list)) -> preprocessMacro macro rootExpr list
        _ -> preprocessMacro macro rootExpr operands
    _-> preprocessMacro macro rootExpr operands


-- |preprocess macro. tries to fit amount of arguments recursively.
preprocessMacro :: WithMD Expr -> WithMD Expr -> [WithMD Expr] -> Preprocess Expr
preprocessMacro macro rootExpr operands =
  case macro of
    WithMD md (LIST []) -> throwErr (Just rootExpr) $ "arity problem, defmacro at " ++ show md ++ "does not take " ++ show (length operands) ++ " arguments"
    WithMD md (LIST (WithMD _ (LIST [WithMD smd (ATOM (Symbol args)), body]):_)) -> -- |^argument is a list
      do
        modul <- getMod
        -- |preprocess arguments
        preprocessedArgs <- mapM preprocess operands
        -- |get argument name
        let argName = args
        -- |get argument renamed with initial ';' as symbols
        argRenamed <- (MT.lift . mapExprToSymbolName (';':)) (WithMD smd (ATOM (Symbol args)))
        -- |extend environment by mapping arguments symbols from name to ;name
        renamedEnv <- return . M.union (M.fromList [(argName, argRenamed)]) =<< getEnv
        -- |replace arguments symbols from name to ;name in body of macro
        afterRename <- MT.withReaderT (const (renamedEnv, modul)) (preprocess body)
        -- |extend environment with real arguments
        let renamedName = (';':) argName
        extendedEnv <- return . M.union (M.fromList [(renamedName, WithMD md $ QUOTE $ WithMD md $ LIST preprocessedArgs)]) =<< getEnv
        -- |preprocess renamed macro body and replace argument names with argument expressions in it.
        MT.withReaderT (const (extendedEnv, modul)) (preprocess afterRename) >>= wrapInClosureCall
    WithMD md (LIST (WithMD _ (LIST [WithMD _ (LIST args), body]):rest)) -> do
      modul <- getMod
      -- |get argument names
      aNames   <- mapM (MT.lift . exprToSymbolName) args
      case hasRest aNames aNames of
        (argNames, Nothing) ->
          if length operands /= length args
          -- |try next macro
          then preprocessMacro (WithMD md (LIST rest)) rootExpr operands
          -- |this macro fits. preprocess macro,
          else do
            -- |preprocess arguments
            preprocessedArgs <- mapM preprocess operands
            -- |get arguments renamed with initial ';' as symbols
            argRenamed <- mapM (MT.lift . mapExprToSymbolName (';':)) args
            -- |extend environment by mapping arguments symbols from name to ;name
            renamedEnv <- return . M.union (M.fromList (zip argNames argRenamed)) =<< getEnv
            -- |replace arguments symbols from name to ;name in body of macro
            afterRename <- MT.withReaderT (const (renamedEnv, modul)) (preprocess body)
            -- |extend environment with real arguments
            let renamedNames = map (';':) argNames
            extendedEnv <- return . M.union (M.fromList (zip renamedNames preprocessedArgs)) =<< getEnv
            -- |preprocess renamed macro body and replace argument names with argument expressions in it.
            MT.withReaderT (const (extendedEnv, modul)) (preprocess afterRename) >>= wrapInClosureCall
        (argNames, Just rarg) ->
          if length operands +1 < length args
          -- |try next macro
          then preprocessMacro (WithMD md (LIST rest)) rootExpr operands >>= wrapInClosureCall
          -- |this macro fits. preprocess macro,
          else do
            -- |preprocess arguments
            preprocessedArgs <- mapM preprocess operands
            -- |get arguments renamed with initial ';' as symbols
            argRenamed <- mapM (MT.lift . mapExprToSymbolName (';':)) args
            -- |extend environment by mapping arguments symbols from name to ;name
            let renamedEnv = M.fromList (zip argNames argRenamed)
            -- |replace arguments symbols from name to ;name in body of macro
            afterRename <- MT.withReaderT (const (renamedEnv, modul)) (preprocess body)
            -- |extend environment with real arguments
            let renamedNames = map (';':) argNames
            let firstZip  = zip (init renamedNames) preprocessedArgs
            let secondZip = (';':rarg, WithMD md $ LIST (drop (length (init renamedNames)) preprocessedArgs))
            extendedEnv <- return . M.union (M.fromList (firstZip ++ [secondZip])) =<< getEnv
            -- |preprocess renamed macro body and replace argument names with argument expressions in it.
            MT.withReaderT (const (extendedEnv, modul)) (preprocess afterRename) >>= wrapInClosureCall
    _ -> return rootExpr

hasRest :: [Name] -> [Name] -> ([Name], Maybe Name)
hasRest lst ['&':rest] = (init lst ++ [rest], Just rest)
hasRest lst []         = (lst, Nothing)
hasRest lst (_:xs)     = hasRest lst xs

mapExprToSymbolName :: (Name -> Name) -> WithMD Expr -> MT.ExceptT Error MT.Identity (WithMD Expr)
mapExprToSymbolName f (WithMD md  (ATOM (Symbol ('&':name)))) = return (WithMD md (ATOM (Symbol (f name))))
mapExprToSymbolName f (WithMD md  (ATOM (Symbol name))) = return (WithMD md (ATOM (Symbol (f name))))
mapExprToSymbolName _ expr = MT.throwE $ Error (Just expr) "problem while trying to convert expression to symbol"

exprToSymbolName :: WithMD Expr -> MT.ExceptT Error MT.Identity Name
exprToSymbolName (WithMD _  (ATOM (Symbol name))) = return name
exprToSymbolName expr = MT.throwE $ Error (Just expr) "problem while trying to convert expression to symbol"
