-- |a module to preprocess macros
module Preprocess (preprocess, preprocessModule) where

import Control.Applicative ((<$>))
import Prelude hiding (mapM)
import Data.Traversable (mapM)
import Control.Monad.Trans.Reader (ask)
import qualified Control.Monad.Trans.Class  as MT (lift)
import qualified Control.Monad.Trans.Reader as MT
import qualified Control.Monad.Trans.Except as MT
import qualified Data.Functor.Identity as MT
import qualified Data.Map as M

import Utils
import AST
import Printer()

-- |evaluation type
type Preprocess a = MT.ReaderT Env (MT.ExceptT Error MT.Identity) (WithMD a)

-- |preprocess a program.
preprocessModule :: Module -> MT.ExceptT Error MT.Identity Module
preprocessModule modul = do
  let env = getModMacros modul
  macros <- mapM (mapM (\expr -> MT.runReaderT (preprocess expr) env)) $ M.toList (getModMacros modul)
  definitions <- mapM (mapM (\expr -> MT.runReaderT (preprocess expr) (M.fromList macros))) $ M.toList (getModEnv modul)
  return $ modul { getModEnv = M.fromList definitions, getModMacros = M.fromList macros }

-- |preprocess an expression.
preprocess :: WithMD Expr -> Preprocess Expr
preprocess exprWithMD@(WithMD md expr) =
  case expr of
    ATOM a      -> preprocessAtom exprWithMD a
    QUOTE l     -> return . WithMD md . QUOTE =<< return l
    LIST ls     -> preprocessOp exprWithMD ls
    _           -> throwErr (Just exprWithMD) "something went wrong here."

-- |preprocess an Atom.
preprocessAtom :: WithMD Expr -> Atom -> Preprocess Expr
preprocessAtom (WithMD md _) atom = do
  env <- ask
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
  env <- ask
  if name == "quote"
  then return exprWithMD
  else
    case M.lookup name env of
      -- |not in environment. preprocess all elements and return the list.
      Nothing -> do
        prepOp  <- return $ WithMD md $ ATOM $ Symbol name
        prepOps <- mapM preprocess operands
        return $ WithMD md $ LIST (prepOp:prepOps)
      -- |in environment and is in the form of a macro, preprocess macro.
      Just v@(WithMD _ (LIST (WithMD _ (LIST [WithMD _ (LIST _), _]):_)))  -> do
        prepOps <- mapM preprocess operands
        preprocessMacro v exprWithMD prepOps
      -- |in environment but is not in the form of a macro, preprocess rest and return the list.
      Just v -> WithMD md . LIST <$> mapM preprocess (v:operands)


-- |preprocess macro. tries to fit amount of arguments recursively.
preprocessMacro :: WithMD Expr -> WithMD Expr -> [WithMD Expr] -> Preprocess Expr
preprocessMacro macro rootExpr operands =
  case macro of
    WithMD md (LIST []) -> throwErr (Just rootExpr) $ "arity problem, defmacro at " ++ show md ++ "does not take " ++ show (length operands) ++ " arguments"
    WithMD md (LIST (WithMD _ (LIST [WithMD _ (LIST args), body]):rest)) ->
      if length operands /= length args
      -- |try next macro
      then preprocessMacro (WithMD md (LIST rest)) rootExpr operands
      -- |this macro fits. preprocess macro,
      else do
        -- |preprocess arguments
        preprocessedArgs <- mapM preprocess operands
        -- |get argument names
        argNames   <- mapM (MT.lift . exprToSymbolName) args
        -- |get arguments renamed with initial ';' as symbols
        argRenamed <- mapM (MT.lift . mapExprToSymbolName (';':)) args
        -- |extend environment by mapping arguments symbols from name to ;name
        renamedEnv <- return . M.union (M.fromList (zip argNames argRenamed)) =<< ask
        -- |replace arguments symbols from name to ;name in body of macro
        afterRename <- MT.withReaderT (const renamedEnv) (preprocess body)
        -- |extend environment with real arguments
        let renamedNames = map (';':) argNames
        extendedEnv <- return . M.union (M.fromList (zip renamedNames preprocessedArgs)) =<< ask
        -- |preprocess renamed macro body and replace argument names with argument expressions in it.
        MT.withReaderT (const extendedEnv) (preprocess afterRename)
    _ -> return rootExpr


  -- can use ';' for renaming


mapExprToSymbolName :: (Name -> Name) -> WithMD Expr -> MT.ExceptT Error MT.Identity (WithMD Expr)
mapExprToSymbolName f (WithMD md  (ATOM (Symbol name))) = return (WithMD md (ATOM (Symbol (f name))))
mapExprToSymbolName _ expr = MT.throwE $ Error (Just expr) "problem while trying to convert expression to symbol"

exprToSymbolName :: WithMD Expr -> MT.ExceptT Error MT.Identity Name
exprToSymbolName (WithMD _  (ATOM (Symbol name))) = return name
exprToSymbolName expr = MT.throwE $ Error (Just expr) "problem while trying to convert expression to symbol"
