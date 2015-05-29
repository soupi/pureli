
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}

module Pureli.Eval (initEvalState, addToEnv, addImport, replModule, evalExpr, evalModule) where

import Control.Exception (IOException, catch)
import Data.List (find)
import Control.Monad.Trans.Class  (lift)
import qualified Control.Monad.Trans.Reader as MT
import Control.Monad.Trans.Reader (ask)
import qualified Control.Monad.Trans.Except as MT
import Control.Monad.Trans.Except (throwE)
import qualified Data.Functor.Identity as MT
import Data.Function (on)
import qualified Data.Map as M
import qualified Control.Parallel.Strategies as P

import Debug.Trace

import Pureli.Utils
import Pureli.AST
import Pureli.Printer()


-----------------
-- Par Builtins
-----------------

-- |the magic!!!
seqParMap f xs = sequence $ (fmap f xs `P.using` P.parList P.rdeepseq)

-----------------

-- |type for evaluation
type Evaluation m a = MT.ReaderT (EvalState m) (MT.ExceptT Error m) (WithMD a)
-- |a type of evaluation
type MEval m a = MT.ReaderT (EvalState m) (MT.ExceptT Error m) a
-- |evaluation in the identity monad
type PureEval a = MT.ReaderT (EvalState MT.Identity) (MT.ExceptT Error MT.Identity) a
-- |evaluation in the io monad
type IOEval a = Evaluation IO a

instance P.NFData Error

-- |environment and builtin function
data EvalState m = EvalState { getModule :: Module, getBuiltins :: Builtins m }

getEnv :: EvalState m -> Env
getEnv = getModEnv . getModule

lookupInModule :: Name -> Module -> Maybe (WithMD Expr, Module)
lookupInModule name modul =
  case M.lookup name (getModExports modul) of
    Just x -> return (x, modul)
    Nothing -> do
      let namePath = splitBy moduleSplitter name
      if length namePath < 2
        then Nothing
        else case find ((==) (head namePath) . getModName) (getModImports modul) of
          Nothing -> Nothing
          Just m  -> lookupInModule (concat (tail namePath)) m

-- |update the environnment with a function
updateEnv ::  (Env -> Env) -> EvalState m -> EvalState m
updateEnv f   state = state { getModule = (getModule state) { getModEnv = (f (getModEnv (getModule state))) } }


addToEnv :: (Name, WithMD Expr) -> Module -> Module
addToEnv (name, expr) modul = modul { getModEnv = M.insert name expr (getModEnv modul) }

addImport :: Module -> Module -> Module
addImport imprt modul = modul { getModImports = imprt : getModImports modul }

-- |change the environment
changeEnv ::  Env -> EvalState m -> EvalState m
changeEnv env state = state { getModule = (getModule state) { getModEnv = env } }

-- |change the module
changeModule :: Module -> EvalState m -> EvalState m
changeModule modu state = state { getModule = modu }

-- |change the builtin functions
changeBuiltins ::  Builtins m -> EvalState t -> EvalState m
changeBuiltins builtins state = state { getBuiltins = builtins }

-- |an initial environment state
initEvalState :: EvalState IO
initEvalState = EvalState (Module "" "REPL" [] emptyEnv emptyEnv emptyEnv emptyEnv) builtinsIO

replModule :: Module
replModule =  Module "" "REPL" [] emptyEnv emptyEnv emptyEnv emptyEnv

-- |empty Environment
emptyEnv :: Env
emptyEnv = M.fromList []

-- |evaluate a program executable from environment by looking up main.
evalModule :: Module -> MT.ExceptT Error IO (WithMD Expr)
evalModule modul = do
  case M.lookup "main" (getModEnv modul) of
    Nothing   -> throwE $ Error Nothing $ "No main function in program \n*** " ++ (show $ map fst (M.toList (getModEnv modul)))
    Just expr -> MT.runReaderT (eval expr) (EvalState modul ioBuiltins)


-- |evaluate an expression in pure context
pureEval :: Module -> WithMD Expr -> Either Error (WithMD Expr)
pureEval modul exprWithMD@(WithMD md expr) = do
  flip pureEvaluation modul $
    case expr of
      ATOM a      -> evalAtom exprWithMD a
      PROCEDURE p -> return $ WithMD md $ PROCEDURE p
      QUOTE l     -> return $ WithMD md $ QUOTE l
      LIST ls     -> evalOp exprWithMD ls


evalExpr :: Module -> WithMD Expr -> IO (Either Error (WithMD Expr))
evalExpr modul expr = MT.runExceptT $ MT.runReaderT (eval expr) $ EvalState modul builtinsIO

-- |evaluate an expression
eval :: Monad m => WithMD Expr -> Evaluation m Expr
eval exprWithMD@(WithMD md expr) = do
  case expr of
    ATOM a      -> evalAtom exprWithMD a
    PROCEDURE p -> return $ WithMD md $ PROCEDURE p
    QUOTE l     -> return $ WithMD md $ QUOTE l
    LIST ls     -> evalOp exprWithMD ls

-- |evaluate a primitive expression
evalAtom :: Monad m => WithMD Expr -> Atom -> Evaluation m Expr
evalAtom rootExpr@(WithMD md _) atom = do
  modul    <- ask >>= return . getModule
  env      <- ask >>= return . getEnv
  builtins <- ask >>= return . getBuiltins
  case atom of
    Symbol var -> case M.lookup var env of
      Just v  -> eval v
      Nothing -> case lookupInModule var modul of
        Just (v, vMod) -> MT.withReaderT  (changeModule vMod) (eval v)
        Nothing -> case M.lookup var builtins of
          Nothing -> throwErr (Just rootExpr) $ "Could not find " ++ show var ++ " in environment"
          Just _  -> return $ WithMD md $ ATOM $ Symbol var -- |^this will be handled later
    other -> return $ WithMD md (ATOM other)

-- |evaluate an operation application
evalOp :: Monad m => WithMD Expr -> [WithMD Expr] -> Evaluation m Expr
evalOp (WithMD md _) [] = return $ WithMD md $ ATOM Nil
evalOp exprWithMD (WithMD md operator:operands) = do
  builtins <- return . getBuiltins =<< ask
  -- |decide what to do based on the operator
  case operator of
    -- |call the function
    (PROCEDURE _)     -> case M.lookup ";evalProcedure" builtins of -- evalProcedure operands (WithMD md operator)
      Nothing -> throwErr (Just exprWithMD) "implementation bug: could no evaluate procedure. please report this"
      Just ep -> ep (WithMD md operator) operands
    -- |evaluate the operator first and try again
    l@(LIST _)        -> evalOp exprWithMD . (:operands) =<< eval (WithMD md l)
    -- |maybe it's a name for a function in the environment or a builtin function?
    (ATOM (Symbol s)) -> evalOpSymbol exprWithMD operands s
    other             -> throwErr (Just exprWithMD) $ show other ++ " is not a function"


-- |calls a function
evalProcedure :: Monad m => [WithMD Expr] -> WithMD Expr -> Evaluation m Expr
evalProcedure operands (WithMD md (PROCEDURE (Closure closure_env (WithMD _ (Fun (FunArgsList args) body))))) = do
  -- |evaluate arguments
  evaluated_args <- mapM eval operands
  let argsAsList = WithMD md $ QUOTE $ WithMD md $ LIST evaluated_args
  -- |extend environment with arguments
  let extended_env = M.fromList [(args, argsAsList)] `M.union` closure_env
  -- |evaluate the function's body in the extended environment
  MT.withReaderT (changeEnv extended_env) (eval (WithMD md body))
evalProcedure operands rootExpr@(WithMD md (PROCEDURE (Closure closure_env (WithMD _ (Fun (FunArgs args mrest) body))))) =
  let (operands_length, args_length) = (length operands, length args)
  in
    case mrest of
      Just rest ->
        if operands_length < args_length -- |^check arity
        then
          throwErr (Just rootExpr) $ " arity problem: expecting at least " ++ show args_length ++ " arguments, got " ++ show operands_length
        else do
          -- |evaluate arguments
          evaluated_args <- mapM eval operands
          -- |extend environment with arguments
          let extended_env = zipWithRest rest md args evaluated_args `M.union` closure_env
          -- |evaluate the function's body in the extended environment
          MT.withReaderT (changeEnv extended_env) (eval (WithMD md body))
      Nothing ->
        if operands_length /= args_length -- |^check arity
        then
          throwErr (Just rootExpr) $ " arity problem: expecting " ++ show args_length ++ " arguments, got " ++ show operands_length
        else do
          -- |evaluate arguments
          evaluated_args <- mapM eval operands
          -- |extend environment with arguments
          let extended_env = M.fromList (zip args evaluated_args) `M.union` closure_env
          -- |evaluate the function's body in the extended environment
          MT.withReaderT (changeEnv extended_env) (eval (WithMD md body))
-- not a procedure
evalProcedure _ rootExpr = throwErr (Just rootExpr) $ "not a procedure"


-- |calls a function in pure context
pureEvalProcedure :: [WithMD Expr] -> WithMD Expr -> PureEval (WithMD Expr)
pureEvalProcedure operands (WithMD md (PROCEDURE (Closure closure_env (WithMD _ (Fun (FunArgsList args) body))))) = do
  modul <- return . getModule =<< ask
  -- |evaluate arguments
  evaluated_args <- liftFromEither $ seqParMap (pureEval modul) operands
  let argsAsList = WithMD md $ QUOTE $ WithMD md $ LIST evaluated_args
  -- |extend environment with arguments
  let extended_env = M.fromList [(args, argsAsList)] `M.union` closure_env
  -- |evaluate the function's body in the extended environment
  MT.withReaderT (changeEnv extended_env) (eval (WithMD md body))
pureEvalProcedure operands rootExpr@(WithMD md (PROCEDURE (Closure closure_env (WithMD _ (Fun (FunArgs args mrest) body))))) =
  let (operands_length, args_length) = (length operands, length args)
  in
    case mrest of
      Just rest ->
        if operands_length < args_length -- |^check arity
        then
          throwErr (Just rootExpr) $ " arity problem: expecting at least " ++ show args_length ++ " arguments, got " ++ show operands_length
        else do
          modul <- return . getModule =<< ask
          -- |evaluate arguments
          evaluated_args <- liftFromEither $ seqParMap (pureEval modul) operands
          -- |extend environment with arguments
          let extended_env = zipWithRest rest md args evaluated_args `M.union` closure_env
          -- |evaluate the function's body in the extended environment
          MT.withReaderT (changeEnv extended_env) (eval (WithMD md body))
      Nothing ->
        if operands_length /= args_length -- |^check arity
        then
          throwErr (Just rootExpr) $ " arity problem: expecting " ++ show args_length ++ " arguments, got " ++ show operands_length
        else do
          modul <- return . getModule =<< ask
          -- |evaluate arguments
          evaluated_args <- liftFromEither $ seqParMap (pureEval modul) operands
          -- |extend environment with arguments
          let extended_env = M.fromList (zip args evaluated_args) `M.union` closure_env
          -- |evaluate the function's body in the extended environment
          MT.withReaderT (changeEnv extended_env) (eval (WithMD md body))
-- not a procedure
pureEvalProcedure _ rootExpr = throwErr (Just rootExpr) $ "not a procedure"




zipWithRest restVar md args ops =
  case zipWithRemains args ops of
      (zipped, rest) -> M.fromList $ (restVar, WithMD md (LIST (WithMD md (ATOM $ Symbol "list") : rest))) : zipped

zipWithRemains :: [a] -> [b] -> ([(a,b)],[b])
zipWithRemains [] rest = ([], rest)
zipWithRemains _  []   = ([], [])
zipWithRemains (x:xs) (y:ys) =
  case zipWithRemains xs ys of
      (zipped, rest) -> ((x,y):zipped, rest)


-- |tries to find symbol in environment and evaluate function call
evalOpSymbol :: Monad m => WithMD Expr -> [WithMD Expr] -> Name -> Evaluation m Expr
evalOpSymbol exprWithMD operands name = do
  modul    <- ask >>= return . getModule
  env      <- ask >>= return . getEnv
  builtins <- ask >>= return . getBuiltins
  case M.lookup name builtins of
    Just op -> op exprWithMD operands
    Nothing -> case M.lookup name env of
      Just v  -> eval v >>= \result -> evalOp exprWithMD (result : operands)
      Nothing -> case lookupInModule name modul of
        Nothing -> throwErr (Just exprWithMD) $ " Could not find " ++ show name ++ " in environment: " ++ show env
        Just (v, vMod) -> do
          result <- MT.withReaderT (changeModule vMod) (eval v)
          MT.withReaderT (changeModule modul) $ evalOp exprWithMD (result : operands)

-------------
-- Builtins
-------------

type Builtins m = M.Map Name (WithMD Expr -> [WithMD Expr] -> Evaluation m Expr)

-- |pure builtins in the identity monad
builtinsID :: Builtins MT.Identity
builtinsID = pureContextBuiltins

-- |all builtins in the IO monad
builtinsIO :: Builtins IO
builtinsIO = pureBuiltins `M.union` ioBuiltins


-- |pure builtins in an arbitrary monad
pureBuiltins :: Monad m => Builtins m
pureBuiltins =
  M.fromList
    [(";evalProcedure", flip evalProcedure)
    ]
  `M.union`
  M.fromList
    [("+", evalPlus)
    ,("-", evalMinus)
    ,("*", evalMul)
    ,("/", evalDiv)
    ,("if", evalIf)
    ,("++", evalAppend)
    ,("zero?",      evalIs isZeroTest)
    ,("nil?",       evalIs isNilTest)
    ,("empty?",     evalIs isEmptyTest)
    ,("string?",    evalIs isStringTest)
    ,("integer?",   evalIs isIntegerTest)
    ,("real?",      evalIs isRealTest)
    ,("number?",    evalIs isNumberTest)
    ,("procedure?", evalIs isFunTest)
    ,("list?",      evalIs isListTest)
    ,("=",      evalCompare (compareAtoms (==)))
    ,("<>",     evalCompare (compareAtoms (/=)))
    ,("<",      evalCompare (compareAtoms (<)))
    ,(">",      evalCompare (compareAtoms (>)))
    ,("<=",     evalCompare (compareAtoms (<=)))
    ,(">=",     evalCompare (compareAtoms (>=)))
    ,("length", evalLength)
    ,("slice", evalSlice)
    ,("show", evalShow)
    ,("error", evalError)
    ,("try", evalTry)
    ,("trace", evalTrace)
    ,("list", evalList)
    ,("car", evalCar)
    ,("cdr", evalCdr)
    ,("lambda", evalLambda)
    ,("quote",  evalQuote)
    ,("mquote", evalQuote)
    ,("eval", evalEval)
    ,("let", evalLet)
    ,("letrec", evalLetrec)]





pureContextBuiltins :: Builtins MT.Identity
pureContextBuiltins =
  M.fromList
    [(";evalProcedure", flip pureEvalProcedure)
    ,("let", pureEvalLet)
    ,("++", pureEvalAppend)
    ,("list", pureEvalList)]
  `M.union` pureBuiltins

-- |IO builtins in the IO monad
ioBuiltins :: Builtins IO
ioBuiltins = M.fromList [("pure",  evalPure)
                        ,("do!",    evalDo)
                        ,("print!", evalPrint)
                        ,("read!",  evalRead)
                        ,("print-file!", evalPrintFile)
                        ,("read-file!",  evalReadFile)]

-- |take the value from an IO context
fromIO :: WithMD Expr -> IOEval Expr
fromIO (WithMD _ (QUOTE (WithMD _ (LIST [WithMD _ (ATOM (Symbol ";IO")), result])))) = lift $ return result
fromIO rootExpr = throwErr (Just rootExpr) "not an IO action"

-- |insert a value into an IO context
returnIO expr@(WithMD md _) = lift $ return $ WithMD md $ QUOTE $ WithMD md $ LIST [WithMD md (ATOM (Symbol ";IO")), expr]

--------
-- IO
--------

-- |evaluate 'pure' action
evalPure :: WithMD Expr -> [WithMD Expr] -> IOEval Expr
evalPure rootExpr = \case
  -- |check arity
  [element] -> do
    -- |evaluate an expression in a pure context
    state <- ask
    case pureEval (getModule state) element of
      Left err -> lift $ MT.throwE err
      Right rs -> returnIO rs
  xs        -> throwErr (Just rootExpr) $  "bad arity, expected 1 argument, got: " ++ show (length xs)


-- |evaluate a 'do!' sequence of IO actions
evalDo :: WithMD Expr -> [WithMD Expr] -> IOEval Expr
evalDo rootExpr actions = do
  evaluated <- MT.withReaderT (changeBuiltins builtinsIO) (evalSequence rootExpr actions) >>= fromIO
  returnIO evaluated


-- |evaluate a sequence of IO actions
evalSequence :: WithMD Expr -> [WithMD Expr] -> IOEval Expr
evalSequence rootExpr@(WithMD exprMD _) = \case
  -- |a last let! returns the argument
  [WithMD _ (LIST [WithMD _ (ATOM (Symbol "let!")), WithMD _ (ATOM (Symbol _)), expr])] -> eval expr
  -- |a let! binds the value from the IO action to a name for the rest of the sequence
  (WithMD _ (LIST [WithMD _ (ATOM (Symbol "let!")), WithMD _ (ATOM (Symbol name)), expr])):rest -> do
    result <- eval expr >>= fromIO
    MT.withReaderT (updateEnv (M.insert name result)) (evalSequence rootExpr rest)
  [x]    -> eval x
  x:rest -> eval x >>= fromIO >> evalSequence rootExpr rest
  []     -> returnIO $ WithMD exprMD (ATOM Nil)


-- |evaluate a 'print!' IO action
evalPrint :: WithMD Expr -> [WithMD Expr] -> IOEval Expr
evalPrint _ [expr@(WithMD md _)] = do
  evalled <- eval expr
  lift $ lift $ case evalled of
    (WithMD _ (ATOM (String str))) -> putStrLn str
    _                              -> print evalled
  returnIO $ WithMD md $ ATOM Nil
evalPrint expr xs = throwErr (Just expr) $ "bad arity, expected 1 argument, got: " ++ show (length xs)

-- |evaluate a 'read!' IO action
evalRead :: WithMD Expr -> [WithMD Expr] -> IOEval Expr
evalRead (WithMD md _) [] = do
  input <- lift (lift getLine)
  returnIO $ WithMD md $ ATOM $ String input
evalRead expr xs = throwErr (Just expr) $ "bad arity, expected 0 arguments, got: " ++ show (length xs)

-- |evaluate a 'print-file!' IO action
evalPrintFile :: WithMD Expr -> [WithMD Expr] -> IOEval Expr
evalPrintFile rootExpr [file@(WithMD md _), expr] = do
  eFile   <- eval file
  evalled <- eval expr
  case (eFile, evalled) of
    ((WithMD _ (ATOM (String wfile))), (WithMD _ (ATOM (String str)))) -> lift $ lift $ writeFile wfile str
    ((WithMD _ (ATOM (String wfile))), _) -> do
      result <- lift $ lift ((writeFile wfile (show evalled) >> return Nothing) `catch` (\e -> return $ Just $ show (e :: IOException)))
      case result of
        Nothing -> return ()
        Just er -> throwErr (Just rootExpr) er
    _ -> throwErr (Just eFile) $ "unexpected filepath"
  returnIO $ WithMD md $ ATOM Nil
evalPrintFile expr xs = throwErr (Just expr) $ "bad arity, expected 2 argument, got: " ++ show (length xs)

-- |evaluate a 'read!' IO action
evalReadFile :: WithMD Expr -> [WithMD Expr] -> IOEval Expr
evalReadFile expr@(WithMD md _) [file] = do
  eFile <- eval file
  input <- case eFile of
    (WithMD _ (ATOM (String rfile))) -> do
      result <- lift $ lift ((readFile rfile >>= return . Right) `catch` (\e -> return $ Left $ show (e :: IOException)))
      case result of
        Right x -> return x
        Left er -> throwErr (Just expr) er
    _                                -> throwErr (Just eFile) $ "unexpected filepath"
  returnIO $ WithMD md $ ATOM $ String input
evalReadFile expr xs = throwErr (Just expr) $ "bad arity, expected 1 arguments, got: " ++ show (length xs)


----------
-- Pure
----------


-- |evaluate a 'show' expression. converts an expression to string.
evalShow :: Monad m => WithMD Expr -> [WithMD Expr] -> Evaluation m Expr
evalShow rootExpr@(WithMD md _) = \case
  [expr] -> do
    eval expr >>= lift . return . WithMD md . ATOM . String . show
  xs -> throwErr (Just rootExpr) $ "bad arity: expects 1 argument, got " ++ show (length xs)

-- |evaluate a 'trace' expression.
evalTrace :: Monad m => WithMD Expr -> [WithMD Expr] -> Evaluation m Expr
evalTrace rootExpr@(WithMD md _) = \case
  [tr, expr] -> eval tr >>= flip traceShow (eval expr)
  xs -> throwErr (Just rootExpr) $ "bad arity: expects 2 arguments, got " ++ show (length xs)


-- |evaluate a 'try' expression. try will return the first expression that does not fail to evaluate
evalTry :: Monad m => WithMD Expr -> [WithMD Expr] -> Evaluation m Expr
evalTry rootExpr operands = go operands
  where go = \case
            []    -> throwErr (Just rootExpr) $ "bad arity: expects at least 1 argument, got 0"
            [try] -> eval try
            (try:retry) -> do
              env <- ask
              lift $ MT.runReaderT (eval try) env `MT.catchE` \_ -> MT.runReaderT (go retry) env

-- |evaluate an 'error' expression. will evaluate it's expression and will throw the error
evalError :: Monad m => WithMD Expr -> [WithMD Expr] -> Evaluation m Expr
evalError rootExpr = \case
  [err] -> do
    res <- eval err
    throwErr (Just res) "Error thrown by the user"
  xs -> throwErr (Just rootExpr) $ "bad arity: expects 1 argument, got " ++ show (length xs)




-- |evaluate a 'lambda' expression
evalLambda :: Monad m => WithMD Expr -> [WithMD Expr] -> Evaluation m Expr
evalLambda rootExpr@(WithMD exprMD _) exprs = do
  env <- ask >>= return . getEnv
  case exprs of
    -- |check arity
    [WithMD _ (ATOM (Symbol argName)), (WithMD _ body)] ->
      lift $ return $ WithMD exprMD $ PROCEDURE $ Closure env $ WithMD exprMD $ Fun (FunArgsList argName) body
    [WithMD _ (LIST symbolList), bodyExpr@(WithMD _ body)] -> do
      -- |check for duplicate argument names
      symbols <- liftFromEither $ seqParMap toSymbol symbolList
      if length (duplicates symbols) > 0
      then throwErr (Just bodyExpr) $ "lambda arguments must have different names"
      -- |return a procedure
      else case validArgs symbols of
        (False, _) -> throwErr (Just bodyExpr) $ "unexpected &. rest argument is not last"
        (True, Nothing) -> lift $ return $ WithMD exprMD $ PROCEDURE $ Closure env $ WithMD exprMD $ Fun (FunArgs symbols Nothing) body
        (True, Just vr) -> lift $ return $ WithMD exprMD $ PROCEDURE $ Closure env $ WithMD exprMD $ Fun (FunArgs (init symbols) (Just vr)) body
    xs -> throwErr (Just rootExpr) $ "bad arity: lambda expects 2 arguments, got " ++ show (length xs)



-- |evaluate a 'letrec' expression
evalLetrec :: Monad m => WithMD Expr -> [WithMD Expr] -> Evaluation m Expr
evalLetrec rootExpr = \case
  -- |check arity
  [binders, body] -> case binders of
    -- |evaluate binds and evaluate body in extended environment
    WithMD _ (LIST binds) -> lift (evalLetrecBinds binds) >>= \checkedBinds -> MT.withReaderT (updateEnv (M.fromList checkedBinds `M.union`)) (eval body)
      where evalLetrecBinds = \case
    -- |^check let arguments form, convert to (Name, WithMD Expr) but don't evaluate
              [] -> return []
              (WithMD _ (LIST [WithMD _ (ATOM (Symbol name)), expr]) :xs) -> return . ((name, expr):) =<< evalLetrecBinds xs
              _  -> throwE $ Error (Just rootExpr) $ "unexpected binds in let expression"
    expr -> throwErr (Just expr) "second argument to let is not a list of bindings"
  ls -> throwErr (Just rootExpr) $ "let expects 2 arguments, got " ++ show (length ls)

-- |evaluate a 'let' expression
evalLet :: Monad m => WithMD Expr -> [WithMD Expr] -> Evaluation m Expr
evalLet rootExpr = \case
  -- |check arity
  [binders, body] -> case binders of
    -- |evaluate binds and evaluate body in extended environment
    WithMD _ (LIST binds) -> evalLetBinds binds >>= \evaluated_binds -> MT.withReaderT (updateEnv (M.fromList (evaluated_binds) `M.union`)) $ eval body
      where evalLetBinds = \case
    -- |^evaluate binds
              [] -> lift $ return []
              (WithMD _ (LIST [WithMD _ (ATOM (Symbol name)), expr]) :xs) -> eval expr >>= \result -> return . ((name, result):) =<< evalLetBinds xs
              _  -> throwErr (Just rootExpr) $ "unexpected binds in let expression"
    _ -> throwErr (Just rootExpr) $ "argument to let is not a list of bindings"
  ls -> throwErr (Just rootExpr) $ "let expects 2 arguments, got " ++ show (length ls)


-- |evaluate a 'let' expression
pureEvalLet :: WithMD Expr -> [WithMD Expr] -> PureEval (WithMD Expr)
pureEvalLet rootExpr operands = do
  modul <- return . getModule =<< ask
  case operands of
    -- |check arity
    [binders, body] -> case binders of
      -- |evaluate binds and evaluate body in extended environment
      WithMD _ (LIST binds) -> liftFromEither (seqParMap evalLetBind binds) >>= \evaluated_binds -> liftFromEither $ pureEval (modul { getModEnv = M.fromList (evaluated_binds) `M.union` (getModEnv modul) }) body
        where evalLetBind = \case
      -- |^evaluate binds
                WithMD _ (LIST [WithMD _ (ATOM (Symbol name)), expr]) -> pureEval modul expr >>= \result -> return $ (name, result)
                _  -> Left $ Error (Just rootExpr) $ "unexpected binds in let expression"
      _ -> throwErr (Just rootExpr) $ "argument to let is not a list of bindings"
    ls -> throwErr (Just rootExpr) $ "let expects 2 arguments, got " ++ show (length ls)



-- |evaluate an 'if' expression. '#f' is false, everything else is true.
evalIf :: Monad m => WithMD Expr -> [WithMD Expr] -> Evaluation m Expr
evalIf rootExpr = \case
    [bool, trueBranch, falseBranch] -> do
      eval bool >>= \case
        WithMD _ (ATOM (Bool False)) -> eval falseBranch
        _                            -> eval trueBranch
    xs -> throwErr (Just rootExpr) $ "bad arity, expected 3 arguments, got: " ++ show (length xs)

-- |evaluate compare expressions. such as '==', '<', '/=' etc.
evalCompare :: Monad m => (WithMD Expr-> Expr -> Expr -> MT.ExceptT Error m Bool) -> WithMD Expr -> [WithMD Expr] -> Evaluation m Expr
evalCompare test rootExpr@(WithMD exprMD _) operands = do
  -- |evaluate operands
  modul <- ask >>= return . getModule
  evalled <- liftFromEither $ seqParMap (pureEval modul) operands
  -- |test and return
  lift (testCompare rootExpr test evalled) >>= \case
    True  -> return $ WithMD exprMD $ ATOM $ Bool True
    False -> return $ WithMD exprMD $ ATOM $ Bool False


-- |compare values
testCompare :: Monad m => WithMD Expr -> (WithMD Expr -> Expr -> Expr -> MT.ExceptT Error  m Bool) -> [WithMD Expr] -> MT.ExceptT Error m Bool
-- |compare of an empty list is true
testCompare _ _ []        = return True
-- |compare of a single argument is true
testCompare _ _ [_]       = return True
-- |test at least two expressions
testCompare expr f (x:y:xs) = on (f expr) stripMD x y >>= \case
  True  -> testCompare expr f (y:xs)
  False -> return False

-- | evaluate '<'
compareAtoms :: Monad m => (Atom -> Atom -> Bool) -> WithMD Expr-> Expr -> Expr -> MT.ExceptT Error m Bool
compareAtoms  f _ (ATOM x@(Integer _)) (ATOM y@(Integer _)) = return $ f x y
compareAtoms  f _ (ATOM x@(Real _)) (ATOM y@(Real _)) = return $ f x y
compareAtoms  f _ (ATOM x@(String _)) (ATOM y@(String _)) = return $ f x y
compareAtoms  f _ (ATOM x@(Bool _)) (ATOM y@(Bool _)) = return $ f x y
compareAtoms  _ expr _ _ = throwE $ Error (Just expr) "cannot compare types"


-- |evaluate 'is-...' expressions
evalIs :: Monad m => (Expr -> Bool) -> WithMD Expr -> [WithMD Expr] -> Evaluation m Expr
evalIs test rootExpr@(WithMD exprMD _) = \case
  [expr] -> eval expr >>= \case
    WithMD _ result -> if test result
      then lift $ return $ WithMD exprMD $ ATOM $ Bool True
      else lift $ return $ WithMD exprMD $ ATOM $ Bool False
  xs -> throwErr (Just rootExpr) $ "bad arity, expected 1 argument, got: " ++ show (length xs)

-- |test zero?
isZeroTest :: Expr -> Bool
isZeroTest (ATOM (Integer 0)) = True
isZeroTest (ATOM (Real 0.0))  = True
isZeroTest _                  = False

-- |test nil?
isNilTest :: Expr -> Bool
isNilTest (ATOM Nil) = True
isNilTest _          = False

-- |test empty?
isEmptyTest :: Expr -> Bool
isEmptyTest (QUOTE (WithMD _ (LIST []))) = True
isEmptyTest _          = False

-- |test integer?
isIntegerTest :: Expr -> Bool
isIntegerTest (ATOM (Integer _)) = True
isIntegerTest _                  = False

-- |test real?
isRealTest :: Expr -> Bool
isRealTest (ATOM (Real _)) = True
isRealTest _               = False

-- |test number?
isNumberTest :: Expr -> Bool
isNumberTest (ATOM (Integer _)) = True
isNumberTest (ATOM (Real _))    = True
isNumberTest _                  = False

-- |test string?
isStringTest :: Expr -> Bool
isStringTest (ATOM (String _)) = True
isStringTest _                 = False

-- |test list?
isListTest :: Expr -> Bool
isListTest (QUOTE (WithMD _ (LIST _))) = True
isListTest _ = False


-- |test procedure?
isFunTest :: Expr -> Bool
isFunTest (PROCEDURE _) = True
isFunTest (LIST (WithMD _ (ATOM (Symbol "lambda")) : (WithMD _ (LIST _)) : _)) = True
isFunTest (ATOM (Symbol s)) =
  case M.lookup s builtinsID of
    Nothing -> False
    Just _  -> True
isFunTest _             = False

-- |evaluate 'list' expression
evalList :: Monad m => WithMD Expr -> [WithMD Expr] -> Evaluation m Expr
evalList (WithMD exprMD _) elements = do
  evalled <- sequence $ fmap eval elements
  lift $ return $ WithMD exprMD $ QUOTE $ WithMD exprMD $ LIST evalled

-- |evaluate 'list' expression in pure context
pureEvalList :: WithMD Expr -> [WithMD Expr] -> PureEval (WithMD Expr)
pureEvalList (WithMD exprMD _) elements = do
  modul <- return . getModule =<< ask
  evalled <- liftFromEither $ seqParMap (pureEval modul) elements
  lift $ return $ WithMD exprMD $ QUOTE $ WithMD exprMD $ LIST evalled


-- |evaluate 'car' expression
evalCar :: Monad m => WithMD Expr -> [WithMD Expr] -> Evaluation m Expr
evalCar rootExpr = \case
  [expr] -> eval expr >>= \case
    WithMD _       (QUOTE (WithMD _ (LIST (x:_)))) -> return x
    WithMD _       (QUOTE (WithMD _ (LIST [])))    -> throwErr (Just rootExpr) "car on empty list"
    _  -> throwErr (Just rootExpr)   " cannot car a non-list type"
  xs   -> throwErr (Just rootExpr) $ "bad arity, expected 1 argument, got: " ++ show (length xs)

-- |evaluate 'cdr' expression
evalCdr :: Monad m => WithMD Expr -> [WithMD Expr] -> Evaluation m Expr
evalCdr rootExpr = \case
  [expr] -> eval expr >>= \case
    WithMD quoteMD (QUOTE (WithMD md (LIST (_:xs)))) -> lift $ return $ WithMD quoteMD $ QUOTE $ WithMD md $ LIST xs
    WithMD _       (QUOTE (WithMD _ (LIST [])))      -> throwErr (Just rootExpr) "cdr on empty list"
    _  -> throwErr (Just rootExpr) "cannot cdr a non-list type"
  xs        ->throwErr (Just rootExpr) $ "bad arity, expected 1 argument, got: " ++ show (length xs)

-- |evaluate a 'quote' expression. don't evaluate argument
evalQuote :: Monad m => WithMD Expr -> [WithMD Expr] -> Evaluation m Expr
evalQuote rootExpr@(WithMD exprMD _) = \case
  [element] -> lift $ return $ WithMD exprMD $ QUOTE $ element
  xs        -> throwErr (Just rootExpr) $ "bad arity, expected 1 argument, got: " ++ show (length xs)

-- |evaluate 'eval' expression. evaluate QUOTE
evalEval :: Monad m => WithMD Expr -> [WithMD Expr] -> Evaluation m Expr
evalEval rootExpr = \case
  [element] -> eval element >>= \case
    WithMD _ (QUOTE expr) -> eval expr
    expr                  -> eval expr >>= eval
  xs        ->throwErr (Just rootExpr) $ "bad arity, expected 1 argument, got: " ++ show (length xs)



-- |(+)
evalPlus :: Monad m => WithMD Expr -> [WithMD Expr] -> Evaluation m Expr
evalPlus = evalArith (return . add) (return . add)

-- |(-)
evalMinus :: Monad m => WithMD Expr -> [WithMD Expr] -> Evaluation m Expr
evalMinus = evalArith (return . sub) (return . sub)

-- |(*)
evalMul :: Monad m => WithMD Expr -> [WithMD Expr] -> Evaluation m Expr
evalMul = evalArith (return . mul) (return . mul)

-- |(/)
evalDiv :: Monad m => WithMD Expr -> [WithMD Expr] -> Evaluation m Expr
evalDiv rootExpr = evalArith (divide rootExpr div) (divide rootExpr (/)) rootExpr


-- |evaluate arithmetic expressions. converts integers to floats if an argument is a float.
evalArith :: Monad m => ([Integer] -> MT.ExceptT Error m Integer) -> ([Double] -> MT.ExceptT Error m Double) -> WithMD Expr -> [WithMD Expr] -> Evaluation m Expr
evalArith intOp realOp rootExpr@(WithMD exprMD _) operands = do
  modul <- return . getModule =<< ask
  results <- liftFromEither $ seqParMap (evalToNumber modul rootExpr) operands
  if length (filter isReal results) > 0
  then
    lift ((realOp $ (fmap atomToDouble results)) >>= return . WithMD exprMD . ATOM . Real)
  else
    lift ((intOp $ (fmap atomToInteger results)) >>= return . WithMD exprMD . ATOM . Integer)


-- |evaluate '++' expression for lists and strings.
evalAppend :: Monad m => WithMD Expr -> [WithMD Expr] -> Evaluation m Expr
evalAppend rootExpr@(WithMD exprMD _) operands =
  (sequence $ fmap (evalToList rootExpr) operands) >>= \res -> case sequence res of
    Right results -> lift $ return $ WithMD exprMD $ QUOTE $ WithMD exprMD $ LIST $ foldl (++) [] results
    Left _ -> do
        result <- mapM (evalToString rootExpr) operands
        case sequence result of
          Right results -> lift $ return $ WithMD exprMD $ ATOM $ String $ foldl (++) "" results
          Left _ -> throwErr (Just rootExpr) "bad arguments to append. all arguments must be of type string or list."



-- |evaluate '++' expression for lists and strings.
pureEvalAppend :: WithMD Expr -> [WithMD Expr] -> PureEval (WithMD Expr)
pureEvalAppend rootExpr@(WithMD exprMD _) operands = do
  modul <- return . getModule =<< ask
  case (seqParMap (pureEvalToList modul rootExpr) operands) of
    Right results -> lift $ return $ WithMD exprMD $ QUOTE $ WithMD exprMD $ LIST $ foldl (++) [] results
    Left _ -> do
        case seqParMap (pureEvalToString modul rootExpr) operands of
          Right results -> lift $ return $ WithMD exprMD $ ATOM $ String $ foldl (++) "" results
          Left _ -> throwErr (Just rootExpr) "bad arguments to append. all arguments must be of type string or list."




-- |test if atom is real
isReal :: Atom -> Bool
isReal = \case
  Real _ -> True
  _      -> False

-- |test if atom is integer
isInteger :: Atom -> Bool
isInteger = \case
  Integer _ -> True
  _         -> False

-- |test if atom is a number
isNumber :: Atom -> Bool
isNumber = \case
  Integer _ -> True
  Real _    -> True
  _         -> False

-- |test if atom is a symbol
isSymbol :: WithMD Expr -> Bool
isSymbol (WithMD _ (ATOM (Symbol _))) = True
isSymbol _                            = False

-- |try converting an expression to a symbol
toSymbol :: WithMD Expr -> Either Error String
toSymbol (WithMD _ (ATOM (Symbol s))) = return s
toSymbol expr                         = Left $ Error (Just expr)  "expecting a symbol"



evalLength :: Monad m => WithMD Expr -> [WithMD Expr] -> Evaluation m Expr
evalLength rootExpr@(WithMD exprMD _) operands =
  case operands of
    [x] -> evalToList rootExpr x >>= \case
      Right ls -> lift $ return $ WithMD exprMD $ ATOM $ Integer $ fromIntegral (length ls)
      Left _   -> evalToString rootExpr x >>= \case
        Right str -> lift $ return $ WithMD exprMD $ ATOM $ Integer $ fromIntegral (length str)
        Left e    -> lift $ MT.throwE e
    vs -> throwErr (Just rootExpr) $ " arity problem: expecting 1 argument, got " ++ show (length vs)



-- |slice: a slice of a string or list
evalSlice :: Monad m => WithMD Expr -> [WithMD Expr] -> Evaluation m Expr
evalSlice rootExpr@(WithMD exprMD _) operands = do
  modul <- return . getModule =<< ask
  case operands of
    [start, end, value] -> do
      res <- sequence3
                (evalToList rootExpr value
               ,evalAndGetInt modul rootExpr start
               ,evalAndGetInt modul rootExpr end)
      case res of
        (Right l, Right s, Right e) -> lift $ return $ WithMD exprMD $ QUOTE $ WithMD exprMD $ LIST $ take (fromIntegral (e - s)) $ drop (fromIntegral s) l
        (Left _, Right s, Right e) -> evalToString rootExpr value >>= \case
          Left _  -> throwErr (Just value) "not of type string or list"
          Right str -> lift $ return $ WithMD exprMD $ ATOM $ String $ take (fromIntegral (e - s)) $ drop (fromIntegral s) str
        (_, Left s, _) -> lift $ MT.throwE s
        (_, _, Left e) -> lift $ MT.throwE e
    vs -> throwErr (Just rootExpr) $ " arity problem: expecting 3 arguments, got " ++ show (length vs)

evalAndGetInt modul rootExpr e = do
  r <- case evalToNumber modul rootExpr e of
         Right x -> return x
         Left er -> lift $ MT.throwE er
  lift $ return $ castAtomToInteger rootExpr r

sequence3 :: Monad m => (m a, m b, m c) -> m (a, b, c)
sequence3 (x,y,z) = do
  r1 <- x
  r2 <- y
  r3 <- z
  return (r1, r2, r3)

castAtomToInteger :: WithMD Expr -> Atom -> Either Error Integer
castAtomToInteger _ (Integer i) = return i
castAtomToInteger e _           = Left $ Error (Just e) "Not an integer"

castAtomToDouble :: Monad m => WithMD Expr -> Atom -> (MT.ExceptT Error m) Double
castAtomToDouble _ (Real d) = return d
castAtomToDouble e _        = MT.throwE $ Error (Just e) "Not a real"

-- dangerous!!!
atomToInteger :: Atom -> Integer
atomToInteger (Integer i) = i

-- dangerous!!!
atomToDouble :: Atom -> Double
atomToDouble (Real d) = d
atomToDouble (Integer i) = fromIntegral i

pureEvaluation :: PureEval a -> Module -> Either Error a
pureEvaluation go m =
  MT.runIdentity $ MT.runExceptT $ MT.runReaderT go (EvalState m pureContextBuiltins)

liftFromEither :: Monad m => Either Error a -> MEval m a
liftFromEither = \case
  Left err -> lift $ MT.throwE err
  Right rs -> return rs


-- |try to evaluate expression to a number
evalToNumber :: Module -> WithMD Expr -> WithMD Expr -> Either Error Atom
evalToNumber modul rootExpr expr = do
    case pureEval modul expr of
      Right (WithMD _ (ATOM a)) -> if isNumber a then (return a) else Left $ Error (Just rootExpr) $ show a ++ " not a number"
      Right _        -> Left $ Error (Just rootExpr) $ show expr ++ " not a number"
      Left x -> Left x

-- |try to evaluate expression to a string
evalToString :: Monad m => WithMD Expr -> WithMD Expr -> MT.ReaderT (EvalState m) (MT.ExceptT Error m) (Either Error String)
evalToString rootExpr expr = eval expr >>= \case
  (WithMD _ (ATOM (String str))) -> lift $ return $ Right str
  _ -> lift $ return $ Left $ Error (Just rootExpr) $ show expr ++ " is not a string"

-- |try to evaluate expression to a list
evalToList :: Monad m => WithMD Expr -> WithMD Expr -> MT.ReaderT (EvalState m) (MT.ExceptT Error m) (Either Error [WithMD Expr])
evalToList rootExpr expr = eval expr >>= \case
  WithMD _ (QUOTE (WithMD _ (LIST list))) -> lift $ return $ Right list
  _ -> lift $ return $ Left $ Error (Just rootExpr) $ show expr ++ " is not a list"


-- |try to evaluate expression to a string
pureEvalToString :: Module -> WithMD Expr -> WithMD Expr -> Either Error String
pureEvalToString modul rootExpr expr = pureEval modul expr >>= \case
  (WithMD _ (ATOM (String str))) -> Right str
  _ -> Left $ Error (Just rootExpr) $ show expr ++ " is not a string"

-- |try to evaluate expression to a list
pureEvalToList :: Module -> WithMD Expr -> WithMD Expr -> Either Error [WithMD Expr]
pureEvalToList modul rootExpr expr = pureEval modul expr >>= \case
  WithMD _ (QUOTE (WithMD _ (LIST list))) -> Right list
  _ -> Left $ Error (Just rootExpr) $ show expr ++ " is not a list"