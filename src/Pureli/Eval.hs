
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE FlexibleInstances #-}

-- |
-- This module will interpret and run an expression or program in the language
module Pureli.Eval (initEvalState, addToEnv, addImport, replModule, evalExpr, evalModule) where

import Control.Monad (liftM)
import Control.Applicative ((<$>))
import Data.Foldable (forM_)
import Control.Exception (IOException, catch)
import Data.List (find)
import Control.Monad.Trans.Class  (lift, MonadTrans)
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
import Pureli.Parser (parseExpr, getMDSource)


-----------------
-- Par Builtins
-----------------

-- |the magic!!!
seqParMap :: (P.NFData (m b), Monad m) => (a -> m b) -> [a] -> m [b]
seqParMap f xs = sequence (fmap f xs `P.using` P.parList P.rdeepseq)

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

-- |environment and builtin function: we need to save these things to know how
-- should we evaluate expressions
data EvalState m = EvalState { getModule :: Module, getBuiltins :: Builtins m }

-- |getting the environment
getEnv :: EvalState m -> Env
getEnv = getModEnv . getModule

-- |find an expression in the module by name, so we can evaluate
-- the expression in it's environment
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
updateEnv f   state = state { getModule = (getModule state) { getModEnv = f (getModEnv (getModule state)) } }

-- |add a named expression to a module's environment
addToEnv :: (Name, WithMD Expr) -> Module -> Module
addToEnv (name, expr) modul = modul { getModEnv = M.insert name expr (getModEnv modul) }

-- |add a module to import list of another module
addImport :: Module -> Module -> Module
addImport imprt modul = modul { getModImports = imprt : getModImports modul }

-- |change the environment
changeEnv ::  Env -> EvalState m -> EvalState m
changeEnv env state = state { getModule = (getModule state) { getModEnv = env } }

-- |change the module
changeModule :: Module -> EvalState m -> EvalState m
changeModule modul state = state { getModule = modul }

-- |change the builtin functions
changeBuiltins ::  Builtins m -> EvalState t -> EvalState m
changeBuiltins builtins state = state { getBuiltins = builtins }

-- |an initial environment state
initEvalState :: EvalState IO
initEvalState = EvalState (Module "" "REPL" [] emptyEnv emptyEnv emptyEnv emptyEnv) builtinsIO

-- |an initial empty module for a repl
replModule :: Module
replModule =  Module "" "REPL" [] emptyEnv emptyEnv emptyEnv emptyEnv

-- |empty Environment
emptyEnv :: Env
emptyEnv = M.fromList []

-- |evaluate a program executable from environment by looking up main.
evalModule :: Module -> MT.ExceptT Error IO (WithMD Expr)
evalModule m =
  let modul = m { getModEnv = fmap (wrapInEnv modul) (getModEnv m) }
  in
    case M.lookup "main" (getModEnv modul) of
      Nothing   -> throwE $ Error Nothing $ "No main function in program \n*** " ++ show (map fst (M.toList (getModEnv modul)))
      Just expr@(WithMD md _) -> MT.runReaderT (eval $ WithMD md $ LIST [WithMD md $ ATOM $ Symbol "do!", expr]) (EvalState modul builtinsIO)



-- |evaluate an expression in pure context
pureEval :: Module -> WithMD Expr -> Either Error (WithMD Expr)
pureEval modul exprWithMD@(WithMD md expr) =
  pureEvaluation `uncurry`
    case expr of
      ATOM a      -> (modul, evalAtom exprWithMD a)
      PROCEDURE p -> (modul, return $ WithMD md $ PROCEDURE p)
      QUOTE l     -> (modul, return $ WithMD md $ QUOTE l)
      LIST ls     -> (modul, evalOp exprWithMD ls)
      ENVEXPR m e -> (m, MT.withReaderT (changeModule m) (eval e))
      STOREENV e  -> (modul, return $ wrapInEnv modul e)


-- |evaluate an expression in pure context and extend environment
pureEvalExtend :: Module -> [(Name, WithMD Expr)] -> WithMD Expr -> Either Error (WithMD Expr)
pureEvalExtend modul evaluated_binds exprWithMD@(WithMD _ expr) =
  case expr of
    ENVEXPR m e ->
      let m' = m { getModEnv = M.fromList evaluated_binds `M.union` getModEnv m }
      in pureEvaluation m' $ MT.withReaderT (changeModule m') (eval e)
    _ -> pureEval (modul { getModEnv = M.fromList evaluated_binds `M.union` getModEnv modul }) exprWithMD


-- |evaluate an expression in a module in IO context
evalExpr :: Module -> (Name, WithMD Expr) -> IO (Either Error (WithMD Expr))
evalExpr modul (name, expr) = do
  res <- MT.runExceptT $ MT.runReaderT (eval expr) $ EvalState modul builtinsIO
  return $ case res of
    Left err -> Left err
    Right (WithMD md (PROCEDURE (Closure closure_env fun))) ->
      Right (WithMD md (PROCEDURE (Closure (addToClosureEnv (name, expr) closure_env) fun)))
    Right result -> Right result

addToClosureEnv :: (Name, WithMD Expr) -> Module -> Module
addToClosureEnv (name, expr) closure_env =
  closure_env { getModEnv = M.fromList [(name, expr)] `M.union` getModEnv closure_env }

-- |evaluate an expression in any context
-- PROCEDURE and QUOTE are returned without evaluation
eval :: Monad m => WithMD Expr -> Evaluation m Expr
eval exprWithMD@(WithMD md expr) = do
  modul <- liftM getModule ask
  case expr of
    ATOM a      -> evalAtom exprWithMD a
    PROCEDURE p -> return $ WithMD md $ PROCEDURE p
    QUOTE l     -> return $ WithMD md $ QUOTE l
    LIST ls     -> evalOp exprWithMD ls
    ENVEXPR m e -> MT.withReaderT (changeModule m) (eval e)
    STOREENV e  -> return $ wrapInEnv modul e

-- |evaluate an expression in pure context and extend environment
evalExtend :: Monad m => [(Name, WithMD Expr)] -> WithMD Expr -> Evaluation m Expr
evalExtend evaluated_binds exprWithMD@(WithMD _ expr) = do
  modul <- liftM getModule ask
  case expr of
    ENVEXPR m e ->
      let m' = m { getModEnv = M.fromList evaluated_binds `M.union` getModEnv m }
      in MT.withReaderT (changeModule m') (eval e)
    _ -> MT.withReaderT (changeModule (modul { getModEnv = M.fromList evaluated_binds `M.union` getModEnv modul })) $ eval exprWithMD



-- |get the environment from inside of a monad
getEnvironment :: Monad m => MEval m (Module, Env, Builtins m)
getEnvironment = do
  modul    <- liftM getModule ask
  env      <- liftM getEnv ask
  builtins <- liftM getBuiltins ask
  return (modul, env, builtins)

-- |evaluate a primitive expression
-- a Symbol is searched in environment and is evaluated
-- any other Atom is simply returned
evalAtom :: Monad m => WithMD Expr -> Atom -> Evaluation m Expr
evalAtom rootExpr@(WithMD md _) atom = do
  (modul, env, builtins) <- getEnvironment
  case atom of
    Symbol var -> case M.lookup var env of
      Just v  -> eval v
      Nothing -> case lookupInModule var modul of
        Just (v, vMod) -> MT.withReaderT  (changeModule vMod) (eval v)
        Nothing -> case M.lookup var builtins of
          Nothing -> throwErr (Just rootExpr) $ case M.lookup var ioBuiltins of
            Just _  -> "Cannot run IO action '" ++ var ++ "' in pure context."
            Nothing -> "Could not find " ++ show var ++ " in environment: " ++ show (fst <$> M.toList env)
          Just _  -> return $ WithMD md $ ATOM $ Symbol var -- |^this will be handled later
    other -> return $ WithMD md (ATOM other)

-- |evaluate a primitive expression
-- a Symbol is searched in environment and is evaluated
-- any other Atom is simply returned
--evalAtomOrSymbol :: Monad m => WithMD Expr -> Atom -> MEval m (Module, WithMD Expr)
evalAtomOrSymbol modul env (WithMD md _) atom = do
 -- (modul, env, _) <- getEnvironment
  case atom of
    Symbol var -> case M.lookup var env of
      Just v  -> return (modul, v)
      Nothing -> case lookupInModule var modul of
        Just (v, vMod) -> return (vMod, v)
        Nothing -> return (modul, WithMD md $ ATOM $ Symbol var) -- |^this will be handled later
    other -> return (modul, WithMD md $ ATOM other)



-- |evaluate a function application
evalOp :: Monad m => WithMD Expr -> [WithMD Expr] -> Evaluation m Expr
evalOp e@(WithMD md _) [] = return $ WithMD md $ ATOM Nil
evalOp exprWithMD (WithMD md operator:operands) = do
  builtins <- liftM getBuiltins ask
  -- |decide what to do based on the operator
  case operator of
    -- |call the function
    (PROCEDURE _)     -> case M.lookup ";evalProcedure" builtins of -- |^evaluate Procedure in either context
      Nothing -> throwErr (Just exprWithMD) "implementation bug: could no evaluate procedure. please report this"
      Just ep -> ep (WithMD md operator) operands
    -- |evaluate the operator first and try again
    l@(LIST _)        -> evalOp exprWithMD . (:operands) =<< eval (WithMD md l)
    -- |maybe it's a name for a function in the environment or a builtin function?
    (ATOM (Symbol s)) -> evalOpSymbol exprWithMD operands s
    (ENVEXPR m e)     -> MT.withReaderT (changeModule m) (eval $ wrapInEval e) >>= evalOp exprWithMD . (:operands)
    --(QUOTE e)         -> evalOp exprWithMD (e:operands) -- might not be appropriate, check
    other             -> throwErr (Just exprWithMD) $ show other ++ " is not a function"


-- |calls a function
evalProcedure :: Monad m => [WithMD Expr] -> WithMD Expr -> Evaluation m Expr
evalProcedure operands (WithMD md (PROCEDURE (Closure closure_env (WithMD _ (Fun (FunArgsList args) body))))) = do
  -- |evaluate arguments
  (argsName, evaluated_args) <- case args of
                                  ('~':n) -> return . (,) n    =<< mapM eval (fmap wrapInStoreEnv operands)
                                  _       -> return . (,) args =<< mapM eval operands
  let argsAsList = WithMD md $ QUOTE $ WithMD md $ LIST evaluated_args
  -- |extend environment with arguments
  let extended_env = M.fromList [(argsName, argsAsList)] `M.union` getModEnv closure_env
  -- |evaluate the function's body in the extended environment
  MT.withReaderT (changeEnv extended_env . changeModule closure_env) (eval (WithMD md body))
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
          let (argsZipped, remains) = zipWithRemains (,) args operands
          evaluated_args <- determineEvaluation eval (rest, wrapInListCall md remains) >>= \x -> mapM (determineEvaluation eval) argsZipped >>= \y -> return (x:y)
          -- |extend environment with arguments
          let extended_env = M.fromList evaluated_args `M.union` getModEnv closure_env
          -- |evaluate the function's body in the extended environment
          MT.withReaderT (changeEnv extended_env . changeModule closure_env) (eval (WithMD md body))
      Nothing ->
        if operands_length /= args_length -- |^check arity
        then
          throwErr (Just rootExpr) $ " arity problem: expecting " ++ show args_length ++ " arguments, got " ++ show operands_length
        else do
          -- |evaluate arguments
          evaluated_args <- mapM (determineEvaluation eval) $ zip args operands
          -- |extend environment with arguments
          let extended_env = M.fromList evaluated_args `M.union` getModEnv closure_env
          -- |evaluate the function's body in the extended environment
          MT.withReaderT (changeEnv extended_env . changeModule closure_env) (eval (WithMD md body))
-- not a procedure
evalProcedure _ rootExpr = throwErr (Just rootExpr) "not a procedure"


determineEvaluation evalF (('~':n), operand) = return . (,) n =<< evalF (wrapInStoreEnv operand)
determineEvaluation evalF (n, operand) = return . (,) n  =<< evalF operand


-- |calls a function in pure context
pureEvalProcedure :: [WithMD Expr] -> WithMD Expr -> PureEval (WithMD Expr)
pureEvalProcedure operands (WithMD md (PROCEDURE (Closure closure_env (WithMD _ (Fun (FunArgsList args) body))))) = do
  modul <- liftM getModule ask
  -- |evaluate arguments
  (argsName, evaluated_args) <- liftFromEither $ case args of
                                  ('~':n) -> return . (,) n    =<< seqParMap (pureEval modul) (fmap wrapInStoreEnv operands)
                                  _       -> return . (,) args =<< seqParMap (pureEval modul) operands
  let argsAsList = WithMD md $ QUOTE $ WithMD md $ LIST evaluated_args
  -- |extend environment with arguments
  let extended_env = M.fromList [(argsName, argsAsList)] `M.union` getModEnv closure_env
  -- |evaluate the function's body in the extended environment
  MT.withReaderT (changeEnv extended_env . changeModule closure_env) (eval (WithMD md body))
pureEvalProcedure operands rootExpr@(WithMD md (PROCEDURE (Closure closure_env (WithMD _ (Fun (FunArgs args mrest) body))))) =
  let (operands_length, args_length) = (length operands, length args)
  in
    case mrest of
      Just rest ->
        if operands_length < args_length -- |^check arity
        then
          throwErr (Just rootExpr) $ " arity problem: expecting at least " ++ show args_length ++ " arguments, got " ++ show operands_length
        else do
          modul <- liftM getModule ask
          -- |evaluate arguments
          let (argsZipped, remains) = zipWithRemains (,) args operands
          --evaluated_args <- liftFromEither (determineEvaluation (pureEval modul) (rest, wrapInList md     remains) >>= \x -> seqParMap (determineEvaluation (pureEval modul)) argsZipped >>= \y -> return (x:y))
          evaluated_args <- liftFromEither (determineEvaluation (pureEval modul) (rest, wrapInListCall md remains) >>= \x -> seqParMap (determineEvaluation (pureEval modul)) argsZipped >>= \y -> return (x:y))
          -- |extend environment with arguments
          let extended_env = M.fromList evaluated_args `M.union` getModEnv closure_env
          -- |evaluate the function's body in the extended environment
          MT.withReaderT (changeEnv extended_env . changeModule closure_env) (eval (WithMD md body))
      Nothing ->
        if operands_length /= args_length -- |^check arity
        then
          throwErr (Just rootExpr) $ " arity problem: expecting " ++ show args_length ++ " arguments, got " ++ show operands_length
        else do
          modul <- liftM getModule ask
          -- |evaluate arguments
          evaluated_args <- liftFromEither $ seqParMap (determineEvaluation (pureEval modul)) $ zip args operands
          -- |extend environment with arguments
          let extended_env = M.fromList evaluated_args `M.union` getModEnv closure_env
          -- |evaluate the function's body in the extended environment
          MT.withReaderT (changeEnv extended_env . changeModule closure_env) (eval (WithMD md body))
-- not a procedure
pureEvalProcedure _ rootExpr = throwErr (Just rootExpr) "not a procedure"



-- |bind a list of variable names and a rest variable with a list of expressions
zipWithRest :: Ord k => k -> Metadata -> [k] -> [WithMD Expr] -> M.Map k (WithMD Expr)
zipWithRest restVar md args ops =
  case zipWithRemains (,) args ops of
      (zipped, rest) -> M.fromList $ (restVar, WithMD md (LIST (WithMD md (ATOM $ Symbol "list") : rest))) : zipped

-- |zip and get a list of unzipped elements
zipWithRemains :: (a -> b -> c) -> [a] -> [b] -> ([c],[b])
zipWithRemains _ [] rest = ([], rest)
zipWithRemains _ _  []   = ([], [])
zipWithRemains f (x:xs) (y:ys) =
  case zipWithRemains f xs ys of
      (zipped, rest) -> (f x y : zipped, rest)


-- |tries to find symbol in environment and evaluate function call
evalOpSymbol :: Monad m => WithMD Expr -> [WithMD Expr] -> Name -> Evaluation m Expr
evalOpSymbol exprWithMD operands name = do
  (modul, env, builtins) <- getEnvironment
  case M.lookup name builtins of
    Just op -> op exprWithMD operands
    Nothing -> case M.lookup name ioBuiltins of
      Just _  -> throwErr (Just exprWithMD) $ "Cannot run IO action '" ++ name ++ "' in pure context."
      Nothing -> case M.lookup name env of
        Just v  -> eval v >>= \result -> evalOp exprWithMD (result : operands)
        Nothing -> case lookupInModule name modul of
          Nothing -> throwErr (Just exprWithMD) $ " Could not find " ++ show name ++ " in environment: " ++ show (fst <$> M.toList env) ++ unlines ("": listMods 0 modul)
          Just (v, vMod) -> do
            result <- MT.withReaderT (changeModule vMod) (eval v)
            MT.withReaderT (changeModule modul) $ evalOp exprWithMD (result : operands)

-------------
-- Builtins
-------------

-- |a type for built-in functions in any context
type Builtins m = M.Map Name (WithMD Expr -> [WithMD Expr] -> Evaluation m Expr)

-- |pure builtins in the identity monad
builtinsID :: Builtins MT.Identity
builtinsID = pureContextBuiltins

-- |all builtins in the IO monad
builtinsIO :: Builtins IO
builtinsIO = pureBuiltins `M.union` ioBuiltins


-- |pure builtins in an arbitrary monad
-- to add a new built-in function: add it to the map
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
    ,("mod", evalMod)
    ,("if", evalIf)
    ,("++", evalAppend)
    ,("zero?",      evalIs isZeroTest)
    ,("nil?",       evalIs isNilTest)
    ,("empty?",     evalIs isEmptyTest)
    ,("keyword?",   evalIs isKeywordTest)
    ,("string?",    evalIs isStringTest)
    ,("integer?",   evalIs isIntegerTest)
    ,("real?",      evalIs isRealTest)
    ,("number?",    evalIs isNumberTest)
    ,("procedure?", evalIs isFunTest)
    ,("list?",      evalIs isListTest)
    ,("=",      evalCompare (compareExprs (==)))
    ,("<>",     evalCompare (compareExprs (/=)))
    ,("<",      evalCompare (compareExprs (<)))
    ,(">",      evalCompare (compareExprs (>)))
    ,("<=",     evalCompare (compareExprs (<=)))
    ,(">=",     evalCompare (compareExprs (>=)))
    ,("str->words", evalStringSplit (strToList words))
    ,("str->lines", evalStringSplit (strToList lines))
    ,("words->str", evalStringMerge (listToStr unwords))
    ,("lines->str", evalStringMerge (listToStr unlines))
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
    ,("read-str", evalReadString)
    ,("let", evalLet)
    ,("letrec", evalLetrec)]


-- |redefining parallel built-in functions
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

-- |takes the value from an IO context
fromIO :: WithMD Expr -> IOEval Expr
fromIO (WithMD _ (QUOTE (WithMD _ (LIST [WithMD _ (ATOM (Symbol ";IO")), result])))) = lift $ return result
fromIO rootExpr = throwErr (Just rootExpr) "not an IO action"

-- |inserts a value into an IO context
returnIO :: (MonadTrans t, Monad m) => WithMD Expr -> t m (WithMD Expr)
returnIO expr@(WithMD md _) = lift $ return $ WithMD md $ QUOTE $ WithMD md $ LIST [WithMD md (ATOM (Symbol ";IO")), expr]

--------
-- IO
--------

-- |evaluate a 'pure' action and insert it into an IO context
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
  WithMD _ (LIST [WithMD _ (ATOM (Symbol "let!")), WithMD _ (ATOM (Symbol name)), expr]):rest -> do
    result <- eval expr >>= fromIO
    MT.withReaderT (updateEnv (M.insert name result)) (evalSequence rootExpr rest)
  [x]    -> eval x
  x:rest -> eval x >>= fromIO >> evalSequence rootExpr rest
  []     -> returnIO $ WithMD exprMD (ATOM Nil)


-- |evaluate a 'print!' IO action
evalPrint :: WithMD Expr -> [WithMD Expr] -> IOEval Expr
evalPrint _ [expr@(WithMD md _)] = do
  modul <- liftM getModule ask
  let evalled = pureEval modul expr
  case evalled of
    Right (WithMD _ (ATOM (String str))) -> lift (lift $ putStrLn str) >> returnIO (WithMD md $ ATOM Nil)
    Right result                         -> lift (lift $ print result) >> returnIO (WithMD md $ ATOM Nil)
    Left  err                            -> lift $ MT.throwE err
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
  modul <- liftM getModule ask
  let feOrFail = pureEval modul file >>= \f -> pureEval modul expr >>= \e -> return (f,e)
  (eFile, evalled) <- case feOrFail of
                        Left err -> lift $ MT.throwE err
                        Right rs -> return rs
  case (eFile, evalled) of
    (WithMD _ (ATOM (String wfile)), WithMD _ (ATOM (String str))) -> lift $ lift $ writeFile wfile str
    (WithMD _ (ATOM (String wfile)), _) -> do
      result <- lift $ lift ((writeFile wfile (show evalled) >> return Nothing) `catch` (\e -> return $ Just $ show (e :: IOException)))
      forM_ result $ throwErr (Just rootExpr)
    _ -> throwErr (Just eFile) "unexpected filepath"
  returnIO $ WithMD md $ ATOM Nil
evalPrintFile expr xs = throwErr (Just expr) $ "bad arity, expected 2 argument, got: " ++ show (length xs)

-- |evaluate a 'read!' IO action
evalReadFile :: WithMD Expr -> [WithMD Expr] -> IOEval Expr
evalReadFile expr@(WithMD md _) [file] = do
  eFile <- eval file
  input <- case eFile of
    (WithMD _ (ATOM (String rfile))) -> do
      result <- lift $ lift ((Right <$> readFile rfile) `catch` (\e -> return $ Left $ show (e :: IOException)))
      case result of
        Right x -> return x
        Left er -> throwErr (Just expr) er
    _                                -> throwErr (Just eFile) "unexpected filepath"
  returnIO $ WithMD md $ ATOM $ String input
evalReadFile expr xs = throwErr (Just expr) $ "bad arity, expected 1 arguments, got: " ++ show (length xs)

----------
-- Pure
----------


-- |evaluate a 'show' expression. converts an expression to string.
evalShow :: Monad m => WithMD Expr -> [WithMD Expr] -> Evaluation m Expr
evalShow rootExpr@(WithMD md _) = \case
  [expr] -> eval expr >>= lift . return . WithMD md . ATOM . String . show
  xs -> throwErr (Just rootExpr) $ "bad arity: expects 1 argument, got " ++ show (length xs)

-- |evaluate a 'trace' expression.
evalTrace :: Monad m => WithMD Expr -> [WithMD Expr] -> Evaluation m Expr
evalTrace rootExpr = \case
  [tr, expr] -> eval tr >>= flip traceShow (eval expr)
  xs -> throwErr (Just rootExpr) $ "bad arity: expects 2 arguments, got " ++ show (length xs)


-- |evaluate a 'try' expression. try will return the first expression that does not fail to evaluate
evalTry :: Monad m => WithMD Expr -> [WithMD Expr] -> Evaluation m Expr
evalTry rootExpr = go
  where go = \case
            []    -> throwErr (Just rootExpr) "bad arity: expects at least 1 argument, got 0"
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
  modul <- liftM getModule ask
  case exprs of
    -- |check arity
    [WithMD _ (ATOM (Symbol argName)), WithMD _ body] ->
      lift $ return $ WithMD exprMD $ PROCEDURE $ Closure modul $ WithMD exprMD $ Fun (FunArgsList argName) body
    [WithMD _ (LIST symbolList), WithMD _ body] -> do
      -- |check for duplicate argument names
      symbols <- liftFromEither $ seqParMap toSymbol symbolList
      if not $ null (duplicates symbols)
      then throwErr (Just rootExpr) "lambda arguments must have different names"
      -- |check for unexpected &
      else case validArgs symbols of
        (False, _) -> throwErr (Just rootExpr) "unexpected &. rest argument is not last"
        -- |return procedure
        (True, Nothing) -> lift $ return $ WithMD exprMD $ PROCEDURE $ Closure modul $ WithMD exprMD $ Fun (FunArgs symbols Nothing) body
        (True, Just vr) -> lift $ return $ WithMD exprMD $ PROCEDURE $ Closure modul $ WithMD exprMD $ Fun (FunArgs (init symbols) (Just vr)) body
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
              _  -> throwE $ Error (Just rootExpr) "unexpected binds in let expression"
    expr -> throwErr (Just expr) "second argument to let is not a list of bindings"
  ls -> throwErr (Just rootExpr) $ "let expects 2 arguments, got " ++ show (length ls)

-- |evaluate a 'let' expression
evalLet :: Monad m => WithMD Expr -> [WithMD Expr] -> Evaluation m Expr
evalLet rootExpr = \case
  -- |check arity
  [binders, body] -> case binders of
    -- |evaluate binds and evaluate body in extended environment
    WithMD _ (LIST binds) -> evalLetBinds binds >>= \evaluated_binds -> evalExtend evaluated_binds body
      where evalLetBinds = \case
    -- |^evaluate binds
              [] -> lift $ return []
              (WithMD _ (LIST [WithMD _ (ATOM (Symbol name)), expr]) :xs) -> eval expr >>= \result -> return . ((name, result):) =<< evalLetBinds xs
              _  -> throwErr (Just rootExpr) "unexpected binds in let expression"
    _ -> throwErr (Just rootExpr) "argument to let is not a list of bindings"
  ls -> throwErr (Just rootExpr) $ "let expects 2 arguments, got " ++ show (length ls)


-- |evaluate a pure 'let' expression in parallel
pureEvalLet :: WithMD Expr -> [WithMD Expr] -> PureEval (WithMD Expr)
pureEvalLet rootExpr operands = do
  modul <- liftM getModule ask
  case operands of
    -- |check arity
    [binders, body] -> case binders of
      -- |evaluate binds and evaluate body in extended environment
      WithMD _ (LIST binds) -> liftFromEither (seqParMap evalLetBind binds) >>= \evaluated_binds -> liftFromEither $ pureEvalExtend modul evaluated_binds body
        where evalLetBind = \case
      -- |^evaluate binds
                WithMD _ (LIST [WithMD _ (ATOM (Symbol name)), expr]) -> pureEval modul expr >>= \result -> return (name, result)
                _  -> Left $ Error (Just rootExpr) "unexpected binds in let expression"
      _ -> throwErr (Just rootExpr) "argument to let is not a list of bindings"
    ls -> throwErr (Just rootExpr) $ "let expects 2 arguments, got " ++ show (length ls)



-- |evaluate an 'if' expression. '#f' is false, everything else is true.
evalIf :: Monad m => WithMD Expr -> [WithMD Expr] -> Evaluation m Expr
evalIf rootExpr = \case
    [bool, trueBranch, falseBranch] ->
      eval (wrapInEval bool) >>= \case
        WithMD _ (ATOM (Bool False)) -> eval falseBranch
        _                            -> eval trueBranch
    xs -> throwErr (Just rootExpr) $ "bad arity, expected 3 arguments, got: " ++ show (length xs)

-- |evaluate compare expressions. such as '==', '<', '/=' etc.
evalCompare :: Monad m => (WithMD Expr-> Expr -> Expr -> MT.ExceptT Error m Bool) -> WithMD Expr -> [WithMD Expr] -> Evaluation m Expr
evalCompare test rootExpr@(WithMD exprMD _) operands = do
  -- |evaluate operands
  modul <- liftM getModule ask
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

-- | compare expressions by f
-- atoms will be compared regularily
-- lists will compare values in the same position
-- lists of different length will not be compared and an error will be thrown
compareExprs :: Monad m => (Atom -> Atom -> Bool) -> WithMD Expr -> Expr -> Expr -> MT.ExceptT Error m Bool
compareExprs  f _ (ATOM x@(Integer _)) (ATOM y@(Integer _)) = return $ f x y
compareExprs  f _ (ATOM x@(Real _)) (ATOM y@(Real _)) = return $ f x y
compareExprs  f _ (ATOM x@(String _)) (ATOM y@(String _)) = return $ f x y
compareExprs  f _ (ATOM x@(Bool _)) (ATOM y@(Bool _)) = return $ f x y
compareExprs  f _ (QUOTE (WithMD _ (ATOM x@(Symbol _)))) (QUOTE (WithMD _ (ATOM y@(Symbol _)))) = return $ f x y
compareExprs  f expr (QUOTE (WithMD _ (LIST xs))) (QUOTE (WithMD _ (LIST ys))) =
  if length xs == length ys
  then liftM and $ mapM (testCompare expr (compareExprs f)) $ zipWith (\x y -> [x,y]) xs ys
  else return False
compareExprs  _ _ _ _ = return False

-- |evaluate '...?' expressions
evalIs :: Monad m => (Expr -> Bool) -> WithMD Expr -> [WithMD Expr] -> Evaluation m Expr
evalIs test rootExpr@(WithMD exprMD _) = \case
  [expr] -> eval expr >>= \result ->
    getEnvironment >>= (\(modul,env,_) -> getList modul env result) >>= \x -> lift . return . WithMD exprMD . ATOM . Bool . test . stripMD $ case x of
                               (Left _ )   -> result
                               (Right res) -> res
  xs -> throwErr (Just rootExpr) $ "bad arity, expected 1 argument, got: " ++ show (length xs)

-- |
-- find the list if it has one inside the expression
--getList :: Monad m => WithMD Expr -> MT.ReaderT (EvalState m) (MT.ExceptT Error m) (Either Error (WithMD Expr))
getList modul env = \case
  e@(WithMD _ (LIST _)) -> return $ return e
  (WithMD _ (QUOTE e@(WithMD _ (LIST _)))) -> return $ return e
  (WithMD _ (QUOTE e@(WithMD _ (ENVEXPR _ _)))) -> getList modul env e
  (WithMD _ (ENVEXPR m (WithMD md (LIST ls)))) -> return $ return $ WithMD md $ LIST $ fmap (wrapInEnv m) ls
  (WithMD _ (ENVEXPR m (WithMD _  (QUOTE (WithMD md (LIST ls)))))) -> return $ return $ WithMD md $ LIST $ fmap (wrapInEnv m) ls
  e@(WithMD _ (ENVEXPR _ comp@(WithMD _ (ATOM a@(Symbol _))))) ->
    evalAtomOrSymbol modul env e a >>= \case
      (m, expr) -> if comp == expr then return (return e) else getList modul env $ wrapInEnv m expr
  e -> return $ Left $ Error (Just e) "not a list type"



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
isEmptyTest (LIST []) = True
isEmptyTest (QUOTE (WithMD _ (LIST []))) = True
isEmptyTest _ = False


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

-- |test symbol?
isKeywordTest :: Expr -> Bool
isKeywordTest (QUOTE (WithMD _ (ENVEXPR _ (WithMD _ (ATOM (Keyword _)))))) = True
isKeywordTest (QUOTE (WithMD _ (ATOM (Keyword _)))) = True
isKeywordTest _ = False

-- |test list?
isListTest :: Expr -> Bool
isListTest (QUOTE (WithMD _ (LIST _))) = True
isListTest (LIST _) = True
isListTest _ = False


-- |test procedure?
isFunTest :: Expr -> Bool
isFunTest = \case
  (ATOM (Symbol s)) -> case M.lookup s builtinsID of
                        Nothing -> False
                        Just _  -> True
  (PROCEDURE _) -> True
  _             -> False

-- |evaluate 'list' expression
evalList :: Monad m => WithMD Expr -> [WithMD Expr] -> Evaluation m Expr
evalList (WithMD exprMD _) elements = do
  evalled <- sequence $ fmap eval elements
  lift $ return $ WithMD exprMD $ QUOTE $ WithMD exprMD $ LIST evalled

-- |evaluate 'list' expression in pure context
pureEvalList :: WithMD Expr -> [WithMD Expr] -> PureEval (WithMD Expr)
pureEvalList (WithMD exprMD _) elements = do
  modul <- liftM getModule ask
  evalled <- liftFromEither $ seqParMap (pureEval modul) elements
  lift $ return $ WithMD exprMD $ QUOTE $ WithMD exprMD $ LIST evalled


-- |evaluate 'car' expression
evalCar :: Monad m => WithMD Expr -> [WithMD Expr] -> Evaluation m Expr
evalCar rootExpr = \case
  [expr] -> getEnvironment >>= (\(modul,env,_) -> tryEvalList modul env eval expr) >>= \case
      WithMD _ (LIST (x:_)) -> return x
      WithMD _ (LIST [])    -> throwErr (Just rootExpr) "car on empty list"
      x  -> throwErr (Just rootExpr) $ " cannot car a non-list type: " ++ show x
  xs   -> throwErr (Just rootExpr) $ "bad arity, expected 1 argument, got: " ++ show (length xs)

-- |evaluate 'cdr' expression
evalCdr :: Monad m => WithMD Expr -> [WithMD Expr] -> Evaluation m Expr
evalCdr rootExpr = \case
  [expr] -> getEnvironment >>= (\(modul,env,_) -> tryEvalList modul env eval expr) >>= \case
     WithMD md (LIST (_:xs))      -> lift $ return $ WithMD md $ QUOTE $ WithMD md $ LIST xs
     WithMD _      (LIST [])      -> throwErr (Just rootExpr) "cdr on empty list"
     _  -> throwErr (Just rootExpr) "cannot cdr a non-list type"
  xs        ->throwErr (Just rootExpr) $ "bad arity, expected 1 argument, got: " ++ show (length xs)

-- |evaluate a 'quote' expression. don't evaluate argument
evalQuote :: Monad m => WithMD Expr -> [WithMD Expr] -> Evaluation m Expr
evalQuote rootExpr@(WithMD exprMD _) = \case
  [e@(WithMD _ (LIST [WithMD _ (ATOM (Symbol "quote")), _]))] -> eval e
  [element] -> lift . return . WithMD exprMD . QUOTE =<< (replaceStoresInQuote element)
  xs        -> throwErr (Just rootExpr) $ "bad arity, expected 1 argument, got: " ++ show (length xs)


replaceStoresInQuote :: Monad m => WithMD Expr -> Evaluation m Expr
replaceStoresInQuote rootExpr@(WithMD md expr) =
  case expr of
    PROCEDURE p -> return $ WithMD md $ PROCEDURE p
    QUOTE l     -> return . WithMD md . QUOTE =<< replaceStoresInQuote l
    LIST ls     -> return . WithMD md . LIST  =<< mapM replaceStoresInQuote ls
    ENVEXPR m e -> return $ wrapInEnv m e
    STOREENV e  -> return . flip wrapInEnv e =<< liftM getModule ask
    ATOM a      ->   getEnvironment >>= (\(modul, env, _) -> evalAtomOrSymbol modul env (WithMD md expr) a) >>= \case
                    (_, e@(WithMD _ (ENVEXPR _ _))) -> return e
                    (m, e) -> return $ wrapInEnv m e


-- |evaluate 'eval' expression. evaluate QUOTE
evalEval :: Monad m => WithMD Expr -> [WithMD Expr] -> Evaluation m Expr
evalEval rootExpr = \case
  [element] -> do
    eval element >>= \case
      (WithMD _ (QUOTE e)) -> eval e
      (WithMD _ (ENVEXPR m e)) -> MT.withReaderT (changeModule m) (eval e)
      e -> return e
  xs        ->throwErr (Just rootExpr) $ "bad arity, expected 1 argument, got: " ++ show (length xs)


-- |converts a string to a quote
evalReadString :: Monad m => WithMD Expr -> [WithMD Expr] -> Evaluation m Expr
evalReadString rootExpr = \case
  [element] -> eval element >>= \case
    WithMD md (ATOM (String str)) -> either (throwErr (Just rootExpr)) (return . WithMD md . QUOTE) $ parseExpr (getMDSource md) str
    _                             -> throwErr (Just rootExpr) "read-string expects a string"
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

-- |(mod)
evalMod :: Monad m => WithMD Expr -> [WithMD Expr] -> Evaluation m Expr
evalMod rootExpr = evalArith (divide rootExpr mod) (const (MT.throwE $ Error (Just rootExpr) "cannot get modulo of real number")) rootExpr



-- |evaluate arithmetic expressions. converts integers to floats if an argument is a float.
evalArith :: Monad m => ([Integer] -> MT.ExceptT Error m Integer) -> ([Double] -> MT.ExceptT Error m Double) -> WithMD Expr -> [WithMD Expr] -> Evaluation m Expr
evalArith intOp realOp rootExpr@(WithMD exprMD _) operands = do
  modul   <- liftM getModule ask
  results <- liftFromEither $ seqParMap (evalToNumber modul rootExpr) operands
  if any isReal results
  then
    lift (liftM (WithMD exprMD . ATOM . Real) (realOp $ fmap atomToDouble results))
  else
    lift (liftM (WithMD exprMD . ATOM . Integer) (intOp $ fmap atomToInteger results))


-- |evaluate '++' expression for lists and strings.
evalAppend :: Monad m => WithMD Expr -> [WithMD Expr] -> Evaluation m Expr
evalAppend rootExpr@(WithMD exprMD _) operands =
  mapM (evalToList rootExpr) operands >>= \res -> case sequence res of
    Right results -> lift $ return $ WithMD exprMD $ QUOTE $ WithMD exprMD $ LIST $ myfold (++) [] results
    Left _ -> do
        result <- mapM (evalToString rootExpr) operands
        case sequence result of
          Right results -> lift $ return $ WithMD exprMD $ ATOM $ String $ foldl (++) "" results
          Left _ -> throwErr (Just rootExpr) "bad arguments to append. all arguments must be of type string or list."



-- |evaluate '++' expression for lists and strings.
pureEvalAppend :: WithMD Expr -> [WithMD Expr] -> PureEval (WithMD Expr)
pureEvalAppend rootExpr@(WithMD exprMD _) operands = do
  modul <- liftM getModule ask
  case seqParMap (pureEvalToList modul rootExpr) operands of
    Right results -> lift $ return $ WithMD exprMD $ QUOTE $ WithMD exprMD $ LIST $ concat results
    Left _ ->
        case seqParMap (pureEvalToString modul rootExpr) operands of
          Right results -> lift $ return $ WithMD exprMD $ ATOM $ String $ foldl (++) "" results
          Left _ -> throwErr (Just rootExpr) "bad arguments to append. all arguments must be of type string or list."

-- |test if atom is real
isReal :: Atom -> Bool
isReal = \case
  Real _ -> True
  _      -> False

-- |test if atom is a number
isNumber :: Atom -> Bool
isNumber = \case
  Integer _ -> True
  Real _    -> True
  _         -> False

-- |try converting an expression to a symbol
toSymbol :: WithMD Expr -> Either Error String
toSymbol (WithMD _ (ATOM (Symbol  s))) = return s
toSymbol (WithMD _ (ATOM (Keyword k))) = return k
toSymbol expr                         = Left $ Error (Just expr)  "expecting a symbol"

-- |eval a 'length' expression on strings and lists
evalLength :: Monad m => WithMD Expr -> [WithMD Expr] -> Evaluation m Expr
evalLength rootExpr@(WithMD exprMD _) operands =
  case operands of
    [x] -> evalToList rootExpr x >>= \case
      Right ls -> lift $ return $ WithMD exprMD $ ATOM $ Integer $ fromIntegral (length ls)
      Left _   -> evalToString rootExpr x >>= \case
        Right str -> lift $ return $ WithMD exprMD $ ATOM $ Integer $ fromIntegral (length str)
        Left e    -> lift $ MT.throwE e
    vs -> throwErr (Just rootExpr) $ " arity problem: expecting 1 argument, got " ++ show (length vs)

-- |eval a 'words' expression on a string
evalStringSplit :: Monad m => (Metadata -> String -> Expr) -> WithMD Expr -> [WithMD Expr] -> Evaluation m Expr
evalStringSplit f rootExpr@(WithMD exprMD _) operands =
  case operands of
    [x] -> evalToString rootExpr x >>= \case
      Right str -> lift $ return $ WithMD exprMD $ f exprMD str
      Left e    -> lift $ MT.throwE e
    vs -> throwErr (Just rootExpr) $ " arity problem: expecting 1 argument, got " ++ show (length vs)


strToList :: (String -> [String]) -> Metadata -> String -> Expr
strToList f exprMD str = QUOTE $ WithMD exprMD $ LIST $ map (WithMD exprMD . ATOM . String) (f str)

listToStr:: ([String] -> String) -> [String] -> Expr
listToStr f ls = ATOM $ String (f ls)

-- |eval a 'words' or 'lines' expression on a string
evalStringMerge :: Monad m => ([String] -> Expr) -> WithMD Expr -> [WithMD Expr] -> Evaluation m Expr
evalStringMerge f rootExpr@(WithMD exprMD _) operands = do
  modul <- liftM getModule ask
  case operands of
    [x] -> evalToList rootExpr x >>= \case
      Left  e  -> lift $ MT.throwE e
      Right ls -> case seqParMap (pureEvalToString modul rootExpr) ls of
        Right sls -> lift $ return $ WithMD exprMD $ f sls
        Left e    -> lift $ MT.throwE e
    vs -> throwErr (Just rootExpr) $ " arity problem: expecting 1 argument, got " ++ show (length vs)





-- |slice: a slice of a string or list
evalSlice :: Monad m => WithMD Expr -> [WithMD Expr] -> Evaluation m Expr
evalSlice rootExpr@(WithMD exprMD _) operands = do
  modul <- liftM getModule ask
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

-- |evaluate an expression to an Integer
evalAndGetInt :: (MonadTrans t, Monad (t (MT.ExceptT Error m)), Monad m) => Module -> WithMD Expr -> WithMD Expr -> t (MT.ExceptT Error m) (Either Error Integer)
evalAndGetInt modul rootExpr e = do
  r <- case evalToNumber modul rootExpr e of
         Right x -> return x
         Left er -> lift $ MT.throwE er
  lift $ return $ castAtomToInteger rootExpr r

-- |run 3 actions in an arbitrary monad
sequence3 :: Monad m => (m a, m b, m c) -> m (a, b, c)
sequence3 (x,y,z) = do
  r1 <- x
  r2 <- y
  r3 <- z
  return (r1, r2, r3)

-- |cast an atom to an integer
castAtomToInteger :: WithMD Expr -> Atom -> Either Error Integer
castAtomToInteger _ (Integer i) = return i
castAtomToInteger e _           = Left $ Error (Just e) "Not an integer"

-- |unsafely cast an atom to an integer
-- dangerous!!!
atomToInteger :: Atom -> Integer
atomToInteger (Integer i) = i
atomToInteger x           = error $ "trying to cast an atom to an intger. implementation error, should not happend: " ++ show x

-- |unsafely cast an atom to a double
-- dangerous!!!
atomToDouble :: Atom -> Double
atomToDouble (Real d) = d
atomToDouble (Integer i) = fromIntegral i
atomToDouble x           = error $ "trying to cast an atom to a double. implementation error, should not happend: " ++ show x

-- |an evaluation of an something in pure context
pureEvaluation :: Show a => Module -> PureEval a -> Either Error a
pureEvaluation m go =
  MT.runIdentity $ MT.runExceptT $ MT.runReaderT go (EvalState m pureContextBuiltins)

-- |insert either to monadic context
liftFromEither :: Monad m => Either Error a -> MEval m a
liftFromEither = \case
  Left err -> lift $ MT.throwE err
  Right rs -> return rs

-- |try to evaluate expression to a number
evalToNumber :: Module -> WithMD Expr -> WithMD Expr -> Either Error Atom
evalToNumber modul rootExpr expr =
    case pureEval modul (wrapInEval expr) of
      Right (WithMD _ (ATOM a)) -> if isNumber a then return a else Left $ Error (Just rootExpr) $ show a ++ " not a number"
      Right _        -> Left $ Error (Just rootExpr) $ show expr ++ " not a number"
      Left x -> Left x

-- |try to evaluate expression to a string
evalToString :: Monad m => WithMD Expr -> WithMD Expr -> MT.ReaderT (EvalState m) (MT.ExceptT Error m) (Either Error String)
evalToString rootExpr expr = eval (wrapInEval expr) >>= \case
  (WithMD _ (ATOM (String str))) -> lift $ return $ Right str
  _ -> lift $ return $ Left $ Error (Just rootExpr) $ show expr ++ " is not a string"

-- |try to evaluate expression to a list
evalToList :: Monad m => WithMD Expr -> WithMD Expr -> MT.ReaderT (EvalState m) (MT.ExceptT Error m) (Either Error [WithMD Expr])
evalToList rootExpr expr =
  getEnvironment >>= (\(modul,env,_) -> tryEvalList modul env eval expr) >>= \case
    (WithMD _ (LIST list)) -> lift $ return $ Right list
    _ -> lift $ return $ Left $ Error (Just rootExpr) $ show expr ++ " is not a list"

tryEvalList modul env evalF expr = do
    result <- evalF expr
    getListResult <- getList modul env result
    return $ case getListResult of
               (Left _)  -> result
               (Right y) -> y


-- |try to evaluate expression to a string
pureEvalToString :: Module -> WithMD Expr -> WithMD Expr -> Either Error String
pureEvalToString modul rootExpr expr = pureEval modul (wrapInEval expr) >>= \case
  (WithMD _ (ATOM (String str))) -> Right str
  _ -> Left $ Error (Just rootExpr) $ show expr ++ " is not a string"

-- |try to evaluate expression to a list
pureEvalToList :: Module -> WithMD Expr -> WithMD Expr -> Either Error [WithMD Expr]
pureEvalToList modul rootExpr expr = tryEvalList modul (getModEnv modul) (pureEval modul) expr >>= \case
  (WithMD _ (LIST list)) -> Right list
  _ -> Left $ Error (Just rootExpr) $ show expr ++ " is not a list"
