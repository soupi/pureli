
-- |
-- a utility module
module Pureli.Utils where

import Data.List (group, sort)
import qualified Control.Monad.Trans.Except as MT
import qualified Control.Monad.Trans.Class  as MT
import System.Info (os)
import qualified System.FilePath.Posix   as P (takeDirectory, takeFileName)
import qualified System.FilePath.Windows as W (takeDirectory, takeFileName)
import qualified System.Console.ANSI as ANSI

import Pureli.AST
import Pureli.Printer()

-- |
-- get relative system functions (windows or posix)
systemFuncs :: (FilePath -> FilePath, FilePath -> FilePath)
systemFuncs =
  if os == "mingw32"
  then (W.takeDirectory,W.takeFileName)
  else (P.takeDirectory,P.takeFileName)

-- |
-- Error might contain the problematic expression with it's metadata and an error message.
data Error = Error (Maybe (WithMD Expr)) String

-- |
-- How to print errors.
instance Show Error where
  show (Error Nothing str) = withRed "*** Error: " ++ str
  show (Error (Just (WithMD md expr)) str) = withRed "*** Error: " ++ show md ++ ": " ++ str ++ withRed "\n***" ++ " In expression: " ++ withBlue (show expr)

-- |
-- utility to throw an expression.
throwErr :: (MT.MonadTrans t, Monad m) => Maybe (WithMD Expr) -> String -> t (MT.ExceptT Error m) a
throwErr expr err =  MT.lift $ MT.throwE $ Error expr err

eitherToErr :: (MT.MonadTrans t, Monad m) => Either String a -> t (MT.ExceptT Error m) a
eitherToErr (Left str) = throwErr Nothing str
eitherToErr (Right x)  = MT.lift $ return x

-- |
-- if Maybe b is nothing, replace it with Left a. otherwise: Right b
maybeToEither :: a -> Maybe b -> Either a b
maybeToEither _ (Just y) = Right y
maybeToEither x Nothing  = Left x

joinSecondEither :: (a -> Either b c) -> (d, a) -> Either b (d, c)
joinSecondEither f (y,x) = case f x of
  Right z -> Right (y,z)
  Left  z -> Left z


-- |
-- sub a list.
sub :: Num a => [a] -> a
sub [] = 0
sub (x:xs) = myfold (-) x xs

-- |
-- sum a list.
add :: Num a => [a] -> a
add = myfold (+) 0

-- |
-- mul a list.
mul :: Num a => [a] -> a
mul = myfold (*) 1

-- |
-- my tail recursive fold
myfold :: (a -> a -> a) -> a -> [a] -> a
myfold _ res []     = res
myfold f res (x:xs) = myfold f (f res x) xs

-- |
-- divide on term from the next in a row. fails on divide by zero.
divide :: Monad m => (Eq a, Num a) => WithMD Expr -> (a -> a -> a) -> [a] -> MT.ExceptT Error m a
divide _ _ []                 = return 1
divide _ _ [x]                = return x
divide rootExpr _ (_:0:_)     = MT.throwE $ Error (Just rootExpr) "cannot divide by zero"
divide rootExpr op (x:y:rest) = divide rootExpr op $ op x y : rest



-- |
-- returns the duplicates of a list.
duplicates :: Ord a => [a] -> [a]
duplicates = map head . filter ((>1) . length) . group . sort

-- |
-- split arguments by element
splitBy :: Eq a => a -> [a] -> [[a]]
splitBy v vs = map reverse $ go [] vs
  where go xs [] = [xs]
        go xs (y:ys)
          | y == v    = xs : go [] ys
          | otherwise = go (y:xs) ys


-- |
-- check if there is at most one &rest argument and the end of a list of arguments
validArgs :: [String] -> (Bool, Maybe String)
validArgs [] = (True, Nothing)
validArgs (x:xs) = case x of
  ('&':var) -> if null xs then (True, Just var) else (False, Nothing)
  _ -> validArgs xs


----------------
-- Formatting
---------------


-- |
-- list all modules
listMods :: Int -> Module -> [String]
listMods _ _ = []

listMods' :: Int -> Module -> [String]
listMods' n modul =
  let m  = getModName modul
      ms = fmap ((replicate (n+2) ' ' ++) . getModName) (getModImports modul)
  in
    ((replicate n ' ' ++ m) : ms ++ concatMap (listMods' n) (getModImports modul))


displayCommands :: String -> [(String, String)] -> [String]
displayCommands opener cmds = fmap (displayCommand opener (cmdWithSpaces n)) cmds
  where n = 8 + foldr (\str acc -> max (length (fst str)) acc) 0 cmds

cmdWithSpaces :: Int -> String -> String
cmdWithSpaces n str = str ++ replicate (n - length str) ' '

displayCommand :: String -> (String -> String) -> (String, String) -> String
displayCommand opener paddCmd (cmd, desc) = opener ++ withGreen (paddCmd cmd) ++ desc

dashOpener, spaceOpener :: String
dashOpener  = "  - "
spaceOpener = "    "


---------------------
-- ast combinators
---------------------

-- |
-- takes an expression and creates a closure from it
wrapInEnv :: Module -> WithMD Expr -> WithMD Expr
wrapInEnv _ e@(WithMD _ (PROCEDURE _)) = e
wrapInEnv _ e@(WithMD _ (ENVEXPR _ _)) = e
wrapInEnv m   (WithMD md (STOREENV e)) = WithMD md $ ENVEXPR m e
wrapInEnv m e@(WithMD md (ATOM (Symbol _))) = WithMD md $ ENVEXPR m e
wrapInEnv _ e@(WithMD _  (ATOM _)) = e
wrapInEnv m e@(WithMD md _) = WithMD md $ ENVEXPR m e


wrapInStoreEnv :: WithMD Expr -> WithMD Expr
wrapInStoreEnv e@(WithMD _ (ENVEXPR _ _)) = e
wrapInStoreEnv e@(WithMD _ (STOREENV _))  = e
wrapInStoreEnv e@(WithMD md _) = fmap (STOREENV . WithMD md) e

wrapInList :: Metadata -> [WithMD Expr] -> WithMD Expr
wrapInList md es = WithMD md $ QUOTE $ WithMD md $ LIST es

wrapInListCall :: Metadata -> [WithMD Expr] -> WithMD Expr
wrapInListCall md es = WithMD md $ LIST (WithMD md (ATOM $ Symbol "list") : es)

wrapInLet :: [(WithMD Expr, WithMD Expr)] -> WithMD Expr -> WithMD Expr
wrapInLet binders body@(WithMD md _) =
  wrapInList md [WithMD md $ ATOM $ Symbol "let", wrapInList md $ fmap (wrapInList md . (\(x,y) -> [x,y]) . fmap wrapInStoreEnv) binders, body]


wrapInEval :: WithMD Expr -> WithMD Expr
wrapInEval e@(WithMD md _) = WithMD md $ LIST [WithMD md $ ATOM $ Symbol "eval", e]

-- Colors

withColor :: ANSI.Color -> String -> String
withColor color string = do
  concat
   [ANSI.setSGRCode [ANSI.SetColor ANSI.Foreground ANSI.Vivid color]
   ,string
   ,ANSI.setSGRCode [ANSI.Reset]
   ]

withRed, withBlue, withYellow, withGreen, withCyan :: String -> String
withRed    = withColor ANSI.Red
withBlue   = withColor ANSI.Blue
withCyan   = withColor ANSI.Cyan
withYellow = withColor ANSI.Yellow
withGreen  = withColor ANSI.Green
