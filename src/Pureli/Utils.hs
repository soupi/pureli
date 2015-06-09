
-- |
-- a utility module
module Pureli.Utils where

import Data.List (group, sort)
import qualified Control.Monad.Trans.Except as MT
import qualified Control.Monad.Trans.Class  as MT
import System.Info (os)
import qualified System.FilePath.Posix   as P (takeDirectory, takeFileName)
import qualified System.FilePath.Windows as W (takeDirectory, takeFileName)

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
  show (Error Nothing str) = "*** Error: " ++ str
  show (Error (Just (WithMD md expr)) str) = "*** Error: " ++ show md ++ ": " ++ str ++ "\n*** In expression: " ++ show expr

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
listMods n modul =
  let m  = getModName modul
      ms = fmap ((replicate (n+2) ' ' ++) . getModName) (getModImports modul)
  in
    ((replicate n ' ' ++ m) : ms ++ concatMap (listMods n) (getModImports modul))


displayCommands :: String -> [(String, String)] -> [String]
displayCommands opener cmds = fmap (displayCommand opener (cmdWithSpaces n)) cmds
  where n = 8 + foldr (\str acc -> max (length (fst str)) acc) 0 cmds

cmdWithSpaces :: Int -> String -> String
cmdWithSpaces n str = str ++ replicate (n - length str) ' '

displayCommand :: String -> (String -> String) -> (String, String) -> String
displayCommand opener paddCmd (cmd, desc) = opener ++ paddCmd cmd ++ desc

dashOpener, spaceOpener :: String
dashOpener  = "  - "
spaceOpener = "    "
