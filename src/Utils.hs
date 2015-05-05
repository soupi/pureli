
-- |
-- a utility module
module Utils where

import Data.List (group, sort)
import qualified Control.Monad.Trans.Except as MT
import qualified Control.Monad.Trans.Class  as MT

import AST
import Printer()

-- |
-- Error might contain the problematic expression with it's metadata and an error message.
data Error = Error (Maybe (WithMD Expr)) String

-- |
-- How to print errors.
instance Show Error where
  show (Error Nothing str) = "Error: " ++ str
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
-- substitute on term from the next in a row.
sub :: Num a => [a] -> a
sub [] = 0
sub xs = foldl1 (-) xs

-- |
-- divide on term from the next in a row. fails on divide by zero.
divide :: Monad m => (Eq a, Num a) => (a -> a -> a) -> [a] -> MT.ExceptT Error m a
divide _ []          = return 1
divide _ [x]         = return x
divide _ (_:0:_)     = MT.throwE $ Error Nothing "cannot divide by zero"
divide op (x:y:rest) = divide op $ op x y : rest

-- |
-- returns the duplicates of a list.
duplicates :: Ord a => [a] -> [a]
duplicates = map head . filter ((>1) . length) . group . sort

-- |
-- strip from metadata
stripMD :: WithMD a -> a
stripMD (WithMD _ x) = x

splitBy :: Eq a => a -> [a] -> [[a]]
splitBy v vs = map reverse $ go [] vs
  where go xs [] = [xs]
        go xs (y:ys)
          | y == v    = xs : go [] ys
          | otherwise = go (y:xs) ys


validArgs :: [String] -> (Bool, Maybe String)
validArgs [] = (True, Nothing)
validArgs (x:xs) = case x of
  ('&':var) -> if null xs then (True, Just var) else (False, Nothing)
  _ -> validArgs xs

