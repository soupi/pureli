{-# LANGUAGE LambdaCase #-}

-- |a way to print expressions
module Pureli.Printer where

import qualified Data.Map as M (toList)

import Pureli.AST

---------------
-- Instances
---------------

instance Show Module where
  show = showModule

instance Show ModuleDef where
  show = showModuleDef

instance Show Atom where
  show = showAtom 0

instance Show Expr where
  show = showExpr 0

instance Show Closure where
  show = showClosure 0

instance Show Fun where
  show = showFun 0

instance Show a => Show (WithMD a) where
  show = showWithMD 0



---------------
-- Functions
---------------


getIndent :: Int -> String
getIndent indent = foldr (++) "" $ replicate indent "  "

-- |convert an atom to a string.
showAtom :: Int -> Atom -> String
showAtom indent atom = getIndent indent ++ case atom of
  Nil        -> "nil"
  Integer x  -> show x
  Real    x  -> show x
  String  x  -> show x
  Bool True  -> "#t"
  Bool False -> "#f"
  Symbol  x  -> x
  Keyword x  -> (':':x)

-- |convert an expression to a string.
showExpr ::  Int -> Expr -> String
showExpr indent expr = getIndent indent ++ case expr of
  QUOTE x@(WithMD _ (LIST _)) -> show x
  LIST xs     -> "(" ++ showListElements xs ++ ")"
  QUOTE x     -> "'" ++ show x
  ATOM a      -> show a
  PROCEDURE x -> show x
  ENVEXPR _ x -> "~" ++ show x
  STOREENV x  -> "(;storenv " ++ show x ++ ")"



-- |convert a closure to a string.
showClosure :: Int -> Closure -> String
showClosure indent (Closure _ x) = getIndent indent ++ show x

-- |convert a function to a string.
showFun ::  Int -> Fun -> String
showFun indent (Fun (FunArgsList arg) body) = getIndent indent ++ "(lambda " ++ arg ++ " " ++ show body ++ ")"
showFun indent (Fun (FunArgs args mn) body) = getIndent indent ++ "(lambda (" ++ showListStrings (args ++ [maybe "" ('&':) mn]) ++ ") " ++ show body ++ ")"

-- |convert a type with metadata to a string.
showWithMD ::  Show a => Int -> WithMD a -> String
showWithMD indent (WithMD _ x) = getIndent indent ++ show x

-- |strings separated by space.
showListStrings :: [String] -> String
showListStrings [] = ""
showListStrings [x] = x
showListStrings (x:y:xs) = x ++ " " ++ showListStrings (y:xs)

-- |showable types separated by space.
showListElements :: Show a => [a] -> String
showListElements = showListStrings . fmap show

-- |convert a define to a string.
showDefine :: Name -> (Name, WithMD Expr) -> String
showDefine kind (name, WithMD _ (LIST [WithMD _ (ATOM (Symbol "lambda")), WithMD _ (LIST args), body])) =
  unlines ["(" ++ kind ++ " " ++ name ++ " [" ++ showListElements args ++ "]"
          ,"  " ++ showExpr 1 (stripMD body) ++ ")"]
showDefine kind (name, WithMD _ expr) =
  unlines ["(" ++ kind ++ " " ++ name
          ,showExpr 1 expr ++ ")"]


-- |convert a module definition to a string.
showModuleDef :: ModuleDef -> String
showModuleDef modu = unlines
    ["(module " ++ show (modName modu) ++ ")"
    ,""
    ,defs modMacros "defmacro"
    ,""
    ,""
    ,defs modDefs "define"]
  where defs f k = unlines $ map (showDefine k) (f modu)

-- |convert a module to a string.
showModule :: Module -> String
showModule modu = unlines
    ["(module " ++ show (getModName modu) ++ ")"
    ,""
    ,defs (M.toList . getModMacros) "defmacro"
    ,""
    ,""
    ,defs (M.toList . getModEnv) "define"]
  where defs f k = unlines $ fmap (showDefine k) (f modu)

-- |
-- convert a data module to a string.
showdataModule :: Module -> String
showdataModule m =
  unlines $ fmap ($m)
  [getModFile
  ,getModName
  ,show . fmap showdataModule . getModImports
  ,show . getModExports
  ,show . getModExportedMacros
  ,show . getModMacros
  ,show . getModEnv]
