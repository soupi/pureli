{-# LANGUAGE LambdaCase #-}

-- |a way to print expressions
module Language.Pureli.Printer where

import qualified Data.Map as M

import Language.Pureli.AST


class Printer a where
  printer :: a -> String

---------------
-- Instances
---------------

instance Printer Module where
  printer = showModule

instance Printer ModuleDef where
  printer = showModuleDef

instance Printer Atom where
  printer = showAtom 0

instance Printer Expr where
  printer = showExpr 0

instance Printer Closure where
  printer = showClosure 0

instance Printer Fun where
  printer = showFun 0

instance Printer a => Printer (WithMD a) where
  printer = showWithMD 0

instance (Show a, Printer b) => Printer (M.Map a b) where
  printer =
      (\list -> "[" ++ unlines list ++ "]")
    . fmap (\(x,y) -> "(" ++ show x ++ " -> " ++ printer y ++ " )")
    . M.toList

---------------
-- Functions
---------------


getIndent :: Int -> String
getIndent indent = concat $ replicate indent "  "

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
  Keyword x  -> ':':x

-- |convert an expression to a string.
showExpr ::  Int -> Expr -> String
showExpr indent expr = getIndent indent ++ case expr of
  QUOTE x@(WithMD _ (LIST _)) -> printer x
  LIST xs     -> "(" ++ showListElements xs ++ ")"
  QUOTE x     -> "'" ++ printer x
  ATOM a      -> printer a
  PROCEDURE x -> printer x
  ENVEXPR _ x -> "~" ++ printer x
  STOREENV x  -> "(;storenv " ++ printer x ++ ")"
  IOResult x  -> "<IO: " ++ printer x ++ ">"



-- |convert a closure to a string.
showClosure :: Int -> Closure -> String
showClosure indent (Closure _ x) = getIndent indent ++ printer x

-- |convert a function to a string.
showFun ::  Int -> Fun -> String
showFun indent (Fun (FunArgsList arg) body) = getIndent indent ++ "(lambda " ++ arg ++ " " ++ printer body ++ ")"
showFun indent (Fun (FunArgs args mn) body) = getIndent indent ++ "(lambda (" ++ showListStrings (args ++ [maybe "" ('&':) mn]) ++ ") " ++ printer body ++ ")"

-- |convert a type with metadata to a string.
showWithMD ::  Printer a => Int -> WithMD a -> String
showWithMD indent (WithMD _ x) = getIndent indent ++ printer x

-- |strings separated by space.
showListStrings :: [String] -> String
showListStrings [] = ""
showListStrings [x] = x
showListStrings (x:y:xs) = x ++ " " ++ showListStrings (y:xs)

-- |showable types separated by space.
showListElements :: Printer a => [a] -> String
showListElements = showListStrings . fmap printer

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
    ,""
    ,defs modDefs "define"]
  where defs f k = unlines $ map (showDefine k) (f modu)

-- |convert a module to a string.
showModule :: Module -> String
showModule modu = unlines
    ["(module " ++ show (getModName modu) ++ ")"
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
  ,printer . getModExports
  ,printer . getModEnv]
