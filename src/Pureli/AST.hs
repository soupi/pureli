
{-# LANGUAGE DeriveGeneric #-}

-- |
-- The AST definition for the language
module Pureli.AST where

import qualified Data.Map    as M
import qualified Text.Parsec as P  (SourcePos)
import System.IO             as IO (FilePath)
import Control.DeepSeq             (NFData)
import GHC.Generics                (Generic)

------------------
-- definitions
------------------

-- |
-- a possible repl expression
data ReqDefExp = Req Require | Def (Name, WithMD Expr) | Exp (WithMD Expr)


-- a definition of a module read from parser
data ModuleDef = ModuleDef { modFile       :: IO.FilePath
                           , modName       :: Name
                           , modExposes    :: Maybe [Name]
                           , modRequires   :: [Require]
                           , modMacros     :: [(Name, WithMD Expr)]
                           , modDefs       :: [(Name, WithMD Expr)]
                           }

-- |
-- A module definition and environment, created from ModuleDef
data Module = Module { getModFile           :: IO.FilePath
                     , getModName           :: Name
                     , getModImports        :: [Module]
                     , getModExports        :: Env
                     , getModExportedMacros :: Env
                     , getModMacros         :: Env
                     , getModEnv            :: Env } deriving (Eq, Generic)

-- |
-- A require for a module.
data Require = Require FilePath Name (Maybe Name) (Maybe [Name]) deriving (Show, Eq, Ord)

-- |
-- An environment for the interpreter.
-- used to find binded names and expression which were bound with let, letrec, let!, define or defmacro
type Env = M.Map Name (WithMD Expr)

-- |
-- metadata, current holds the position in the interpreted file
type Metadata = P.SourcePos

-- |
-- holds metadata on the type
data WithMD a = WithMD Metadata a deriving (Eq, Generic)

-- |
-- remove metadata
stripMD :: WithMD a -> a
stripMD (WithMD _ x) = x

-- |
-- an alias for String
type Name =  String

-- |
-- a function with the environment it had when interpreted - for lexical scoping
data Closure = Closure Module (WithMD Fun) deriving (Eq)

-- |
-- a function is a list of argument names, maybe an additional 'rest' argument and a body
data Fun = Fun FunArgs Expr deriving (Eq)

-- |
-- arguments for a function
-- either it is a list of names arguments can be bind to on by one and a maybe a rest name to bind rest
-- or a name to bind all arguments in a list
data FunArgs = FunArgs [Name] (Maybe Name) | FunArgsList Name deriving (Eq)

-- |
-- the main expression for our language
data Expr = LIST [WithMD Expr]  -- ^a list of expressions
          | QUOTE (WithMD Expr) -- ^quoted expression
          | ATOM Atom           -- ^a primitive expression
          | PROCEDURE Closure   -- ^a procedure - a closure
          deriving (Eq, Generic)

-- |
-- primitives of the language
data Atom = Integer Integer
          | Real Double
          | Symbol Name
          | String String
          | Bool Bool
          | Nil deriving (Eq, Ord, Generic)

-- |
-- defines how to call elements in modules syntactically
moduleSplitter :: Char
moduleSplitter = '/'


----------------
-- instances
----------------

instance NFData Expr
instance NFData Atom
instance NFData (WithMD a)

-- |
-- map a function on the underline type, keeps the metadata
instance Functor WithMD where
  fmap f (WithMD md x) = WithMD md $ f x

{-
-- |
-- I'm sure I had a reason to do that........
instance Eq a => Ord (WithMD a) where
  compare _ _ = EQ
-}
