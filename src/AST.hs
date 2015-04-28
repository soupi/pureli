-- |
-- The AST definition for the language

{-# LANGUAGE DeriveGeneric #-}

module AST where

import qualified Data.Map    as M
import qualified Text.Parsec as P  (SourcePos)
import System.IO             as IO (FilePath)
import Control.DeepSeq             (NFData)
import GHC.Generics                (Generic)

------------------
-- definitions
------------------


data ModuleDef = ModuleDef { modFile       :: IO.FilePath
                           , modName       :: Atom
                           , modExposes    :: Maybe [WithMD Atom]
                           , modRequires   :: [Require]
                           , modMacros     :: [(Name, WithMD Expr)]
                           , modDefs       :: [(Name, WithMD Expr)]
                           }

-- |
-- A module definition
data Module = Module { getModFile           :: Name
                     , getModName           :: Name
                     , getModImports        :: [Module]
                     , getModExports        :: Env
                     , getModExportedMacros :: Env
                     , getModMacros         :: Env
                     , getModEnv            :: Env }

-- |
-- A require for a module.
data Require = Require FilePath Name (Maybe Name) (Maybe [Name]) deriving (Show, Eq, Ord)

-- |
-- An environment for the interpreter.
--  used to find binded names and expression which were bound with let, letrec, let!, define or defmacro
type Env = M.Map Name (WithMD Expr)

-- |
-- holds metadata on the type
data WithMD a = WithMD Metadata a deriving (Eq, Generic)

removeMD :: WithMD a -> a
removeMD (WithMD _ x) = x
-- |
-- metadata, current holds the position in the interpreted file
type Metadata = P.SourcePos

-- |
-- an alias for String
type Name =  String

-- |
-- a function with the environment it had when interpreted - for lexical scoping
data Closure = Closure Env (WithMD Fun) deriving (Eq, Ord)

-- |
-- a function is a list of argument names, maybe an additional 'rest' argument and a body
data Fun = Fun [Name] (Maybe Name) Expr deriving (Eq, Ord)

-- |
-- the main expression for our language
data Expr = LIST [WithMD Expr]  -- ^a list of expressions
          | QUOTE (WithMD Expr) -- ^quoted expression
          | ATOM Atom           -- ^a primitive expression
          | PROCEDURE Closure   -- ^a procedure - a closure
          deriving (Eq, Ord, Generic)

-- |
-- primitives of the language
data Atom = Integer Integer
          | Real Double
          | Symbol Name
          | String String
          | Bool Bool
          | Nil deriving (Eq, Ord, Generic)

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

-- |
-- I'm sure I had a reason to do that........
instance Eq a => Ord (WithMD a) where
  compare _ _ = EQ
