{-|
Module      : Expr
Description : Data types of expressions
Copyright   : (c) Grigoriy Bokov, 2018
                  Gleb Kalachev, 2018
License     : GPL-3
Maintainer  : bokov@intsys.msu.ru
Stability   : experimental
Portability : POSIX

Expressions are built up from terminal expressions using brackets, braces, commas, etc. Terminal expressions are either terminal symbols (logical symbol, program variables, function's calls, etc.), entries (pointers, referenses, etc.) or booleans (logical constants, boolean expressions, etc.).

There are two classes of expressions: logical and program. Logical expressions represent logical terms that are elements of inner language of the system. Program expressions represent program terms that are elements of outer language of the system. The type @Expr@ is used for denoting common parts of logical and program expressions.

The type @Expr'@ denotes the common type of expressions without detalization of the types of their values. In order to reduce error detection to parsing-level, a class of expressions with the same type of values can be isolated as a particular type of expressions. As an example, the type of boolean expressions is a particular type of expressions, since using a boolean expression in composite context does not supported.
-}
module Expr
    (
      -- exports
      Expr(..),
      Expr'(..)
    )
where

-- External imports

-- Internal imports
import           Term

------------------------------------------------------------------------------------------
-- Data types and clases declaration

-- | Type of aggregate expression with a type of terminal symbols @a@
data Expr' a

  -- Terminal expressions:
  = Term (Term LSymbol a) -- ^ term

  -- Composite expressions:
  | Alt   [Expr' a] -- ^ list of alternative expressions
  | Tuple [Expr' a] -- ^ tuple of expressions
  | List  [Expr' a] -- ^ list of expressions
  | Set   [Expr' a] -- ^ set of expressions
  deriving (Eq, Ord, Show)

-- | Type of expressions with a type of terminal symbols @a@ and type of booleans $b$.
data Expr a c
  = NONE           -- ^ undefined expression
  | Aggr (Expr' a) -- ^ aggregate expression
  | Bool b         -- ^ boolean expression
  deriving (Eq, Ord, Show)
