{-|
Module      : Expr
Description : Data types of expressions
Copyright   : (c) Grigoriy Bokov, 2018
                  Gleb Kalachev, 2018
License     : GPL-3
Maintainer  : bokov@intsys.msu.ru
Stability   : experimental
Portability : POSIX

Expressions are built up from terminal expressions using brackets, braces, commas, alternating, logical terms etc. Terminal expressions are defined outside. As an example, terminal expressions are logical symbols, program variables, function's calls, pointers, referenses.

There are two classes of expressions: logical and program. Logical expressions represent logical terms that are elements of inner language of the system. Program expressions represent program terms that are elements of outer language of the system. The type @Expr@ is used for denoting common parts of logical and program expressions.
-}
module Expr
    (
      -- exports
      Expr(..)
    )
where

-- External imports

-- Internal imports
import           Term
import LSymbol

------------------------------------------------------------------------------------------
-- Data types and clases declaration

-- | Type of expression with a type of terminal expressions @a@
data Expr a

  -- Terminal expressions:
  = Term (Term LSymbol a) -- ^ logical term over terminal expressions

  -- Composite expressions:
  | Alt   [Expr a] -- ^ list of alternative expressions
  | Tuple [Expr a] -- ^ tuple of expressions
  | List  [Expr a] -- ^ list of expressions
  | Set   [Expr a] -- ^ set of expressions
  deriving (Eq, Ord, Show)
