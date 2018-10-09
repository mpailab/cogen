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
      Expr(..),
      FExpr,
      SExpr,
      TExpr
    )
where

-- External imports

-- Internal imports
import           LSymbol
import           Term

------------------------------------------------------------------------------------------
-- Data types and clases declaration

type Var = Int
type Args = M.Map Var Expr

class Function m a b where
  apply :: (Args -> m a) -> [(Var,Expr)] -> [Var] -> b

instance Monad m => Function m a (m a) where
  apply f args [] = f (M.fromList args)

instance (Monad m, Function m a b) => Function m a (Expr -> b) where
  apply f args (x:xs) a = apply f ((x,a):args) xs

type FExpr = forall m a . (Monad m, Function m Expr a) => a
type SExpr = forall m a . (Monad m, Function m () a) => a
type TExpr = Term Expr

-- | Type of expression with a type of terminal expressions @a@
data Expr

  -- Simple expressions:
  = Var Var        -- ^ program variable
  | Ptr Var Expr   -- ^ pointer to expression
  | Ref Var Expr   -- ^ reference to expression

  -- Constant expressions:
  | Sym LSymbol    -- ^ logical symbol
  | Int Int        -- ^ integer
  | Any            -- ^ any expression
  | AnySeq         -- ^ any sequence of expressions

    -- Boolean expressions:
  | Bool Bool         -- ^ Boolean constant (True or False)
  | Equal Expr Expr   -- ^ statement A eq B
  | NEqual Expr Expr  -- ^ statement A ne B
  | In Expr Expr      -- ^ statement A in B
  | Not Expr          -- ^ statement not A
  | And [Expr]        -- ^ statement A and B
  | Or [Expr]         -- ^ statement A or B

  -- Conditional expressions:
  | IfElse Expr Expr Expr      -- ^ conditional expression
  | CaseOf Expr [(Expr, Expr)] -- ^ switching expression

  -- Composite expressions:
  | Term  TExpr                -- ^ term over expressions
  | Alt   [Expr]               -- ^ alternating of expressions
  | Tuple [Expr]               -- ^ tuple of expressions
  | List  [Expr]               -- ^ list of expressions
  | Set   [Expr]               -- ^ set of expressions

  -- Functional expressions:
  | Call LSymbol [Expr]  -- ^ partial function call
  | Fun  FExpr           -- ^ function over expressions

  -- Overloaded expressions:
  | Swap Expr SExpr  -- ^ expression together with a swap-function

  -- Undefined expression:
  | NONE

  deriving (Eq, Ord, Show)
