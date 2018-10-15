{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE RankNTypes           #-}
{-# LANGUAGE TypeSynonymInstances #-}

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
      TExpr,
      Command(..),
      PAssign(..)
    )
where

-- External imports

-- Internal imports
import           LSymbol
import           Term

------------------------------------------------------------------------------------------
-- Data types and clases declaration

type FExpr = forall m . Monad m => [Expr] -> m Expr
type SExpr = forall m . Monad m => Expr -> m ()
type TExpr = Term Expr

newtype Func = Func FExpr
newtype SFunc = SFunc SExpr  -- ^ expression together with a swap-function

instance Eq Func where
  (==) a b = error "comparison between functions not allowed"
instance Ord Func where
  (<=) a b = error "comparison between functions not allowed"
instance Show Func where
  show a = error "cannot show compiled function"

instance Eq SFunc where
  (==) a b = error "comparison between swap functions not allowed"
instance Ord SFunc where
  (<=) a b = error "comparison between swap functions not allowed"
instance Show SFunc where
  show a = error "cannot show swap function"

-- | Type of expression with a type of terminal expressions @a@
data Expr

  -- Simple expressions:
  = Var Int        -- ^ program variable
  | BVar Int       -- ^ global variable
  | Ptr Int Expr   -- ^ pointer to expression
  | Ref Int Expr   -- ^ reference to expression

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
  | Call Expr [Expr]  -- ^ partial function call
  | FunDef [Expr] [Command] -- ^ function definiton
  | Fun Func           -- ^ function over expressions

  -- Overloaded expressions:
  | Swap Expr SFunc  -- ^ expression together with a swap-function

  -- Undefined expression:
  | NONE

  deriving (Eq, Ord, Show)

-- | type of assignment statement
data PAssign
  = Select -- ^ match patterns with list elements (l1,...,lN <- right)
  | Unord  -- ^ match list pattern with list of elements in any order (left ~= right)
  | Append -- ^ appends right part to variable (left << right)
  deriving (Eq, Ord)

instance Show PAssign where
  show Select = " <- "
  show Unord  = " ~= "
  show Append = " << "

-- | Type of program statement
data Command

  -- | Assigning instruction iterates terms with respect to a given condition
  --   and assigns them to a given program variable
  = Assign
    {
      assign    :: PAssign, -- ^ type of pattern matching in assignment
      pattern_  :: Expr,   -- ^ assigned pattern
      generate  :: Expr,   -- ^ generator of list of terms
      condition :: Expr    -- ^ condition for iterating of terms
    }

  -- | Branching instruction jumps to a given program fragment
  --   with respect to a given condition
  | Branch
    {
      condition :: Expr,   -- ^ condition for the branch
      branch    :: [Command]  -- ^ branch to program fragment
    }

  -- | Switching instruction jumps to a program fragment defined by a given expression
  | Switch
    {
      expression :: Expr,              -- ^ expression
      condition  :: Expr,              -- ^ condition for switching
      cases      :: [(Expr, Expr, [Command])] -- ^ list of cases (p,c,f), where
                                                -- p is a pattern of the case
                                                -- c is a condition of the case
                                                -- f is a list of commands of the case
    }

  -- | Acting instruction performs a given action with respect to a given condition
  | Action
    {
      action    :: Expr,  -- ^ action
      condition :: Expr   -- ^ condition of action
    }

  deriving (Eq,Ord,Show)
