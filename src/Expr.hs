{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE RankNTypes           #-}
{-# LANGUAGE TypeSynonymInstances #-}

{-|
Module      : Expr
Description : Data types of expressions in the language Coral
Copyright   : (c) Grigoriy Bokov, 2018
                  Gleb Kalachev, 2018
License     : GPL-3
Maintainer  : bokov@intsys.msu.ru
Stability   : experimental
Portability : POSIX
-}
module Expr
    (
      -- exports
      Assign(..),
      Command(..),
      Expr(..),
      TExpr,
      Var
    )
where

-- External imports

-- Internal imports
import           LSymbol
import           Term

------------------------------------------------------------------------------------------
-- Data types and clases declaration

type Var = Int
type TExpr = Term Expr

-- | Type of expressions
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
  | Call Expr  [Expr]    -- ^ partial function call
  | Fun [Expr] [Command] -- ^ partial function definiton

  -- Undefined expression:
  | NONE

  deriving (Eq, Ord, Show)

-- | type of assigning
data Assign
  = Simple
  | Select -- ^ match patterns with list elements (l1,...,lN <- right)
  | Unord  -- ^ match list pattern with list of elements in any order (left ~= right)
  | Append -- ^ appends right part to variable (left << right)
  deriving (Eq, Ord)

instance Show Assign where
  show Simple = " = "
  show Select = " <- "
  show Unord  = " ~= "
  show Append = " << "

-- | Type of commands
data Command

  -- | Assign values to undefined variables
  = Assign
    {
      atype :: Assign, -- ^ type of assigning
      left  :: Expr,   -- ^ left part of assigning
      right :: Expr,   -- ^ right part of assigning
      cond  :: Expr    -- ^ condition of assigning
    }

  -- | Branch to a commands sequence
  | Branch
    {
      cond   :: Expr,      -- ^ condition of branching
      branch :: [Command]  -- ^ commands sequence
    }

  -- | Switch to a commands sequence
  | Switch
    {
      expr  :: Expr,              -- ^ expression of switching
      cond  :: Expr,              -- ^ condition of switching
      cases :: [(Expr, Expr, [Command])] -- ^ list of cases (p,c,f), where
                                         -- ^ p is a pattern of the case
                                         -- ^ c is a condition of the case
                                         -- ^ f is a commands sequence of the case
    }

  -- | Apply a function
  | Apply
    {
      func :: Expr,  -- ^ function
      cond :: Expr   -- ^ condition of applying
    }

  deriving (Eq, Ord, Show)
