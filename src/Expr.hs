{-# LANGUAGE CPP                  #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE PatternSynonyms      #-}
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
      Var,
#ifdef DEBUG
      Debug(..),
      pattern Var,
      pattern Ptr,
      pattern APtr,
      pattern Ref,
      pattern Sym,
      pattern Int,
      pattern Str,
      pattern Any,
      pattern AnySeq,
      pattern Bool,
      pattern Equal,
      pattern NEqual,
      pattern In,
      pattern Not,
      pattern And,
      pattern Or,
      pattern IfElse,
      pattern CaseOf,
      pattern Term,
      pattern Alt,
      pattern Tuple,
      pattern List,
      pattern Set,
      pattern Call,
      pattern Fun,
      pattern NONE,
      getList,
      getTerm
#endif
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

#ifdef DEBUG

data Debug = EmptyDBG

data Expr = Expr Expr' Debug

instance Eq Expr where
  (==) (Expr x _) (Expr y _) = x == y

instance Ord Expr where
  (<=) (Expr x _) (Expr y _) = x <= y

instance Show Expr where
  show (Expr x _) = show x

pattern Var x <- Expr (Var' x) _ where
  Var x = Expr (Var' x) EmptyDBG

pattern Ptr x e <- Expr (Ptr' x e) _ where
  Ptr x e = Expr (Ptr' x e) EmptyDBG

pattern APtr x <- Expr (APtr' x) _ where
  APtr x = Expr (APtr' x) EmptyDBG

pattern Ref x e <- Expr (Ref' x e) _ where
  Ref x e = Expr (Ref' x e) EmptyDBG

pattern Sym x <- Expr (Sym' x) _ where
  Sym x = Expr (Sym' x) EmptyDBG

pattern Int x <- Expr (Int' x) _ where
  Int x = Expr (Int' x) EmptyDBG

pattern Str x <- Expr (Str' x) _ where
  Str x = Expr (Str' x) EmptyDBG

pattern Any <- Expr Any' _ where
  Any = Expr Any' EmptyDBG

pattern AnySeq <- Expr AnySeq' _ where
  AnySeq = Expr AnySeq' EmptyDBG

pattern Bool x <- Expr (Bool' x) _ where
  Bool x = Expr (Bool' x) EmptyDBG

pattern Equal x y <- Expr (Equal' x y) _ where
  Equal x y = Expr (Equal' x y) EmptyDBG

pattern NEqual x y <- Expr (NEqual' x y) _ where
  NEqual x y = Expr (NEqual' x y) EmptyDBG

pattern In x y <- Expr (In' x y) _ where
  In x y = Expr (In' x y) EmptyDBG

pattern Not x <- Expr (Not' x) _ where
  Not x = Expr (Not' x) EmptyDBG

pattern And x <- Expr (And' x) _ where
  And x = Expr (And' x) EmptyDBG

pattern Or x <- Expr (Or' x) _ where
  Or x = Expr (Or' x) EmptyDBG

pattern IfElse x y z <- Expr (IfElse' x y z) _ where
  IfElse x y z = Expr (IfElse' x y z) EmptyDBG

pattern CaseOf x y <- Expr (CaseOf' x y) _ where
  CaseOf x y = Expr (CaseOf' x y) EmptyDBG

pattern Term x <- Expr (Term' x) _ where
  Term x = Expr (Term' x) EmptyDBG

pattern Alt x <- Expr (Alt' x) _ where
  Alt x = Expr (Alt' x) EmptyDBG

pattern Tuple x <- Expr (Tuple' x) _ where
  Tuple x = Expr (Tuple' x) EmptyDBG

pattern List x <- Expr (List' x) _ where
  List x = Expr (List' x) EmptyDBG

pattern Set x <- Expr (Set' x) _ where
  Set x = Expr (Set' x) EmptyDBG

pattern Call x y <- Expr (Call' x y) _ where
  Call x y = Expr (Call' x y) EmptyDBG

pattern Fun x y <- Expr (Fun' x y) _ where
  Fun x y = Expr (Fun' x y) EmptyDBG

pattern NONE <- Expr NONE' _ where
  NONE = Expr NONE' EmptyDBG

getTerm :: Expr -> TExpr
getTerm (Expr (Term' t) _) = t

getList :: Expr -> [Expr]
getList (Expr (List' es) _) = es

-- | Type of expressions
data Expr'

  -- Simple expressions:
  = Var' Var        -- ^ program variable
  | Ptr' Var Expr   -- ^ pointer to expression
  | APtr' Var       -- ^ pointer to mutable variable in left part of assign expression
  | Ref' Var Expr   -- ^ reference to expression

  -- Constant expressions:
  | Sym' LSymbol    -- ^ logical symbol
  | Int' Integer    -- ^ integer
  | Str' String     -- ^ string
  | Any'            -- ^ any expression
  | AnySeq'         -- ^ any sequence of expressions

    -- Boolean expressions:
  | Bool' Bool         -- ^ Boolean constant (True or False)
  | Equal' Expr Expr   -- ^ statement A eq B
  | NEqual' Expr Expr  -- ^ statement A ne B
  | In' Expr Expr      -- ^ statement A in B
  | Not' Expr          -- ^ statement not A
  | And' [Expr]        -- ^ statement A and B
  | Or' [Expr]         -- ^ statement A or B

  -- Conditional expressions:
  | IfElse' Expr Expr Expr      -- ^ conditional expression
  | CaseOf' Expr [(Expr, Expr)] -- ^ switching expression

  -- Composite expressions:
  | Term'  TExpr       -- ^ term over expressions
  | Alt'   [Expr]      -- ^ alternating of expressions
  | Tuple' [Expr]      -- ^ tuple of expressions
  | List'  [Expr]      -- ^ list of expressions
  | Set'   [Expr]      -- ^ set of expressions

  -- Functional expressions:
  | Call' Expr  [Expr]    -- ^ partial function call
  | Fun' [Expr] [Command] -- ^ partial function definiton

  -- Undefined expression:
  | NONE'

  deriving (Eq, Ord, Show)

#else

-- | Type of expressions
data Expr

  -- Simple expressions:
  = Var Var        -- ^ program variable
  | Ptr Var Expr   -- ^ pointer to expression
  | APtr Var       -- ^ pointer to mutable variable in left part of assign expression
  | Ref Var Expr   -- ^ reference to expression

  -- Constant expressions:
  | Sym LSymbol    -- ^ logical symbol
  | Int Integer    -- ^ integer
  | Str String     -- ^ string
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
  | Term  { getTerm :: TExpr }  -- ^ term over expressions
  | Alt   [Expr]                -- ^ alternating of expressions
  | Tuple [Expr]                -- ^ tuple of expressions
  | List  { getList :: [Expr] } -- ^ list of expressions
  | Set   [Expr]                -- ^ set of expressions

  -- Functional expressions:
  | Call Expr  [Expr]    -- ^ partial function call
  | Fun [Expr] [Command] -- ^ partial function definiton

  -- Undefined expression:
  | NONE

  deriving (Eq, Ord, Show)

#endif

-- | type of assigning
data Assign
  = Simple  -- ^ match a pattern with en expression (left = right | cond)
  | Select  -- ^ match patterns with list elements ([l1,...,lN] <- right)
  | Iterate -- ^ match a pattern with a result of iterating expression (left <= right)
  | ReplLoc -- ^ same as Simple except all variables in left part are new (let left = right | cond)
  | Replace -- ^ ??? same as Simple except all existing variables in left part are replaced with new values
  | Unord   -- ^ match list pattern with list of elements in any order (left ~= right)
  | Append  -- ^ appends right part to variable (left << right)
  deriving (Eq, Ord)

instance Show Assign where
  show Simple  = " = "
  show Select  = " <- "
  show Unord   = " ~= "
  show Append  = " << "
  show Replace = " := "
  show ReplLoc = " =< "

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

  -- | Return statement or NONE, halts function computation
  | Return { expr :: Expr }

  -- | generate next value in generating function
  | Yield  { expr :: Expr }

  deriving (Eq, Ord, Show)
