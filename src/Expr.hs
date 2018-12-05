{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE RankNTypes           #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE DeriveFunctor        #-}
{-# LANGUAGE DeriveFoldable       #-}
{-# LANGUAGE PatternSynonyms      #-}
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
      getTerm,
      getList,
      Var,
      ExprS,
      CommandS,
      TExprS,
      Expr0,
      Command0,
      TExpr0,
      pattern Var,
      pattern Ptr,
      pattern  APtr,
      pattern  Ref,
      pattern  Sym,
      pattern  Int,
      pattern  Str,
      pattern  Any,
      pattern   AnySeq,
      pattern  Bool,
      pattern  Equal,
      pattern  NEqual,
      pattern In,
      pattern Not,
      pattern  And,
      pattern  Or,
      pattern  IfElse,
      pattern  CaseOf,
      pattern  Term,
      pattern  Alt,
      pattern  Tuple,
      pattern  List,
      pattern  Set,
      pattern  Call,
      pattern  Fun,
      pattern  ClassDef
    )
where

-- External imports
import qualified Data.Map as M

-- Internal imports
import           LSymbol
import           Term
import           DebugInfo

------------------------------------------------------------------------------------------
-- Data types and clases declaration

type Var = Int
type TExpr d = Term (Expr d)

type ExprS = Expr SrcInfo
type CommandS = Command SrcInfo
type TExprS = TExpr SrcInfo

type Expr0 = Expr ()
type Command0 = Command ()
type TExpr0 = TExpr ()

-- | Type of expressions
data Expr d

  -- Simple expressions:
  = Var' d Var            -- ^ program variable
  | Ptr' d Var (Expr d)   -- ^ pointer to expression
  | APtr' d Var           -- ^ pointer to mutable variable in left part of assign expression
  | Ref' d Var (Expr d)   -- ^ reference to expression

  -- Constant expressions:
  | Sym' d LSymbol   -- ^ logical symbol
  | Int' d Integer   -- ^ integer
  | Str' d String    -- ^ string
  | Any' d           -- ^ any expression
  | AnySeq' d        -- ^ any sequence of expressions

    -- Boolean expressions:
  | Bool' d Bool                 -- ^ Boolean constant (True or False)
  | Equal' d (Expr d) (Expr d)   -- ^ statement A eq B
  | NEqual' d (Expr d) (Expr d)  -- ^ statement A ne B
  | In' d (Expr d) (Expr d)      -- ^ statement A in B
  | Not' d (Expr d)              -- ^ statement not A
  | And' d [Expr d]              -- ^ statement A and B
  | Or' d [Expr d]               -- ^ statement A or B

  -- Conditional expressions:
  | IfElse' d (Expr d) (Expr d) (Expr d)  -- ^ conditional expression
  | CaseOf' d (Expr d) [(Expr d, Expr d)] -- ^ switching expression

  -- Composite expressions:
  | Term'  d (TExpr d)             -- ^ term over expressions
  | Alt'   d [Expr d]              -- ^ alternating of expressions
  | Tuple' d [Expr d]              -- ^ tuple of expressions
  | List'  d [Expr d]              -- ^ list of expressions
  | Set'   d [Expr d]              -- ^ set of expressions

  -- Functional expressions:
  | Call' d (Expr d) [Expr d]   -- ^ partial function call
  | Fun' d [Expr d] [Command d] -- ^ partial function definiton

  | Class (M.Map Var (Expr d))   -- ^ class instance
  | ClassDef' d [Var] [Command d] -- ^ class definition; evaluated to Class

  -- Undefined expression:
  | NONE

  deriving (Eq, Ord, Show, Functor, Foldable)



dbginfo (Var' d _) = d
dbginfo (Ptr' d _ _) = d
dbginfo (APtr' d _) = d
dbginfo (Ref' d _ _) = d
dbginfo (Sym' d _) = d
dbginfo (Int' d _) = d
dbginfo (Str' d _) = d
dbginfo (Any' d) = d
dbginfo (AnySeq' d) = d
dbginfo (Bool' d _) = d
dbginfo (Equal' d _ _) = d
dbginfo (NEqual' d _ _) = d
dbginfo (In' d _ _) = d
dbginfo (Not' d _) = d
dbginfo (And' d _) = d
dbginfo (Or' d _) = d
dbginfo (IfElse' d _ _ _) = d
dbginfo (CaseOf' d _ _) = d
dbginfo (Term'  d _) = d
dbginfo (Alt'   d _) = d
dbginfo (Tuple' d _) = d
dbginfo (List'  d _) = d
dbginfo (Set'   d _) = d
dbginfo (Call' d _ _) = d
dbginfo (Fun' d _ _) = d
dbginfo (ClassDef' d _ _) = d

pattern Var      :: Monoid d => Var -> Expr d
pattern Ptr      :: Monoid d => Var -> Expr d -> Expr d
pattern APtr     :: Monoid d => Int -> Expr d
pattern Ref      :: Monoid d => Var -> Expr d -> Expr d
pattern Sym      :: Monoid d => LSymbol -> Expr d
pattern Int      :: Monoid d => Integer -> Expr d
pattern Str      :: Monoid d => String -> Expr d
pattern Any      :: Monoid d => Expr d
pattern AnySeq   :: Monoid d => Expr d
pattern Bool     :: Monoid d => Bool -> Expr d
pattern Equal    :: Monoid d => Expr d -> Expr d -> Expr d
pattern NEqual   :: Monoid d => Expr d -> Expr d -> Expr d
pattern In       :: Monoid d => Expr d -> Expr d -> Expr d
pattern Not      :: Monoid d => Expr d -> Expr d
pattern And      :: Monoid d => [Expr d] -> Expr d
pattern Or       :: Monoid d => [Expr d] -> Expr d
pattern IfElse   :: Monoid d => Expr d -> Expr d -> Expr d -> Expr d
pattern CaseOf   :: Monoid d => Expr d -> [(Expr d,Expr d)] -> Expr d
pattern Term     :: Monoid d => TExpr d -> Expr d
pattern Alt      :: Monoid d => [Expr d] -> Expr d
pattern Tuple    :: Monoid d => [Expr d] -> Expr d
pattern List     :: Monoid d => [Expr d] -> Expr d
pattern Set      :: Monoid d => [Expr d] -> Expr d
pattern Call     :: Monoid d => Expr d -> [Expr d] -> Expr d
pattern Fun      :: Monoid d => [Expr d] -> [Command d] -> Expr d
pattern ClassDef :: Monoid d => [Var] -> [Command d] -> Expr d

pattern Var x        <- Var' _ x  where       Var x = Var' mempty x
pattern Ptr x y      <- Ptr' _ x y where      Ptr x y      = Ptr' mempty x y
pattern APtr x       <- APtr' _ x where       APtr x       = APtr' mempty x
pattern Ref x y      <- Ref' _ x y where      Ref x y      = Ref' mempty x y
pattern Sym x        <- Sym' _ x where        Sym x        = Sym' mempty x
pattern Int x        <- Int' _ x where        Int x        = Int' mempty x
pattern Str x        <- Str' _ x where        Str x        = Str' mempty x
pattern Any          <- Any' _ where          Any          = Any' mempty
pattern AnySeq       <- AnySeq' _ where       AnySeq       = AnySeq' mempty
pattern Bool x       <- Bool' _ x where       Bool x       = Bool' mempty x
pattern Equal x y    <- Equal' _ x y where    Equal x y    = Equal' mempty x y
pattern NEqual x y   <- NEqual' _ x y where   NEqual x y   = NEqual' mempty x y
pattern In x y       <- In' _ x y where       In x y       = In' mempty x y
pattern Not x        <- Not' _ x where        Not x        = Not' mempty x
pattern And x        <- And' _ x where        And x        = And' mempty x
pattern Or x         <- Or' _ x where         Or x         = Or' mempty x
pattern IfElse x y z <- IfElse' _ x y z where IfElse x y z = IfElse' mempty x y z
pattern CaseOf x y   <- CaseOf' _ x y where   CaseOf x y   = CaseOf' mempty x y
pattern Term x       <- Term' _ x where       Term x       = Term' mempty x
pattern Alt x        <- Alt' _ x where        Alt x        = Alt' mempty x
pattern Tuple x      <- Tuple' _ x where      Tuple x      = Tuple' mempty x
pattern List x       <- List' _ x where       List x       = List' mempty x
pattern Set x        <- Set' _ x where        Set x        = Set' mempty x
pattern Call x y     <- Call' _ x y where     Call x y     = Call' mempty x y
pattern Fun x y      <- Fun' _ x y where      Fun x y      = Fun' mempty x y
pattern ClassDef x y <- ClassDef' _ x y where ClassDef x y = ClassDef' mempty x y

getTerm (Term x) = x
getList (List x) = x

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
  show Simple = " = "
  show Select = " <- "
  show Unord  = " ~= "
  show Append = " << "
  show Replace = " := "
  show ReplLoc = " =< "

-- | Type of commands
data Command d

  -- | Assign values to undefined variables
  = Assign
    {
      atype :: Assign, -- ^ type of assigning
      left  :: Expr d,   -- ^ left part of assigning
      right :: Expr d,   -- ^ right part of assigning
      cond  :: Expr d   -- ^ condition of assigning
    }

  -- | Branch to a commands sequence
  | Branch
    {
      cond   :: Expr d,      -- ^ condition of branching
      branch :: [Command d]  -- ^ commands sequence
    }

  -- | Switch to a commands sequence
  | Switch
    {
      expr  :: Expr d,              -- ^ expression of switching
      cond  :: Expr d,              -- ^ condition of switching
      cases :: [(Expr d, Expr d, [Command d])] -- ^ list of cases (p,c,f), where
                                         -- ^ p is a pattern of the case
                                         -- ^ c is a condition of the case
                                         -- ^ f is a commands sequence of the case
    }

  -- | Apply a function
  | Apply
    {
      func :: Expr d,  -- ^ function
      cond :: Expr d   -- ^ condition of applying
    }

  -- | Return statement or NONE, halts function computation
  | Return { expr :: Expr d }

  -- | generate next value in generating function
  | Yield  { expr :: Expr d }
  | Import String

  deriving (Eq, Ord, Show, Functor, Foldable)
