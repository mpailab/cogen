{-# LANGUAGE DeriveFoldable       #-}
{-# LANGUAGE DeriveFunctor        #-}
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
      Expr'(..),
      Expr(..),
      Label,
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
      pattern ClassDef,
      pattern Void,
      pattern NONE
    )
where

-- External imports
import qualified Data.Map  as M

-- Internal imports
import           DebugInfo
import           LSymbol
import           Term

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

data Expr d = Expr !(Expr' d) !d
  deriving (Functor, Foldable)

-- | Type of expressions
data Expr' d

  -- Simple expressions:
  = Var' Var            -- ^ program variable
  | Ptr' Var (Expr d)   -- ^ pointer to expression
  | APtr' Var           -- ^ pointer to mutable variable in left part of assign expression
  | Ref' Var (Expr d)   -- ^ reference to expression

  -- Constant expressions:
  | Sym' LSymbol   -- ^ logical symbol
  | Int' Integer   -- ^ integer
  | Str' String    -- ^ string
  | Any'           -- ^ any expression
  | AnySeq'        -- ^ any sequence of expressions

    -- Boolean expressions:
  | Bool' Bool                 -- ^ Boolean constant (True or False)
  | Equal' (Expr d) (Expr d)   -- ^ statement A eq B
  | NEqual' (Expr d) (Expr d)  -- ^ statement A ne B
  | In' (Expr d) (Expr d)      -- ^ statement A in B
  | Not' (Expr d)              -- ^ statement not A
  | And' [Expr d]              -- ^ statement A and B
  | Or' [Expr d]               -- ^ statement A or B

  -- Conditional expressions:
  | IfElse' (Expr d) (Expr d) (Expr d)  -- ^ conditional expression
  | CaseOf' (Expr d) [(Expr d, Expr d)] -- ^ switching expression

  -- Composite expressions:
  | Term'  (TExpr d)             -- ^ term over expressions
  | Alt'   [Expr d]              -- ^ alternating of expressions
  | Tuple' [Expr d]              -- ^ tuple of expressions
  | List'  [Expr d]              -- ^ list of expressions
  | Set'   [Expr d]              -- ^ set of expressions

  -- Functional expressions:
  | Call' (Expr d) [Expr d]   -- ^ partial function call
  | Fun' [Expr d] [Command d] -- ^ partial function definiton
  | Void'

  | Class (M.Map Var (Expr d))   -- ^ class instance
  | ClassDef' [Var] [Command d] -- ^ class definition; evaluated to Class

  -- Undefined expression:
  | NONE'

  deriving (Eq, Ord, Show, Functor, Foldable)



dbginfo (Expr _ d) = d

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
pattern Void     :: Monoid d => Expr d
pattern NONE     :: Monoid d => Expr d
pattern ClassDef :: Monoid d => [Var] -> [Command d] -> Expr d

instance Eq (Expr d) where
  (==) (Expr x _) (Expr y _) = x == y

instance Ord (Expr d) where
  (<=) (Expr x _) (Expr y _) = x <= y

instance Show (Expr d) where
  show (Expr x _) = show x

pattern Var x <- Expr (Var' x) _ where
  Var x = Expr (Var' x) mempty

pattern Ptr x e <- Expr (Ptr' x e) _ where
  Ptr x e = Expr (Ptr' x e) mempty

pattern APtr x <- Expr (APtr' x) _ where
  APtr x = Expr (APtr' x) mempty

pattern Ref x e <- Expr (Ref' x e) _ where
  Ref x e = Expr (Ref' x e) mempty

pattern Sym x <- Expr (Sym' x) _ where
  Sym x = Expr (Sym' x) mempty

pattern Int x <- Expr (Int' x) _ where
  Int x = Expr (Int' x) mempty

pattern Str x <- Expr (Str' x) _ where
  Str x = Expr (Str' x) mempty

pattern Any <- Expr Any' _ where
  Any = Expr Any' mempty

pattern AnySeq <- Expr AnySeq' _ where
  AnySeq = Expr AnySeq' mempty

pattern Bool x <- Expr (Bool' x) _ where
  Bool x = Expr (Bool' x) mempty

pattern Equal x y <- Expr (Equal' x y) _ where
  Equal x y = Expr (Equal' x y) mempty

pattern NEqual x y <- Expr (NEqual' x y) _ where
  NEqual x y = Expr (NEqual' x y) mempty

pattern In x y <- Expr (In' x y) _ where
  In x y = Expr (In' x y) mempty

pattern Not x <- Expr (Not' x) _ where
  Not x = Expr (Not' x) mempty

pattern And x <- Expr (And' x) _ where
  And x = Expr (And' x) mempty

pattern Or x <- Expr (Or' x) _ where
  Or x = Expr (Or' x) mempty

pattern IfElse x y z <- Expr (IfElse' x y z) _ where
  IfElse x y z = Expr (IfElse' x y z) mempty

pattern CaseOf x y <- Expr (CaseOf' x y) _ where
  CaseOf x y = Expr (CaseOf' x y) mempty

pattern Term x <- Expr (Term' x) _ where
  Term x = Expr (Term' x) mempty

pattern Alt x <- Expr (Alt' x) _ where
  Alt x = Expr (Alt' x) mempty

pattern Tuple x <- Expr (Tuple' x) _ where
  Tuple x = Expr (Tuple' x) mempty

pattern List x <- Expr (List' x) _ where
  List x = Expr (List' x) mempty

pattern Set x <- Expr (Set' x) _ where
  Set x = Expr (Set' x) mempty

pattern Call x y <- Expr (Call' x y) _ where
  Call x y = Expr (Call' x y) mempty

pattern Fun x y <- Expr (Fun' x y) _ where
  Fun x y = Expr (Fun' x y) mempty

pattern ClassDef x y <- Expr (ClassDef' x y) _ where
  ClassDef x y = Expr (ClassDef' x y) mempty

pattern Void <- Expr Void' _ where
  Void = Expr Void' mempty

pattern NONE <- Expr NONE' _ where
  NONE = Expr NONE' mempty

getTerm (Term x) = x
getList (List x) = x

-- | type of assigning
data Assign
  = Simple  -- ^ match a pattern with en expression (left = right)
  | Select  -- ^ match patterns with list elements ([l1,...,lN] <- right)
  | Iterate -- ^ match a pattern with a result of iterating expression (left <= right)
  | ReplLoc -- ^ same as Simple except all variables in left part are new (let left = right)
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

type Label = Int

-- | Type of commands
data Command d

  -- | Applies a function
  = Apply (Expr d)

  -- | Assigns values to undefined variables
  | Assign
    {
      atype :: Assign,  -- ^ type of assigning
      left  :: Expr d,  -- ^ left part of assigning
      right :: Expr d   -- ^ right part of assigning
    }

  -- | Performs a commands sequence
  | Branch [Command d]

  -- | Breaks all generators until a label
  | Break Label

  -- | Halts function computation
  | Exit

  -- | Checks a condition
  | Guard (Expr d)

  -- | Branches to a commands sequence with respect to condition
  | IfBlock (Expr d) [Command d] [Command d]

  -- | Imports a module
  | Import String

  -- | Sets a label
  | Label Label

  -- | Returns value and halts function computation
  | Return (Expr d)

  -- | Switches to a commands sequence
  | Switch
    {
      expr  :: Expr d,                            -- ^ expression of switching
      cases :: [(Expr d, [(Expr d, [Command d])])] -- ^ list of cases (p,[(c,f)]), where
                                                  -- ^ p is a pattern of the case
                                                  -- ^ c is a condition of the case
                                                  -- ^ f is a command sequence of the case
    }

  | When (Expr d) (Command d)

  -- | Generates next value in generating function
  | Yield (Expr d)

  deriving (Eq, Ord, Show, Functor, Foldable)
