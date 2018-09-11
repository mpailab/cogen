{-|
Module      : Expr
Description :
Copyright   : (c) Grigoriy Bokov, 2018
License     : GPL-3
Maintainer  : bokov@intsys.msu.ru
Stability   : experimental
Portability : POSIX
-}
module Expr
    (
      -- exports
      Aggregate(..),
      Composite(..),
      Expr(..)
    )
where

-- External imports
import qualified Data.Map as M

-- Internal imports
import           Term

------------------------------------------------------------------------------------------
-- Data types and clases declaration

data Aggregate a b c
  = Sym a
  | Int Int
  | Entr b
  | Comp (Composite a b c)
  | IfElse (c, Aggregate a b c)
  | CaseOf (Aggregate a b c, [(Aggregate a b c, Aggregate a b c)])
  deriving (Eq, Ord, Show)

-- data Conditional a b c
--   = IfElse (c, Aggregate a b c)
--   | CaseOf (Aggregate a b c, [Aggregate a b c])
--   deriving (Eq, Ord, Show)

type HTable a b c = M.Map a [Aggregate a b c]

data Composite a b c
  = List [Aggregate a b c]
  | Tuple [Aggregate a b c]
  | Table (HTable a b c)
  | Set [Aggregate a b c]
  | Term (Term a)
  deriving (Eq, Ord, Show)

data Expr a b c
  = NONE
  | Aggr (Aggregate a b c)
  | Bool c
  deriving (Eq, Ord)
