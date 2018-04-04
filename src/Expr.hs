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

-- Internal imports
import           Term

------------------------------------------------------------------------------------------
-- Data types and clases declaration

data Aggregate a b
  = Sym a
  | Int Int
  | Entr b
  | Comp (Composite a b)
  deriving (Eq)

data Composite a b
  = List [Aggregate a b]
  | Tuple [Aggregate a b]
  | Set [Aggregate a b]
  | Term (Term a)
  deriving (Eq)

data Expr a b c
  = NONE
  | Aggr (Aggregate a b)
  | Bool c
  deriving (Eq)
