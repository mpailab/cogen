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
      VExpr(..),
      Expr(..)
    )
where

-- External imports
import qualified Data.Map as M

-- Internal imports
import           Term

------------------------------------------------------------------------------------------
-- Data types and clases declaration

-- | type of expression that returns value
--   common part of logical and program expressions
data VExpr a b
  = Sym a
  | Int Int
  | Entr b
  | List [VExpr a b]
  | Tuple [VExpr a b]
  | Table (HTable a b) -- | ????????
  | Set [VExpr a b]
  | Term (Term a)
  deriving (Eq, Ord, Show)

type HTable a b = M.Map a [VExpr a b]

data Expr a b c
  = NONE
  | Val (VExpr a b)
  | Bool c
  deriving (Eq, Ord)
