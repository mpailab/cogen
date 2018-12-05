{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleInstances         #-}
{-# LANGUAGE LambdaCase                #-}
{-# LANGUAGE MultiParamTypeClasses     #-}
{-# LANGUAGE Rank2Types                #-}
{-# LANGUAGE TypeSynonymInstances      #-}
{-# LANGUAGE ScopedTypeVariables       #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE IncoherentInstances       #-}

{-|
Module      : Program.Handler
Description : Programs interpreter
Copyright   : (c) Grigoriy Bokov, 2018
License     : GPL-3
Maintainer  : bokov@intsys.msu.ru
Stability   : experimental
Portability : POSIX
-}
module DebugInfo
    (
      -- exports
      SrcInfo(..),
      DebugInfo
    )
where

-- External imports
import           Control.Monad.State
import           Data.Foldable
-- import           Data.Monoid
import           Data.Semigroup
import qualified Data.Map            as M
import           Data.Array
import           Data.Bits
import           Data.List
import           Control.Monad.Identity
import           Text.Parsec.Expr

-- Internal imports

------------------------------------------------------------------------------------------
-- Data types and clases declaration

data SrcInfo = SrcSegm {
    srcFile   :: String,
    exprBegin :: (Int,Int),
    exprEnd   :: (Int,Int)
  }
  | SrcSegms [SrcInfo] -- severl not joined segments
  | Comment String
  | EmptyInfo
  deriving (Eq, Ord, Show)

-- lesspos (a,b) (c,d) =
instance Semigroup SrcInfo where
    EmptyInfo <> b = b
    a <> EmptyInfo = a
    SrcSegms s <> b = SrcSegms $ s++[b]
    a <> SrcSegms s = SrcSegms $ a:s
    a@(SrcSegm f1 b1 e1) <> b@(SrcSegm f2 b2 e2) =
        if f1==f2 {-&& ((b1 <= e2  && e1>=e2) || (b2<=e1 && e2 >= e1))-} then
            SrcSegm f1 (min b1 b2) (max e1 e2)
        else SrcSegms [a,b]

instance Monoid SrcInfo where
    mempty = EmptyInfo
    mappend = (<>)

class (Monoid d,Eq d,Ord d) => DebugInfo d where

instance DebugInfo SrcInfo where

instance DebugInfo () where
