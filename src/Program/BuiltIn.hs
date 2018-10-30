{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleInstances         #-}
{-# LANGUAGE LambdaCase                #-}
{-# LANGUAGE MultiParamTypeClasses     #-}
{-# LANGUAGE Rank2Types                #-}
{-# LANGUAGE TypeSynonymInstances      #-}

{-|
Module      : Program.Handler
Description : Programs interpreter
Copyright   : (c) Grigoriy Bokov, 2018
License     : GPL-3
Maintainer  : bokov@intsys.msu.ru
Stability   : experimental
Portability : POSIX
-}
module Program.BuiltIn
    (
      -- exports
      getBuiltInFunc,
      BuiltInFunc(..)
    )
where

-- External imports
import           Control.Monad.State
import           Data.Foldable
import qualified Data.Map            as M
import           Data.Array
import           Data.Bits
import           Data.List
import           Control.Monad.Identity

-- Internal imports
import           Expr
import           LSymbol
import           Term

------------------------------------------------------------------------------------------
-- Data types and clases declaration

data BuiltInFunc m = BuiltInFunc {
  arity :: Int,
  funct :: Monad m => [Expr] -> m Expr
}

class IExpr a where
  fromExpr :: Expr -> a
  toExpr   :: a -> Expr

instance IExpr Integer where
  fromExpr (Int x) = x
  toExpr = Int

instance IExpr Int where
  fromExpr (Int x) = fromInteger x
  toExpr = Int . toInteger

instance IExpr LSymbol where
  fromExpr (Sym x) = x
  toExpr = Sym

instance IExpr t => IExpr (Term t) where
  fromExpr (Term x) = fromExpr <$> x
  toExpr x = Term (toExpr <$> x)

instance IExpr t => IExpr [t] where
  fromExpr (List xs) = map fromExpr xs
  toExpr = List . map toExpr

instance IExpr Expr where
  fromExpr = id
  toExpr   = id

class ExprFunc a where
  cbi :: Monad m => a -> [Expr] -> m Expr
  cnt :: (Int -> a) -> Int

instance ExprFunc Expr where
  cbi ff _ = return ff
  cnt _ = 0

instance ExprFunc Integer where
  cbi i _ = return (Int i)
  cnt _ = 0

instance IExpr t => ExprFunc [t] where
  cbi l _ = return (List $ map toExpr l)
  cnt _ = 0

instance (ExprFunc f, IExpr t) => ExprFunc (t -> f) where
  cbi ff (x:args) = cbi (ff $ fromExpr x) $ args
  cnt ff = cnt (\_ -> ff 0 $ fromExpr NONE) -- create dummy function of new type

type BuiltInFuncs m = M.Map Int (BuiltInFunc m)
type FuncNames = M.Map String Int

convf :: (Monad m, ExprFunc f) => f -> BuiltInFunc m
convf f = BuiltInFunc { arity = cnt (\_ -> f), funct = cbi f}

data BIFunc m = BIFunc {
  name :: String,
  prior :: Int,
  fun :: BuiltInFunc m,
  isOp :: Bool
}

data FuncInfo m = FuncInfo {
  funcs :: Array Int (BIFunc m),
  names :: M.Map String Int,
  nf    :: Int
}

registerFunctions :: Monad m => [BIFunc m] -> FuncInfo m
registerFunctions l = FuncInfo {
                                funcs = listArray (0,len-1) l,
                                names = M.fromList $ zip (name <$> l) [0..len-1],
                                nf = len
                             }
                             where len = length l

type Bin m = m -> m -> m
type Un m = m -> m

op :: (Monad m, ExprFunc f) => String -> Int -> f -> BIFunc m
op nm pr f = BIFunc {
  name = nm,
  prior = pr,
  fun = convf f,
  isOp = True
}

bfun :: (Monad m, ExprFunc f) => String -> f -> BIFunc m
bfun nm f = BIFunc {
  name = nm,
  prior = 0,
  fun = convf f,
  isOp = False
}

builtIn :: Monad m => FuncInfo m
builtIn = registerFunctions [
    --(".",   convf (\f g -> ))
    op "~" 10 (complement :: Un Integer),
    op "^"   8  ((^) :: Bin Integer),
    op "*"   7  ((*) :: Bin Integer),
    op "/"   7  (quot :: Bin Integer),
    op "%"   7  (rem :: Bin Integer),
    op ".&." 7  ((.&.) :: Bin Integer),
    op "+"   6  ((+) :: Bin Integer),
    op "-"   6  ((-) :: Bin Integer),
    op ".^." 6  (xor :: Bin Integer),
    op "++"  5  ((++) :: Bin [Expr]),
    op ".|." 5  ((.|.) :: Bin Integer),
    op "$"   0  (\x y -> Call x [y]),
    bfun "header" (header :: Term Expr -> Expr)
  ]

getBuiltInFunc :: Monad m => Int -> Maybe (BuiltInFunc m)
getBuiltInFunc i = if i < length f then Just . fun $ f ! i else Nothing
                   where f = funcs builtIn

getBuiltInOps :: [[(String,Int)]]
getBuiltInOps = map (map (\(x,n) -> (name x,n))) $ groupBy (\(x,_) (y,_) -> prior x == prior y) $ filter (\(x,_) -> isOp x) $ zip (elems $ funcs bi) [0..nf bi -1]
                where bi = (builtIn :: FuncInfo Identity)
