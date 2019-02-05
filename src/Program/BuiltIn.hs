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
module Program.BuiltIn
    (
      -- exports
      getBuiltInFunc,
      getBuiltInOps,
      nameBuiltIn,
      findBIFunc,
      BIFunc(..),
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
import           Text.Parsec.Expr

-- Internal imports
import           Expr
import           LSymbol
import           Term

------------------------------------------------------------------------------------------
-- Data types and clases declaration

data BuiltInFunc m d = BuiltInFunc {
  arity :: Int,
  funct :: Monad m => [Expr d] -> m (Expr d)
}

class Monoid d => IExpr d a where
  fromExpr :: Expr d -> a
  toExpr   :: a -> Expr d

instance Monoid d => IExpr d Integer where
  fromExpr (Int x) = x
  toExpr = Int

instance Monoid d => IExpr d Bool where
  fromExpr (Bool x) = x
  toExpr = Bool

instance Monoid d => IExpr d Int where
  fromExpr (Int x) = fromInteger x
  toExpr = Int . toInteger

instance Monoid d => IExpr d LSymbol where
  fromExpr (Sym x) = x
  toExpr = Sym

instance (Monoid d, IExpr d t) => IExpr d (Term t) where
  fromExpr (Term x) = fromExpr <$> x
  toExpr x = Term (toExpr <$> x)

instance (Monoid d, IExpr d t) => IExpr d [t] where
  fromExpr (List xs) = map fromExpr xs
  toExpr = List . map toExpr

instance Monoid d => IExpr () (Expr d) where
   fromExpr = ((\_ -> mempty) <$>)
   toExpr   = ((\_ -> ()) <$>)

instance Monoid d => IExpr d (Expr d) where
  fromExpr = id
  toExpr   = id

class ExprFuncInfo a where
  cnt :: (Int -> a) -> Int
  isb :: (Int -> a) -> Bool

class ExprFuncInfo a => ExprFunc d a where
  cbi :: Monad m => a -> [Expr d] -> m (Expr d)

instance ExprFuncInfo (Expr d) where
  cnt _ = 0
  isb _ = False

instance ExprFunc d (Expr d) where
  cbi ff _ = return ff

instance ExprFuncInfo Integer where
  cnt _ = 0
  isb _ = False

instance ExprFuncInfo Bool where
  cnt _ = 0
  isb _ = False

instance Monoid d => ExprFunc d Integer where
  cbi i _ = return (Int i)

instance Monoid d => ExprFunc d Bool where
  cbi i _ = return (Bool i)

instance ExprFuncInfo [t] where
  cnt _ = 0
  isb _ = False

instance (Monoid d, IExpr d t) => ExprFunc d [t] where
  cbi l _ = return (List $ map toExpr l)

instance (ExprFuncInfo f, IExpr () t) => ExprFuncInfo (t -> f) where
  cnt ff  = 1 + cnt (\_ -> ff 0 $ fromExpr (NONE::Expr ())) -- create dummy function of new type
  isb ff = isb (\_ -> ff 0 $ fromExpr (NONE::Expr ())) -- create dummy function of new type

instance (Monoid d, ExprFunc d f, IExpr () t, IExpr d t) => ExprFunc d (t -> f) where
  cbi ff (x:args) = cbi (ff $ fromExpr x) $ args

type BuiltInFuncs m d = M.Map Int (BuiltInFunc m d)
type FuncNames = M.Map String Int

convf :: (Monoid d,Monad m, ExprFunc d f) => f -> BuiltInFunc m d
convf ff = BuiltInFunc { arity = cnt (\_ -> ff), funct = cbi ff}

data BIFunc m d = BIFunc {
  fname :: String,
  altname :: String,
  prior :: Int,
  assoc :: Assoc,
  commut :: Int, -- ^ 0 : not commutative, 1 : commutative, 2 : apply in parser
  isbool :: Bool,
  fun :: BuiltInFunc m d,
  isOp :: Bool
}

data FuncInfo m d = FuncInfo {
  funcs :: Array Int (BIFunc m d),
  names :: M.Map String Int,
  nf    :: Int
}

registerFunctions :: Monad m => [BIFunc m d] -> FuncInfo m d
registerFunctions l = FuncInfo {
                                funcs = listArray (0,len-1) l,
                                names = M.fromList $ zip (fname <$> l) [0..len-1],
                                nf = len
                             }
                             where len = length l

type Bin m = m -> m -> m
type Un m = m -> m

op :: (Monoid d,Monad m, ExprFunc d f) => Assoc -> Int -> String -> String -> Int -> f -> BIFunc m d
op a c nm altnm pr f = BIFunc {
  fname = nm,
  altname = altnm,
  prior = pr,
  assoc = a,
  commut = c,
  fun = convf f,
  isbool = isb (\_ ->f),
  isOp = True
}

bfun :: (Monoid d,Monad m, ExprFunc d f) => String -> f -> BIFunc m d
bfun nm f = BIFunc {
  fname = nm,
  altname = nm,
  prior = 0,
  assoc = AssocLeft,
  commut = 0,
  fun = convf f,
  isbool = isb (\_ ->f),
  isOp = False
}

combinefun :: (Monoid d, Eq d) => (Expr d) -> (Expr d) -> (Expr d)
combinefun NONE f = f
combinefun f NONE = f

combinefun (Fun a1 [Switch e1 (Bool True) vars1]) (Fun a2 [Switch e2 (Bool True) vars2])
  | length a1 /= length a2 = error "cannot combine function patterns with different number of arguments"
  | List a1 /= e1 = error "cannot combine function which is not simple case of all its arguments"
  | List a2 /= e2 = error "cannot combine function which is not simple case of all its arguments"
  | otherwise = Fun a1 [Switch e1 (Bool True) $ vars1 ++ vars2] -- needs to rename variables a2->a1 in vars2

combinefun (Fun a1 [Switch e1 (Bool True) vars1]) (Fun a2 cmds)
  | length a1 /= length a2 = error "cannot combine function patterns with different number of arguments"
  | List a1 /= e1 = error "cannot combine function which is not simple case of all its arguments"
  | otherwise = Fun a1 [Switch e1 (Bool True) $ vars1 ++ [(List a2, Bool True, cmds)]]

combinefun f@(Fun a1 cmds) _ = error "cannot combine fully defined function with another"
combinefun _ _ = error "combinefun : cannot combine non-functional objects"


builtIn :: forall d m. (Monoid d, Ord d, Eq d, IExpr () (Expr d), Monad m) => FuncInfo m d
builtIn = registerFunctions [
    --(".",   convf (\f g -> ))
    --op "~"  10 (complement :: Un Integer),
    --op AssocRight 0 "!"  "not"       10 not,
    op AssocRight 0 "-"  "neg"       10 (negate :: Un Integer),
    op AssocRight 0 "^"  "pow"       8  ((^)  :: Bin Integer),
    op AssocLeft  1 "*"  "mul"       7  ((*)  :: Bin Integer),
    op AssocLeft  0 "/"  "quot"      7  (quot :: Bin Integer),
    op AssocLeft  0 "%"  "mod"       7  (rem  :: Bin Integer),
    op AssocLeft  1 ".&." "bitand"   7  ((.&.) :: Bin Integer),
    op AssocLeft  1 "+"   "add"      6  ((+)  :: Bin Integer),
    op AssocLeft  0 "-"   "subtract" 6  ((-)  :: Bin Integer),
    op AssocLeft  1 ".^." "xor"      6  (xor  :: Bin Integer),
    op AssocLeft  1 "++"  "concat"   5  ((++) :: Bin [Expr d]),
    op AssocLeft  1 ".|." "bitor"    5  ((.|.) :: Bin Integer),
    op AssocLeft  0 "<="  "leq"      4  ((<=) :: Expr d->Expr d->Bool),
    op AssocLeft  0 "<"   "lt"       4  ((<)  :: Expr d->Expr d->Bool),
    op AssocLeft  0 ">="  "geq"      4  ((>=) :: Expr d->Expr d->Bool),
    op AssocLeft  0 ">"   "gt"       4  ((>)  :: Expr d->Expr d->Bool),
    op AssocLeft  0 "=="  "eq"       3  ((==) :: Expr d->Expr d->Bool),
    op AssocLeft  0 "!="  "neq"      3  ((/=) :: Expr d->Expr d->Bool),
    bfun "listElem" ((!!) :: [Expr d] -> Int -> Expr d),
    --op AssocLeft  0 "in"  "member"   3  ()
    --op AssocRight 2 "$"   "apply"    0  (\x y -> Call x [y]),
    bfun "header" (header :: Term (Expr d) -> Expr d),
    bfun "combinefun" (combinefun::Bin (Expr d))
  ]

getBuiltInFunc :: (Monoid d,Ord d,IExpr () (Expr d),Monad m) => Int -> Maybe (BuiltInFunc m d)
getBuiltInFunc i = if i < length f then Just . fun $ f ! i else Nothing
                   where f = funcs builtIn

getBuiltInOps :: forall d. (Ord d,Monoid d) => [[(BIFunc Identity d,Int)]]
getBuiltInOps = groupBy (\(x,_) (y,_) -> prior x == prior y) $ filter (\(x,_) -> isOp x) $ zip (elems $ funcs bi) [0..nf bi -1]
                where bi = (builtIn :: FuncInfo Identity d)

nameBuiltIn s = altname $ funcs (builtIn :: FuncInfo Identity ()) ! s

findBIFunc :: Monoid d => String -> Maybe (Expr d)
findBIFunc nm = Sym . IL <$> M.lookup nm (names (builtIn :: FuncInfo Identity ()))
