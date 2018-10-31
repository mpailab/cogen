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

instance IExpr Bool where
  fromExpr (Bool x) = x
  toExpr = Bool

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
  isb :: (Int -> a) -> Bool

instance ExprFunc Expr where
  cbi ff _ = return ff
  cnt _ = 0
  isb _ = False

instance ExprFunc Integer where
  cbi i _ = return (Int i)
  cnt _ = 0
  isb _ = False

instance ExprFunc Bool where
  cbi i _ = return (Bool i)
  cnt _ = 0
  isb _ = True

instance IExpr t => ExprFunc [t] where
  cbi l _ = return (List $ map toExpr l)
  cnt _ = 0
  isb _ = False

instance (ExprFunc f, IExpr t) => ExprFunc (t -> f) where
  cbi ff (x:args) = cbi (ff $ fromExpr x) $ args
  cnt ff = 1 + cnt (\_ -> ff 0 $ fromExpr NONE) -- create dummy function of new type
  isb ff = isb (\_ -> ff 0 $ fromExpr NONE) -- create dummy function of new type

type BuiltInFuncs m = M.Map Int (BuiltInFunc m)
type FuncNames = M.Map String Int

convf :: (Monad m, ExprFunc f) => f -> BuiltInFunc m
convf f = BuiltInFunc { arity = cnt (\_ -> f), funct = cbi f}

data BIFunc m = BIFunc {
  fname :: String,
  altname :: String,
  prior :: Int,
  assoc :: Assoc,
  commut :: Int, -- ^ 0 : not commutative, 1 : commutative, 2 : apply in parser
  isbool :: Bool,
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
                                names = M.fromList $ zip (fname <$> l) [0..len-1],
                                nf = len
                             }
                             where len = length l

type Bin m = m -> m -> m
type Un m = m -> m

op :: (Monad m, ExprFunc f) => Assoc -> Int -> String -> String -> Int -> f -> BIFunc m
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

bfun :: (Monad m, ExprFunc f) => String -> f -> BIFunc m
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

combinefun :: Expr -> Expr -> Expr
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


builtIn :: Monad m => FuncInfo m
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
    op AssocLeft  1 "++"  "concat"   5  ((++) :: Bin [Expr]),
    op AssocLeft  1 ".|." "bitor"    5  ((.|.) :: Bin Integer),
    op AssocLeft  0 "<="  "leq"      4  ((<=) :: Expr->Expr->Bool),
    op AssocLeft  0 "<"   "lt"       4  ((<)  :: Expr->Expr->Bool),
    op AssocLeft  0 ">="  "geq"      4  ((>=) :: Expr->Expr->Bool),
    op AssocLeft  0 ">"   "gt"       4  ((>)  :: Expr->Expr->Bool),
    op AssocLeft  0 "=="  "eq"       3  ((==) :: Expr->Expr->Bool),
    op AssocLeft  0 "!="  "neq"      3  ((/=) :: Expr->Expr->Bool),
    --op AssocLeft  0 "in"  "member"   3  ()
    --op AssocRight 2 "$"   "apply"    0  (\x y -> Call x [y]),
    bfun "header" (header :: Term Expr -> Expr),
    bfun "combinefun" combinefun
  ]

getBuiltInFunc :: Monad m => Int -> Maybe (BuiltInFunc m)
getBuiltInFunc i = if i < length f then Just . fun $ f ! i else Nothing
                   where f = funcs builtIn

getBuiltInOps :: [[(BIFunc Identity,Int)]]
getBuiltInOps = groupBy (\(x,_) (y,_) -> prior x == prior y) $ filter (\(x,_) -> isOp x) $ zip (elems $ funcs bi) [0..nf bi -1]
                where bi = (builtIn :: FuncInfo Identity)

nameBuiltIn s = altname $ funcs (builtIn :: FuncInfo Identity) ! s

findBIFunc :: String -> Maybe Expr
findBIFunc nm = Sym . IL <$> M.lookup nm (names (builtIn :: FuncInfo Identity))
