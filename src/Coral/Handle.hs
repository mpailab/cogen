{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleInstances         #-}
{-# LANGUAGE MultiParamTypeClasses     #-}
{-# LANGUAGE TypeSynonymInstances      #-}

{-|
Module      : Coral.Handle
Description : Interpreter for Coral language
Copyright   : (c) Grigoriy Bokov, 2018
License     : GPL-3
Maintainer  : bokov@intsys.msu.ru
Stability   : experimental
Portability : POSIX
-}
module Coral.Handle
    (
      -- exports
    )
where

-- External imports
import           Control.Monad.State
import           Data.Foldable
import qualified Data.Map            as Map

-- Internal imports
import           Global              (Global)
import           LSymbol
import           Coral.Program
import           Coral.Symbol
import           Term

handle :: Program -> Args -> Global ()
handle p s = return $ evalState (run p) s

data Expr = BE Bool
          | IE Int
          | SE LSymbol
          | TE LTerm
          | LE [Expr]
          | FE Expr -> Expr
          | FIOE (Expr -> IO ())
          deriving (Eq)

type Args = Map.Map Int Expr

type Handler a = State Args a

eval :: PTerm -> Handler Expr

eval (T (X i)) = do
    table <- get
    case Map.lookup i table of
      Just x  -> return x
      Nothing -> error "Evaluating error: a variable is not initialized\n"

eval (T (I i)) = return (IE i)

eval (T (B c)) = return (BE c)

eval (T (S x)) = return (SE x)

eval (List :> x) = LE <$> mapM eval x

eval (Not :> [x]) = eval x >>= \(BE y) -> (return . BE . Prelude.not) y

eval (And :> x) = BE <$> (mapM eval x >>= foldrM (\(BE y) -> (return . (y &&))) True)

eval (Or :> x) = BE <$> (mapM eval x >>= foldrM (\(BE y) -> (return . (y ||))) False)

eval (Equal :> [x,y])  = BE <$> liftM2 (==) (eval x) (eval y)

eval (NEqual :> [x,y]) = BE <$> liftM2 (/=) (eval x) (eval y)

eval (In :> [x, List :> y]) = BE <$> liftM2 elem (eval x) (mapM eval y)

eval (Args :> [x]) = eval x >>= \(TE t) -> (return . LE . fmap TE . Term.args) t


make :: PTerm -> Handler ()
make (Replace :> x) = return ()


-- Identify a program term with a given expression
-- Returns True if there is an assignment of program variables for which the evaluating
-- of program term coincides with the expression, and False otherwise.
-- State of Handler stores assignments of program variables.
ident :: PTerm -> Expr -> Handler Bool

ident (T (X i)) e = do
  s <- get
  case Map.lookup i s of
    Just x
      | x == e    -> return True
      | otherwise -> return False
    Nothing       -> modify (Map.insert i e) >> return True

ident (T x) (TE (T y))
  | x == S y  = return True
  | otherwise = return False

ident (x :> ps) (TE (y :> ts))
  | length ps /= length ts = return False
  | x == S y               = fmap Prelude.and (zipWithM ident ps (fmap TE ts))
  | otherwise              = return False

ident (List :> x) (LE y)
  | length x == length y = fmap Prelude.and (zipWithM ident x y)
  | otherwise            = return False

ident _ _ = return False

-- Identify a program term with a given expression by a condition
-- Returns True if there is an assignment of program's variables for which the evaluating
-- of program term coincides with expression and the condition holds, and False otherwise.
-- Warnings: State of Handler can be changed only if there is such assignment.
identC :: PTerm -> Expr -> PTerm -> Handler Bool
identC p e c = do
  s <- get                -- save current state of Handler
  is_match <- liftM2 (\x (BE y) -> x && y) (ident p e) (eval c) -- identify term with expression by condition
  unless is_match (put s) -- restore current state of Handler if necessary
  return is_match


-- | Run a program fragment
run :: Program -> Handler ()
run (Assign p g c j) = do
  e <- eval g          -- evaluate an expression corresponding to the generator
  jump <- identC p e c -- identify the term with the expression by the condition
  when jump (run j)    -- run the next program fragment if term matches pattern

run (Branch c b j) = do
  s <- get             -- save current state of Handler
  (BE cond) <- eval c  -- evaluate the condition
  when cond (run b)    -- run the branch program fragment if the condition holds
  put s                -- restore current state of Handler
  run j                -- run the next program fragment

run (Switch g c cs j) = do
  s <- get             -- save current state of Handler
  e <- eval g          -- evaluate an expression corresponing to the program expression
  runCase e c cs       -- handle a case corresponing to the expression and the condition
  put s                -- restore current state of Handler
  run j                -- run the next program fragment

run (Action a c j) = do
  (BE cond) <- eval c  -- run the condition
  when cond (make a)   -- make the action if the condition holds
  run j                -- run the next program fragment

run Empty = return ()

-- Run a case corresponing to a given expression
runCase :: Expr -> PTerm -> [(PTerm, Program)] -> Handler ()
runCase e c [] = return ()
runCase e c ((p,b):s) = identC p e c >>= \x -> if x then run b else runCase e c s
