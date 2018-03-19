{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE Rank2Types            #-}
{-# LANGUAGE TypeSynonymInstances  #-}

{-|
Module      : Routine
Description :
Copyright   : (c) Grigoriy Bokov, 2017
License     : GPL-3
Maintainer  : bokov@intsys.msu.ru
Stability   : experimental
Portability : POSIX
-}
module Routine
    (
      -- exports
    )
where

-- External imports
import           Control.Monad.State
import qualified Data.Map            as Map
import           Data.Maybe

-- Internal imports
import           Global
import           LSymbol
import           Problem
import           Program
import           Program.PSymbol     (PTerm)
import qualified Program.PSymbol     as PSymbol
import           Term

type Routine = forall a b . [Term a] -> Global b

type Args = Map.Map Int LTerm

type Bilder a = State Args a

class Build a b where
  build :: a -> Bilder b

instance Build PTerm a where
  build t = do
    s <- get
    case t of
      sym :> ts       -> foldr (\ x y -> fmap y (build x ts)) (PSymbol.eval sym) ts
      T (PSymbol.X i) -> case Map.lookup i s of
        Just x  -> return x
        Nothing -> error "Builder error: a variable is not initialized\n"
      T sym           -> return $ PSymbol.eval sym

instance Build Program () where
  build (Assign p g c j) = do
    t <- build g        -- build a term corresponding to the generator
    jump <- ident t p c -- identify the term with the pattern by the condition
    when jump (build j) -- build the next program fragment if the term matches the pattern

  build (Branch c b j) = do
    s <- get            -- save current state of Bilder
    cond <- build c     -- build the condition
    when cond (build b) -- build the branch program fragment if the condition holds
    put s               -- restore current state of Bilder
    build j             -- build the next program fragment

  build (Switch e c cs j) = do
    s <- get            -- save current state of Bilder
    t <- build e        -- build a term corresponing to the expression
    buildCase t c cs    -- build a case corresponing to the term and the condition
    put s               -- restore current state of Bilder
    build j             -- build the next program fragment

  build (Action a c j) = do
    cond <- build c     -- build the condition
    when cond (build a) -- build the action if the condition holds
    build j             -- build the next program fragment

  build Empty = return ()


-- Identify a term with a given pattern by a condition
-- Returns True if there is an assignment of pattern's variables for which the term
-- matches the pattern and the condition holds, and False otherwise.
-- Warnings: State of Bilder can be changed only if the term matches the pattern.
ident :: LTerm -- a term
      -> PTerm -- a pattern
      -> PTerm -- a condition
      -> Bilder Bool
ident t p c = do
  s <- get                -- save current state of Bilder
  is_match <- liftM2 (&&) (ident' t p) (build c) -- identify term with pattern by condition
  unless is_match (put s) -- restore current state of Bilder if necessary
  return is_match
  where
    ident' :: LTerm -> PTerm -> Bilder Bool
    ident' t (T (PSymbol.X i)) = do
      s <- get
      case Map.lookup i s of
        Just x
          | x == t    -> return True
          | otherwise -> return False
        Nothing       -> modify (Map.insert i t) >> return True

    ident' (T x) (T y)
      | PSymbol.S x == y  = return True
      | otherwise         = return False

    ident' (x :> ts) (y :> ps)
      | PSymbol.S x == y  = fmap and (zipWithM ident' ts ps)
      | otherwise         = return False

    ident' _ _ = return False

-- Build a case corresponing to a given term
buildCase :: LTerm -> PTerm -> [(PTerm, Program)] -> Bilder ()
buildCase t c [] = return ()
buildCase t c ((p,b):cs) = ident t p c >>= \x -> if x then build b else buildCase t c cs
