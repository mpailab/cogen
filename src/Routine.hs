{-# LANGUAGE Rank2Types #-}

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

-- Internal imports
import           Problem

type Routine = forall a b . [Term a] -> Global b

type Args = Map.Map Int LTerm

type Bilder a = State Args a

class Build a b where
  build :: a -> Bilder b

instance Build Program () where
  build prog = do
    args <- get
    case prog of

    (Assign p g c j) -> do
      term <- build g   -- build a term corresponding to the generator
      ident p term      -- identify the term with the assigned pattern
      build c >>= guard -- check the condition
      build j           -- build the next program fragment
      where
        ident :: PTerm -> LTerm -> Bilder ()
        ident (T $ PSymbol.X i) t = modify $ Map.insert i t
        ident (T x) (T y) | x == PSymbol.S y = modify id
        ident (x :> ps) (y :> ts) | x == PSymbol.S y = zipWithM_ ident ps ts

    (Branch c b j) -> do
      cond <- build c -- build the condition
      if cond         -- check the condition
        then build b  -- build the branch program fragment if the condition true
        else build j  -- build the next program fragment if the condition false

    (Switch e c cs j) -> do
      term <- build e  -- build a term corresponing to the expression
      cond <- build c  -- build the condition
      forM_ cs (\(p,b) -> ident p term >> build b)

    (Action a c j)    -> do
      build c >>= guard -- check the condition
      build a           -- build and apply the action
      build j           -- build the next program fragment

    Empty             -> return
