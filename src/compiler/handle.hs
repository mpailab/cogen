{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeSynonymInstances  #-}

{-|
Module      : Compiler.Handel
Description : Representation of programs in compiler
Copyright   : (c) Grigoriy Bokov, 2017
License     : GPL-3
Maintainer  : bokov@intsys.msu.ru
Stability   : experimental
Portability : POSIX
-}
module Compiler.Handel
    (
      -- exports
      Handel(..)
    )
where

-- Internal imports
import           Compiler.Program
import           Term

type Handel = [Term] -> IO ()

-- make :: Program -> Handel
-- make prog tl = case s of
--   (Action vl t : _) -> (build t)

class Build a b where
  build :: a -> [Term] -> b

instance Build Program (IO ()) where
  build prog tl = case prog of

    (Assign vl v g c p) -> do
      map (\x -> build p (tl ++ x)) [ x | x <- build g tl, build c (tl ++ x) ]
      return

    (Branch vl c b p) -> if build c tl
        then build b tl
        else build p tl

    (Switch vl e cl p) -> do
      let i = build e tl
      if (0 <= i) and (i <= length cl)
        then build (cl !! i) tl
        else build p tl

    (Action vl t)    -> build t tl

    Empty                    -> return

instance Build Term (IO ()) where
  build t tl = case t of
    Fun sym stl -> foldr (\ x y -> y (build x tl)) (eval sym) stl
    _           -> error "Handel error: Can't build an action from a program term.\n"

instance Build Term Term where
  build t tl = case t of
    Fun sym stl -> foldr (\ x y -> y (build x tl)) (eval sym) stl
    Var sym     -> tl !! sym
    _           -> error "Handel error: Can't build an expression from a program term.\n"
