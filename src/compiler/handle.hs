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

class Eval a where
  eval :: LSymbol -> a

instance Eval (Bool -> Bool) where
  eval Neg = not

instance Eval (Bool -> Bool -> Bool) where
  eval And = (&&)
  eval Or  = (||)

instance Eval (Term -> Term -> Bool) where
  eval Equal = (==)

instance Eval (Term -> LSymbol) where
  eval Header = header

instance Eval (Term -> [Term]) where
  eval Operands = operands

class Build a b where
  build :: a -> [Term] -> IO b

instance Build Program () where
  build prog tl = case prog of

    (Assign vl v g c p) ->
      forM_ [ x | x <- build g tl, build c (tl ++ [x]) ] (\x -> build p (tl ++ [x]))

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

instance Build Term [Term] where
  build t s = case t of
    sym :> ts -> foldr (\ x y -> y (build x s)) (eval sym) ts
    _         -> error "Handle error: Can't build a list generation from a program term.\n"

instance Build Term (IO ()) where
  build t s = case t of
    sym :> ts -> foldr (\ x y -> y (build x s)) (eval sym) ts
    _           -> error "Handle error: Can't build an action from a program term.\n"

instance Build Term Term where
  build t tl = case t of
    sym :> stl -> foldr (\ x y -> y (build x tl)) (eval sym) stl
    Var sym     -> tl !! sym
    _           -> error "Handle error: Can't build an expression from a program term.\n"
