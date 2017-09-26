{-# LANGUAGE FlexibleInstances #-}

{-|
Module      : Term
Description : Term representation
Copyright   : (c) Grigoriy Bokov, 2017
                  Gleb Kalachev, 2017
License     : GPL-3
Maintainer  : sample@email.com
Stability   : experimental
Portability : POSIX

Модуль содержит реализацию термов и интерфейсов для работы с ними.
-}
module Term
    (
      -- exports
      Term, Tree(..),
      header,
      isContainLSymbol
    )
where

-- External imports
import qualified Control.Arrow as Arrow
--import           Control.Monad
import           Data.List
--import           Data.String

-- Internal imports
import           LSymbol

data Tree a = Var a
            | Const a
            | a :> [Tree a]
            deriving (Eq, Ord, Read, Show)

instance Foldable Tree where
  foldMap f (Var v)   = f v
  foldMap f (Const c) = f c
  foldMap f (s :> l)  = f s `mappend` foldMap (foldMap f) l

type Term = Tree LSymbol

drawTerm :: Term -> String
drawTerm  = unlines . draw

draw :: Term -> [String]
draw (Var s)   = lines $ show s
draw (Const s) = lines $ show s
draw (s :> l)  = lines (show s) ++ drawSubTerms l
  where
    drawSubTerms []     = []
    drawSubTerms [t]    = "|" : shift "`- " "   " (draw t)
    drawSubTerms (t:tl) = "|" : shift "+- " "|  " (draw t) ++ drawSubTerms tl
    shift first other   = zipWith (++) (first : repeat other)

apply :: Fold a => LSymbol -> a
apply sym = fold sym []

class Fold a where
  fold :: LSymbol -> [Term] -> a

instance Fold Bool where
  fold sym tl = apply' sym (reverse tl)

instance Fold Term where
  fold sym tl = apply' sym (reverse tl)

instance Fold [Term] where
  fold sym tl = apply' sym (reverse tl)

instance Fold a => Fold (Term -> a) where
  fold sym tl t = fold sym (t : tl)

class Apply a where
  apply' :: LSymbol -> [Term] -> a

instance Apply Bool where
  apply' Equal (t:s:_) = t == s

instance Apply Term where
  apply' Context _ = Const Context

instance Apply [Term] where
  apply' Operands (t:_) = operands t


class ITerm t where
  header      :: t -> LSymbol         -- ^ returns term header
  operands    :: t -> [Term]          -- ^ enumerate operands of current term
  subterms    :: t -> [Term]          -- ^ enumerates subterms of current term

instance ITerm Term where

  -- Get header of term
  header (sym :> _) = sym

  -- Get operands list of term
  operands (_ :> l) = l

  -- Get subterms list of term
  subterms t =  t : concatMap subterms (operands t)

-- | Does a term contain a logical symbol
isContainLSymbol :: Term -> LSymbol -> Bool
isContainLSymbol trm sym = any f trm
  where
    f x | x == sym = True
    f _ = False
