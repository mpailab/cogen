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
      header, operands, subterms,
      drawTerm,
      getVarNum,
      isContainLSymbol
    )
where

-- External imports
import           Data.Char

-- Internal imports
import           LSymbol

data Tree a = Var a
            | Const a
            | a :> [Tree a]
            deriving (Eq, Ord)

instance Foldable Tree where
  foldMap f (Var v)   = f v
  foldMap f (Const c) = f c
  foldMap f (s :> l)  = f s `mappend` foldMap (foldMap f) l

type Term = Tree LSymbol

instance Show Term where
  show (Var (P x))   = "$p" ++ show x
  show (Var (X x))   = "$x" ++ show x
  show (Const (C x)) = show x
  show (Const x)     = show x
  show (s :> l)      = show s ++ "[" ++ sumstr (map show l) ++ "]"
    where
      sumstr []      = ""
      sumstr [a]     = a
      sumstr (x:y:l) = x ++ "," ++ sumstr (y:l)

instance Read Term where
  readsPrec _ str = do
    [print (str ++ "\n")]
    case lex str of
      ("$",r):_   -> [(Var $ f v, x) | (v,x) <- lex r]
      (s,'[':r):_ -> [(read s :> ts, x) | (ts,x) <- reads ('[':r)]
      (s,r):_     -> [(Const $ g s, r)]
      _           -> []
    where
      f str = case str of
        'x':s -> X $ read s
        'p':s -> P $ read s
        s     -> read s
      g x@(s:r) =
        if isDigit s
          then C $ read x
          else read x

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

class VarNum a where
  getVarNum :: a -> Int

instance VarNum LSymbol where
  getVarNum (P n) = n
  getVarNum (X n) = n

instance VarNum Term where
  getVarNum (Var x) = getVarNum x

-- | Does a term contain a logical symbol
isContainLSymbol :: Term -> LSymbol -> Bool
isContainLSymbol trm sym = any f trm
  where
    f x | x == sym = True
    f _ = False
