{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ExplicitForAll #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ExistentialQuantification #-}

module Terms
    ( Term(..), TermSym, TermReference(..), ITerm(..), TermElement(..),
    replaceTerms, findMatches, hasMatch, parentRefs
    )
where
  import Data.List
  import Data.String
  import Control.Monad

  sumf f l = case l of
      [] -> 0
      x:xs -> f x + sumf f xs

  -- | term with arbitrary type @f@ of functional symbols and type @v@ of variable symbols
  data Term f v
      = Const f          -- ^ constant node (may be used instead of function node with 0 arguments)
      | Var v            -- ^ variable node
      | Fun f [Term f v] -- ^ function node with argument list. Each argument is also a term.
      deriving (Eq, Ord)

  -- | Default type of functional and variable symbols
  type TermSym = String

  -- | 'STerm' is short name for term with default type of functional and variable symbols
  type STerm = Term TermSym TermSym

  -- | 'TermReference' specifies subterm with position in the whole term.
  --    stores current subterm and sequence of parents from current subterm parent to term root
  data TermReference f v = TRef [Term f v]
      deriving (Show, Eq, Ord)

  -- | 'STermReference' is short name for 'TermReference' with default type of functional and variable symbols
  type STermReference = TermReference TermSym TermSym

  --termRefList ref = case ref of TRef x -> x

  -- | represents term element : functional symbol of type @f@ or variable of type @v@
  data TermElement f v = TermVar v | TermSym f
      deriving (Show, Eq, Ord)

  -- | short name for term element with default types of functional and variable symbols
  type STermElement = TermElement TermSym TermSym

  -- | 'ITerm' represents interface of object which is similar to STerm.
  class ITerm t where
      header :: t -> STermElement          -- ^ returns term header
      subterms :: t -> [STerm]             -- ^ enumerates subterms of current term
      subtermRefs :: t -> [STermReference] -- ^ enumerate references to subterms of current term
      operands :: t -> [STerm]             -- ^ enumerate operands of current term
      operandRefs :: t -> [STermReference] -- ^ enumerate references to operands of current term
      term :: t-> STerm                    -- ^ converts given object to 'STerm' type
      termref :: t -> STermReference       -- ^ returns reference to current term or subterm

  -- | 'STerm' represents term itself
  instance ITerm STerm where
      subterms t = case t of
          Fun f l -> t : concatMap subterms l
          _ -> [t]
      header term = case term of
          Var v -> TermVar v
          Const c -> TermSym c
          Fun f l -> TermSym f
      operands term = case term of
          Fun f l -> l
          _ -> []
      operandRefs term = case term of
          Fun f l -> map (\x -> TRef [x,term]) l
          _ -> []
      subtermRefs t = subtermRefs (TRef [t])
      term t = t
      termref t = TRef [t]

  -- | 'STermReference' --- ссылка на подтерм; интерфейс 'ITerm' осуществляет работу с этим подтермом.
  instance ITerm STermReference where
      subterms (TRef (t:ts)) = subterms t
      subterms (TRef []) = []

      header (TRef (t:ts)) = header t
      operands (TRef (t:ts)) = operands t
      operandRefs (TRef ref) = case head ref of
          (Fun f l) -> map (\x -> TRef (x:ref)) l
          _ -> []
      subtermRefs = subrefs where
          subrefs ref = ref : concatMap subrefs (operandRefs ref)

      term (TRef (t:ts)) = t
      termref t = t

  -- | returns list of parent subterms of current subterm
  parentRefs :: TermReference f v -> [Term f v]
  parentRefs (TRef ref) = tail ref --ts:parentRefs (TRef (s:ts))

  class (Show s) => LogSymbol s where
     str :: s -> String
     str = show

  --instance LogSymbol Int where
  --   str = show

  instance LogSymbol Char where
     str c = [c]

  instance LogSymbol String where
     str s = s

  instance (LogSymbol f, LogSymbol v) => Show (Term f v) where
      --show :: Show f => Show v => (Term f v -> String)
      show (Var v) = str v
      show (Fun f l) = str f ++ "(" ++ sumstr [show x | x<-l] ++ ")"
           where
              sumstr [] = ""
              sumstr [a] = a
              sumstr (x:y:l) = x++","++sumstr (y:l)

  infixr 5 &
  (&) :: f -> [Term f v] -> Term f v
  f&[] = Const f
  f&l = Fun f l

  lengthf term = case term of
      Fun f l -> 1 + sumf lengthf l
      _ -> 1

  -- | converts 'TermReference' to 'Term'. For 'Sterm' and 'STermReference' same as 'term'
  refVal (TRef (x:xs)) = x
  --refVal (TRef []) = Empty

  -- replaces subterms from list s by corresponding subterms of list t
  replaceTerms x s t = case elemIndex x s of
      Just i -> t!!i
      Nothing -> case x of
          Fun f l -> Fun f (map (\y -> replaceTerms y s t) l)
          _ -> x

  -- sorts list and removes duplicates
  rmdups :: (Ord a) => [a] -> [a]
  rmdups = map head . group . sort

  -- | hasMatch unify given term with pattern.
  --   If term @trm@ can be obtained from term @patt@ by some substitution x1->t1,...,xn->tn,
  --     then returns 'Just' [(x1,t1),...,(xn,tn)].
  --   Otherwise returns 'Nothing'
  hasMatch :: (Ord f, Ord v, Ord v1) => Term f v -> Term f v1 -> Maybe [(v1, Term f v)]

  hasMatch trm patt = case r of
      (True, l) -> if isMapping sl then Just sl else Nothing where sl = rmdups l
      (False, _) -> Nothing
      where
          --conc :: [(Bool, [(String, Term Char Char)])] -> (Bool, [(String, Term Char Char)])
          conc = foldr (\(b1,l1) (b2,l2) -> (b1 && b2, l1 ++ l2)) (True,[])
          --unify :: Term Char Char-> Term Char String -> (Bool, [(String, Term Char Char)])
          unify x (Var v) = (True, [(v, x)])
          unify (Fun g1 l) (Fun g2 v) = if (g1==g2) && (length l == length v) then conc $ zipWith unify l v else (False, [])
          unify _ _ = (False, [])
          --r::(Bool,[(String, Term Char Char)])
          r = unify trm patt
          isMapping ((x1,y1):(x2,y2):xs) = (x1/=x2 || y1==y2) && isMapping ((x2,y2):xs)
          isMapping _ = True

  -- | 'findMatches' @t@ @pattern@ enumerates all subterms of term @t@ that matches given pattern
  findMatches :: STerm -> STerm -> [(STerm, [(TermSym, STerm)])]
  findMatches trm patt = concatMap m (subterms trm)
      where
          m x = case hasMatch x patt of
              Nothing -> []
              Just l -> [(x,l)]
