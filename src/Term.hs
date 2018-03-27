{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}

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
      Term(..),
      header, args, subterms, change
      -- drawTerm,
      -- getVarNum,
      -- isContainLSymbol
    )
where

-- External imports
import           Data.Char

-- Internal imports

data Term a = T a
            | a :> [Term a]
            | a :>> Term a
            deriving (Eq, Ord)

instance Foldable Term where
  foldMap f (T x)     = f x
  foldMap f (x :> ts) = f x `mappend` foldMap (foldMap f) ts

-- instance Show Term where
--   show (Var (P x))   = "$p" ++ show x
--   show (Var (X x))   = "$x" ++ show x
--   show (Const (C x)) = show x
--   show (Const x)     = show x
--   show (s :> l)      = show s ++ "[" ++ sumstr (map show l) ++ "]"
--     where
--       sumstr []      = ""
--       sumstr [a]     = a
--       sumstr (x:y:l) = x ++ "," ++ sumstr (y:l)

-- instance Read Term where
--   readsPrec _ str = do
--     [print (str ++ "\n")]
--     case lex str of
--       ("$",r):_   -> [(Var $ f v, x) | (v,x) <- lex r]
--       (s,'[':r):_ -> [(read s :> ts, x) | (ts,x) <- reads ('[':r)]
--       (s,r):_     -> [(Const $ g s, r)]
--       _           -> []
--     where
--       f str = case str of
--         'x':s -> X $ read s
--         'p':s -> P $ read s
--         s     -> read s
--       g x@(s:r) =
--         if isDigit s
--           then C $ read x
--           else read x
--
-- drawTerm :: Term -> String
-- drawTerm  = unlines . draw
--
-- draw :: Term -> [String]
-- draw (Var s)   = lines $ show s
-- draw (Const s) = lines $ show s
-- draw (s :> l)  = lines (show s) ++ drawSubTerms l
--   where
--     drawSubTerms []     = []
--     drawSubTerms [t]    = "|" : shift "`- " "   " (draw t)
--     drawSubTerms (t:tl) = "|" : shift "+- " "|  " (draw t) ++ drawSubTerms tl
--     shift first other   = zipWith (++) (first : repeat other)

-- apply :: Fold a => LSymbol -> a
-- apply sym = fold sym []
--
-- class Fold a where
--   fold :: LSymbol -> [Term] -> a
--
-- instance Fold Bool where
--   fold sym tl = apply' sym (reverse tl)
--
-- instance Fold Term where
--   fold sym tl = apply' sym (reverse tl)
--
-- instance Fold [Term] where
--   fold sym tl = apply' sym (reverse tl)
--
-- instance Fold a => Fold (Term -> a) where
--   fold sym tl t = fold sym (t : tl)
--
-- class Apply a where
--   apply' :: LSymbol -> [Term] -> a
--
-- instance Apply Bool where
--   apply' Equal (t:s:_) = t == s
--
-- instance Apply Term where
--   apply' Context _ = Const Context
--
-- instance Apply [Term] where
--   apply' Operands (t:_) = operands t

class ITerm t a where
  header   :: t a -> a         -- ^ returns term header
  args     :: t a -> [Term a]  -- ^ enumerate arguments of current term
  subterms :: t a -> [Term a]  -- ^ enumerates subterms of current term
  change   :: t a -> Int -> t a -> t a

instance ITerm Term a where

  -- Get header of term
  header (x :> _) = x

  -- Get operands list of term
  args (_ :> ts) = ts

  -- Get subterms list of term
  subterms t =  t : concatMap subterms (args t)

  change (s :> ts) i t = s :> f ts i t
    where
      f (x:xs) n newVal
        | n == 0 = newVal : xs
        | otherwise = x : f xs (n-1) newVal

-- class VarNum a where
--   getVarNum :: a -> Int
--
-- instance VarNum LSymbol where
--   getVarNum (P n) = n
--   getVarNum (X n) = n
--
-- instance VarNum Term where
--   getVarNum (Var x) = getVarNum x
--
-- -- | Does a term contain a logical symbol
-- isContainLSymbol :: Term -> LSymbol -> Bool
-- isContainLSymbol trm sym = any f trm
--   where
--     f x | x == sym = True
--     f _ = False

{-
-- | 'TermReference' specifies subterm with position in the whole term.
--    stores current subterm and sequence of parents from current subterm parent to term root
newtype TermReference = TRef [Term] deriving (Show, Eq, Ord)

-- | 'ITerm' represents interface of object which is similar to STerm.
class ITerm t where
  header      :: t -> TermElement     -- ^ returns term header
  subterms    :: t -> [Term]          -- ^ enumerates subterms of current term
  subtermRefs :: t -> [TermReference] -- ^ enumerate references to subterms of current term
  operands    :: t -> [Term]          -- ^ enumerate operands of current term
  operandRefs :: t -> [TermReference] -- ^ enumerate references to operands of current term
  term        :: t -> Term            -- ^ converts given object to 'Term' type
  termref     :: t -> TermReference   -- ^ returns reference to current term or subterm

class ITermList tl where
  termList :: tl -> [Term]

instance (ITerm t) => ITermList [t] where
  termList = map term

instance ITermList () where
  termList () = []
--instance (ITerm t) => ITermList t where
--  termList tt = [term t]

instance (ITerm t1, ITerm t2) => ITermList (t1,t2) where
  termList (tt1, tt2) = [term tt1, term tt2]

instance (ITerm t1, ITerm t2, ITerm t3) => ITermList (t1,t2,t3) where
  termList (tt1, tt2, tt3) = [term tt1, term tt2, term tt3]

instance (ITerm t1, ITerm t2, ITerm t3, ITerm t4) => ITermList (t1,t2,t3,t4) where
  termList (tt1, tt2, tt3, tt4) = [term tt1, term tt2, term tt3, term tt4]

-- | comparison of 2 terms
equalTerms t1 t2 = term t1 == term t2

-- | 'Term' represents term itself
instance ITerm Term where

  -- Get header of term
  header trm = case trm of
    Var v   -> TermVar v
    Const c -> LSymbol c
    Fun f _ -> LSymbol f

  -- Get subterms list of term
  subterms t = case t of
    Fun _ l -> t : concatMap subterms l
    _       -> [t]

  -- Get list of subterm's references of term
  subtermRefs t = subtermRefs (TRef [t])

  -- Get operands list of term
  operands trm = case trm of
    Fun _ l -> l
    _       -> []

  -- Get list of operand's references of term
  operandRefs trm = case trm of
    Fun _ l -> map (\x -> TRef [x,trm]) l
    _       -> []

  -- Get term
  term t = t

  -- Get term's reference
  termref t = TRef [t]

-- | 'TermReference' --- ссылка на подтерм; интерфейс 'ITerm' осуществляет работу с этим подтермом.
instance ITerm TermReference where

  -- Get header of term for term's reference
  header (TRef (t:_)) = header t

  -- Get subterms list of term for term's reference
  subterms (TRef (t:_)) = subterms t
  subterms (TRef [])     = []

  -- Get list of subterm's references of term for term's reference
  subtermRefs = subrefs where
    subrefs ref = ref : concatMap subrefs (operandRefs ref)

  -- Get operands list of term for term's reference
  operands (TRef (t:_)) = operands t

  -- Get list of operand's references of term for term's reference
  operandRefs (TRef ref) = case head ref of
    (Fun _ l) -> map (\x -> TRef (x:ref)) l
    _         -> []

  -- Get term for term's reference
  term (TRef (t:_)) = t

  -- Get term's reference for term's reference
  termref t = t

-- | returns list of parent subterms of current subterm
parentRefs :: TermReference -> [Term]
parentRefs (TRef ref) = tail ref --ts:parentRefs (TRef (s:ts))

-- replaces subterms from list s by corresponding subterms of list t
replaceTerms :: Term -> [Term] -> [Term] -> Term
replaceTerms x s t = case elemIndex x s of
  Just i  -> t!!i
  Nothing -> case x of
    Fun f l -> Fun f (map (\y -> replaceTerms y s t) l)
    _       -> x

-- sorts list and removes duplicates
rmdups :: (Ord a) => [a] -> [a]
rmdups = map head . group . sort

-- | hasMatch unify given term with pattern.
--   If term @trm@ can be obtained from term @patt@ by some substitution x1->t1,...,xn->tn,
--     then returns 'Just' [(x1,t1),...,(xn,tn)].
--   Otherwise returns 'Nothing'
hasMatch :: Term -> Term -> Maybe [(LSymbol, Term)]

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
findMatches :: Term -> Term -> [(Term, [(LSymbol, Term)])]
findMatches trm patt = concatMap m (subterms trm)
  where
    m x = case hasMatch x patt of
      Nothing -> []
      Just l  -> [(x,l)]
-}
