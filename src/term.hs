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
      Term, TermTemplate(..), LSymbol, TermReference(..), ITerm(..), TermElement(..), elementName, (&),
      replaceTerms, findMatches, hasMatch, parentRefs
    )
where

-- External imports
import qualified Control.Arrow as Arrow
--import           Control.Monad
import           Data.List
--import           Data.String


-- | Default type of functional and variable symbols
type LSymbol = String

-- | term with arbitrary type @f@ of functional symbols and variable symbols
-- | template is necessary for type classes that require a type parameter like Foldable
data TermTemplate f
  = Const f                -- ^ constant node (may be used instead of function node with 0 arguments)
  | Var f                  -- ^ variable node
  | Fun f [TermTemplate f] -- ^ function node with argument list. Each argument is also a term.
  deriving (Eq, Ord)


-- | term with arbitrary functional symbols and variable symbols of type LSymbol
type Term = TermTemplate LSymbol

-- | 'TermReference' specifies subterm with position in the whole term.
--    stores current subterm and sequence of parents from current subterm parent to term root
newtype TermReference = TRef [Term] deriving (Show, Eq, Ord)

-- | represents term element : functional symbol of type @f@ or variable of type @v@
data TermElement
  = TermVar LSymbol
  | LSymbol LSymbol
  deriving (Show, Read, Eq, Ord)

-- | returns name of variable or symbol
elementName :: TermElement -> LSymbol
elementName te = case te of
  TermVar var -> var
  LSymbol sym -> sym

-- | 'ITerm' represents interface of object which is similar to STerm.
class ITerm t where
  header      :: t -> TermElement     -- ^ returns term header
  subterms    :: t -> [Term]          -- ^ enumerates subterms of current term
  subtermRefs :: t -> [TermReference] -- ^ enumerate references to subterms of current term
  operands    :: t -> [Term]          -- ^ enumerate operands of current term
  operandRefs :: t -> [TermReference] -- ^ enumerate references to operands of current term
  term        :: t -> Term            -- ^ converts given object to 'Term' type
  termref     :: t -> TermReference   -- ^ returns reference to current term or subterm

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

instance Foldable TermTemplate where
  foldMap f (Const c) = f c
  foldMap f (Var v) = f v
  foldMap f (Fun fun l) = f fun `mappend` foldMap (foldMap f) l

-- | returns list of parent subterms of current subterm
parentRefs :: TermReference -> [Term]
parentRefs (TRef ref) = tail ref --ts:parentRefs (TRef (s:ts))

class (Show s) => LogSymbol s where
  str :: s -> String
  str = show

instance LogSymbol Int where
  str = show

instance LogSymbol Char where
  str c = [c]

instance LogSymbol String where
  str s = s

instance Show Term where
  --show :: Show f => Show v => (Term f v -> String)
  show (Var v) = '$':str v
  show (Const c) = str c
  show (Fun f l) = str f ++ "[" ++ sumstr [show x | x<-l] ++ "]"
    where
      sumstr []      = ""
      sumstr [a]     = a
      sumstr (x:y:ls) = x++","++sumstr (y:ls)

instance Read Term where
  --show :: Show f => Show v => (Term f v -> String)
  readsPrec _ str =
    case lex str of
      ("$",r):_   -> [Arrow.first Var v | v <- lex r]
      (s,'[':r):_ -> [Arrow.first (Fun (rmq s)) lx | lx <- reads ('[':r)]
      (s,r):_     -> [(Const (rmq s),r)]
      _           -> []
      where
        rmq s = case s of
          '"':w -> init w
          x     -> x

infixr 5 &
(&) :: LSymbol -> [Term] -> Term
f&[] = Const f
f&l  = Fun f l

-- | converts 'TermReference' to 'Term'. For 'Sterm' and 'STermReference' same as 'term'
--refVal (TRef (x:_)) = x
--refVal (TRef []) = Empty

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
