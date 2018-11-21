{-# LANGUAGE ScopedTypeVariables  #-}
{-# LANGUAGE TupleSections        #-}

module Structs.Trie ( empty, insert, insertP, trieTail, fromList
                 , getkv, Trie(..)
                 ) where

import           Control.Monad
import           Data.Monoid
import           Data.Maybe
import           Data.Foldable
import qualified Data.Map as Map
import           GHC.Generics (Generic)
import           Control.Applicative

data Trie kc v = Trie (Maybe v) (Map.Map kc (Trie kc v)) deriving (Show, Eq)

insert :: Ord kc => [kc] -> v -> Trie kc v-> Trie kc v
insert []     d (Trie _ m) = Trie (Just d) m
insert (x:xs) d (Trie b m) =
  Trie b $ Map.alter (Just . insert xs d . tmaybe) x m

insertP :: Ord kc => ([kc],v) -> Trie kc v -> Trie kc v
insertP (k,v) = insert k v

ormaybe :: (a -> a -> a) -> Maybe a -> Maybe a -> Maybe a
ormaybe _ Nothing x = x
ormaybe _ x Nothing = x
ormaybe f (Just x) (Just y) = Just $ f x y

unionWith :: Ord k => (a -> a -> a) -> Trie k a -> Trie k a -> Trie k a
unionWith f (Trie v1 m1) (Trie v2 m2) = Trie (ormaybe f v1 v2) (Map.unionWith (unionWith f) m1 m2)

union :: Ord k => Trie k a -> Trie k a -> Trie k a
union = unionWith const

instance Ord kc => Functor (Trie kc) where
  fmap f (Trie v1 m1) = Trie (f <$> v1) (fmap f <$> m1)

nulltrie :: Ord k => Trie k a -> Bool
nulltrie (Trie Nothing m) = Map.null m
nulltrie _ = False

instance Ord kc => Applicative (Trie kc) where
  pure x = Trie (Just x) Map.empty
  (<*>) (Trie x1 m1) (Trie x2 m2) = Trie (x1 <*> x2) (Map.filter nulltrie $ Map.intersectionWith (<*>) m1 m2)

instance Ord kc => Alternative (Trie kc) where
  empty = Trie Nothing Map.empty
  (<|>) = union

instance Ord kc => Foldable (Trie kc) where
  foldMap f (Trie x mp) = foldMap f x <> foldMap (foldMap f) mp

getkvP p (Trie x m) = ((p,) <$> toList x) ++ foldMap (\(k,t) -> getkvP (p++[k]) t) (Map.toList m)

getkv :: Ord kc => Trie kc v -> [([kc],v)]
getkv = getkvP []

tmaybe :: Ord kc => Maybe (Trie kc v) -> Trie kc v
tmaybe (Just x) = x
tmaybe Nothing = empty

fromList :: Ord kc => [([kc],v)] -> Trie kc v
fromList = foldr insertP empty

trieTail :: Ord kc => kc -> Trie kc v -> Trie kc v
trieTail k (Trie _ m) = Map.findWithDefault empty k m
