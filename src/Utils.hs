{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeSynonymInstances  #-}

{-|
Module      : Utils
Description : Auxiliary utilites
Copyright   : (c) Grigoriy Bokov, 2017
License     : GPL-3
Maintainer  : bokov@intsys.msu.ru
Stability   : experimental
Portability : POSIX
-}
module Utils
    (
      -- exports
      echo,
      (+>+), (+<+), (+<>+),
      (<|.), (.|>)
    )
where

-- External imports
import           Control.Monad
import           Debug.Trace
import           Data.Maybe
import           Data.Monoid

-- Internal imports

echo :: Monad f => String -> f ()
echo = traceM
-- echo x = return ()

infixr 5 +>+
(+>+) :: Monad m => String -> m String -> m String
(+>+) x y = (x ++) <$> y

infixr 5 +<+
(+<+) :: Monad m => m String -> String -> m String
(+<+) x y = (++ y) <$> x

infixr 5 +<>+
(+<>+) :: Monad m => m String -> m String -> m String
(+<>+) = liftM2 (++)

--instance Monad m => Monoid (m String) where
--  mappend = (+<>+)
--  mempty = return ""

infixl 4 <|.
(<|.) :: Maybe a -> a -> a
(<|.) x y = fromMaybe y x

infixl 4 .|>
(.|>) :: a -> Maybe a -> a
(.|>) = fromMaybe
