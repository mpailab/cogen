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
      (+>+), (+<+), (+<>+)
    )
where

-- External imports
import           Control.Monad

-- Internal imports

infixr 5 +>+
(+>+) :: Monad m => String -> m String -> m String
(+>+) x y = (x ++) <$> y

infixr 5 +<+
(+<+) :: Monad m => m String -> String -> m String
(+<+) x y = (++ y) <$> x

infixr 5 +<>+
(+<>+) :: Monad m => m String -> m String -> m String
(+<>+) = liftM2 (++)
