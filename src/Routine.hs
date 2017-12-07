{-# LANGUAGE Rank2Types #-}

{-|
Module      : Routine
Description :
Copyright   : (c) Grigoriy Bokov, 2017
License     : GPL-3
Maintainer  : bokov@intsys.msu.ru
Stability   : experimental
Portability : POSIX
-}
module Routine
    (
      -- exports
    )
where

-- External imports

-- Internal imports
import           Problem

type Routine = forall a . Problem a

-- Problem () - routine without arguments which does not return a value
-- Problem Int - routine without arguments which returns an integer
-- Problem (Int -> Problem Int) - routine which maps integers to integers
--
-- GetRoutine :: LSymbol -> Routine
--
-- In monad Problem:
-- let sym :: LSymbol = "plus"
-- let x :: Int = 2
-- let y :: Int = 3
-- f :: Int -> Int -> Problem Int <- GetRoutine sym
-- z :: Int <- f x y
