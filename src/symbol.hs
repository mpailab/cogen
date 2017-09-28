{-|
Module      : Symbol
Description : Logical symbols
Copyright   : (c) Grigoriy Bokov, 2017
License     : GPL-3
Maintainer  : bokov@intsys.msu.ru
Stability   : experimental
Portability : POSIX

Logical symbols are user-defined symbols used for denoting specific notions (relations between objects, operations over them, names of objects, logical connectives, quantifiers and etc.).

Logical symbols are numbered with positive integers. In fact, every logical symbol is a positive integer which is its identifier. The name of symbol and other information contain in a database of logical symbols loaded on startup.

In order to add new logical symbol need:
  1. from the outside: add symbol specification in the loaded database.
  2. from the inside: call the function add.
-}
module Symbol
    (
      -- exports
      Symbol(..), Symbols, Info,
      add
    )
where

-- External imports
import           Data.Array

------------------------------------------------------------------------------------------
-- Data and type declaration

-- | Type of logical symbols
newtype Symbol = Symbol Int deriving (Eq, Ord)

-- | Type for database of logical symbols
type Symbols = Array Int Info

-- | Type for information structure of logical symbol
data Info = Info
  {
    name        :: String,
    synonyms    :: [String],
    los         :: Bool,
    categories  :: [String],
    description :: String
  }

------------------------------------------------------------------------------------------
-- Functions

-- | Add a new logical symbol defined by 'info' to a database
add :: Info -> Symbols -> Symbols
add info st = let (f,l) = bounds st
                  es = elems st
              in listArray (f,l+1) (es ++ [info])
