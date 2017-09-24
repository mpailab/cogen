{-# LANGUAGE FlexibleInstances #-}

{-|
Module      : LSymbol
Description : Representation of databases
Copyright   : (c) Grigoriy Bokov, 2017
License     : GPL-3
Maintainer  : bokov@intsys.msu.ru
Stability   : experimental
Portability : POSIX
-}
module LSymbol
    (
      -- exports
      LSymbol(..)
    )
where

-- | Type of logical symbols
data LSymbol = P Int                    -- ^ program variable
             | X Int                    -- ^ theorem variable
             | If                       -- ^
             | Then                     -- ^
             | Equal                    -- ^
             | Equivalence              -- ^
             | Forall                   -- ^
             | RightToLeft              -- ^
             | LeftToRight              -- ^
             | Context                  -- ^
             | Operands                 -- ^
             | Level                    -- ^
             | Symbol                   -- ^
             | CurrentTerm              -- ^
             | Replacing                -- ^
             deriving (Eq, Ord, Read, Show)

data Info = Info
  {
    name        :: String,
    synonyms    :: [String],
    inner       :: Bool,
    los         :: Bool,
    categories  :: [String],
    description :: String
  }
