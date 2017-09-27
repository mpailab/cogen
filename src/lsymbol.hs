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

-- External imports

-- | Type of logical symbols
data LSymbol = P Int                    -- ^ program variable
             | X Int                    -- ^ theorem variable
             | C Int                    -- ^ constants
             | And                      -- ^ Term: logical and
             | BindOccur                -- ^ Compiler: occurrence of the binding symbol
                                        -- ^           in substituted term
             | BindSymbol               -- ^ Compiler: binding symbol of rule
             | Bounds                   -- ^ Compiler: list of bouned variables
                                        --             in the theorem of rule
             | Context                  -- ^
             | Equal                    -- ^
             | Equivalence              -- ^
             | Forall                   -- ^
             | From                     -- ^ Compiler: substituted term
                                        --             in the theorem of rule
             | Header                   -- ^ Term: function symbol of term
             | If                       -- ^ Term: symbol in if-then statement
             | LeftToRight              -- ^ Rule: header of rule
             | Level                    -- ^
             | Neg                      -- ^ Term: logical negation
             | Operand                  -- ^
             | Operands                 -- ^
             | Plus                     -- ^
             | Premises                 -- ^ Compiler: list of premises
                                        --             in the theorem of rule
             | Replacing                -- ^
             | RightToLeft              -- ^ Rule: header of rule
             | Then                     -- ^ Term: symbol in if-then statement
             | LTrue                    -- ^ Term: logical constant true
             | To                       -- ^ Compiler: substituting term
                                        --             in the theorem of rule
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
