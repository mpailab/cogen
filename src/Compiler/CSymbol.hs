{-|
Module      : Program.CSymbol
Description : Compiler symbols
Copyright   : (c) Grigoriy Bokov, 2017
License     : GPL-3
Maintainer  : bokov@intsys.msu.ru
Stability   : experimental
Portability : POSIX

Compiler symbols are special symbol used for denoting information units in compiler.

Warning: The length of constructor's names for compiler symbols is bounded
         by 20 characters.
-}
module Program.CSymbol
    (
      -- exports
      CSymbol()
    )
where

-- External imports

-- Internal imports

------------------------------------------------------------------------------------------
-- Data and type declaration

-- | Type of compiler symbols
data CSymbol = NONE                 -- ^ auxiliary symbol
             | BindOccur            -- ^ occurrence of binding symbol in substituted term
             | BindSymbol           -- ^ binding symbol of rule
             | Bounds               -- ^ list of bouned variables in the theorem of rule
             | From                 -- ^ substituted term in the theorem of rule
             | Premises             -- ^ list of premises in the theorem of rule
             | To                   -- ^ substituting term in the theorem of rule
             deriving (Eq, Ord, Read, Show)

------------------------------------------------------------------------------------------
-- Functions
