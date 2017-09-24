{-|
Module      : Compiler.Info
Description : Representation of information structure in compiler
Copyright   : (c) Grigoriy Bokov, 2017
License     : GPL-3
Maintainer  : bokov@intsys.msu.ru
Stability   : experimental
Portability : POSIX
-}
module Compiler.Info
    (
      -- exports
      Info(..)
    )
where

-- External imports
import qualified Data.Map         as Map

-- Internal imports
import           Compiler.Program
import           LSymbol
import           Rule
import           Term

-- | Type of table of information units
type Units = Map.Map LSymbol Unit

-- | Type of information structure in compiler
data Info = Info
  {
    variable :: Int,      -- ^ number of first free program variable
    units    :: Units,    -- ^ table of information units
    program  :: [Program] -- ^ list of program fragments
  }

-- | Type of information units
data Unit

  -- | List of bound variables in theorem of rule
  = BoundVars { vars :: [LSymbol] }

  -- | Substituted term in theorem of rule
  | From { from :: Term }

  -- | List of premises in theorem of rule
  | Premises { premises :: [Term] }

  | Root
    {
      root :: Int -- ^ number of program variable representing
                  --   a reference to the root of identified term
    }

  -- | Current inference rule
  | Rule { rule :: Rule }

  -- | Binding symbol of current rule
  | Symbol { symbol :: LSymbol }

  -- | Substituting term in theorem of rule
  | To { to :: Term }

  -- | Empty represents an auxiliary information unit
  | Empty
