{-# LANGUAGE FlexibleInstances #-}

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
      Info(..), Unit(..),
      Compiler.Info.init, addUnit, getRule
    )
where

-- External imports
import           Control.Monad.State
import qualified Data.Map            as Map

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
    rule     :: Rule,     -- ^ compiled rule
    variable :: Int,      -- ^ number of first free program variable
    units    :: Units,    -- ^ table of information units
    program  :: [Program] -- ^ list of program fragments
  }

init :: Rule -> Info
init rule = Info rule 1 initUnits []

-- | Type of information units
data Unit = I Int
          | S LSymbol
          | T Term
          | IS [Int]
          | SS [LSymbol]
          | TS [Term]
          | EmptyUnit

  -- -- | List of bound variables in theorem of rule
  -- = BoundVars { vars :: [LSymbol] }
  --
  -- -- | Substituted term in theorem of rule
  -- | From { from :: Term }
  --
  -- -- | List of premises in theorem of rule
  -- | Premises { premises :: [Term] }
  --
  -- | Root
  --   {
  --     root :: Int -- ^ number of program variable representing
  --                 --   a reference to the root of identified term
  --   }
  --
  -- -- | Current inference rule
  -- | Rule { rule :: Rule }
  --
  -- -- | Binding symbol of current rule
  -- | Symbol { symbol :: LSymbol }
  --
  -- -- | Substituting term in theorem of rule
  -- | To { to :: Term }
  --
  -- -- | Empty represents an auxiliary information unit
  -- | Empty

initUnits :: Units
initUnits = Map.empty

class AddUnit a where
  addUnit :: LSymbol -> a -> State Info ()

instance AddUnit Int where
  addUnit sym n = addUnitInternal sym $ I n

instance AddUnit LSymbol where
  addUnit sym s = addUnitInternal sym $ S s

instance AddUnit Term where
  addUnit sym t = addUnitInternal sym $ T t

instance AddUnit [Int] where
  addUnit sym ns = addUnitInternal sym $ IS ns

instance AddUnit [LSymbol] where
  addUnit sym ss = addUnitInternal sym $ SS ss

instance AddUnit [Term] where
  addUnit sym ts = addUnitInternal sym $ TS ts

addUnitInternal :: LSymbol -> Unit -> State Info ()
addUnitInternal sym unit = modify (\info -> info { units = Map.insert sym unit (units info) })

getRule :: State Info Rule
getRule = state $ \info -> (rule info, info)
