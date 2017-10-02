{-# LANGUAGE Rank2Types #-}

{-|
Module      : Global
Description : Monad Global
Copyright   : (c) Grigoriy Bokov, 2017
License     : GPL-3
Maintainer  : bokov@intsys.msu.ru
Stability   : experimental
Portability : POSIX

Monad Global combines computations that can read, write or modify global informational structure
-}
module Global
    (
      -- exports
    )
where

-- External imports
import           Control.Monad.State

-- Internal imports
-- import           Program
-- import           Routine
import LSymbol

------------------------------------------------------------------------------------------
-- Data and type declaration

-- | Type of monad Globe
type Global a = IO (State Info a)

-- | Type of global informational structure
newtype Info = Info
  {
    lsymbols :: LSymbols -- ^ database of logical symbols
  }
-- data Info = Info
--   {
--     lsymbols :: LSymbols, -- ^ database of logical symbols
--     programs :: Programs,
--     routines :: Routines
--   }

------------------------------------------------------------------------------------------
-- Functions

make :: forall a. (Global a -> a)
make = (`evalState` Info initLSymbols)

-- Instances for databases of logical symbols
instance Database (Global LSymbols) where

  -- | Load the database of logical symbols from a given file
  load file =

  -- | Save a database of logical symbols to a given file
  save db file = writeFile file $ show (elems $ fst db)


  -- do
  -- sdb <- load "database/lsymbols.db"
  -- let info = Info sdb
  -- evalState x info

-- getLSymbols :: Global Rule
-- getLSymbols = fmap lsymbols get
--
-- getPrograms :: Global Program
-- getPrograms = fmap programs get
--
-- getRoutines :: Global Routines
-- getRoutines = fmap routines get
--
-- putLSymbols :: L.Symbols -> Global ()
-- putLSymbols x = modify (\info -> info { lsymbols = x })
--
-- putPrograms :: Programs -> Global ()
-- putPrograms x = modify (\info -> info { programs = x })
--
-- putRoutines :: Routines -> Global ()
-- putRoutines x = modify (\info -> info { routines = x })
