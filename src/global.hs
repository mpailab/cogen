{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}

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
import           Program
-- import           Routine
import           Database
import           LSymbol

------------------------------------------------------------------------------------------
-- Data and type declaration

-- | Type of monad Globe
type Global = StateT Info IO

-- | Type of global informational structure
data Info = Info
  {
    lsymbols :: LSymbols, -- ^ database of logical symbols
    programs :: Programs  -- ^ database of programs
    -- routines :: Routines
  }

------------------------------------------------------------------------------------------
-- Functions

make :: Global a -> IO a
make = (`evalStateT` Info initLSymbols initPrograms)

-- Instances for databases of logical symbols in Global monad
instance Database String LSymbols Global where
  load = liftIO . (load :: String -> IO LSymbols)
  save db = liftIO . (save db :: String -> IO ())


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
