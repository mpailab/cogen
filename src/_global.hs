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
      Global,
      getLSymbols,
      make,
      load, save
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

-- | Get the database of logical symbols
getLSymbols :: Global LSymbols
getLSymbols = fmap lsymbols get

-- | Get the database of programs
getPrograms :: Global Programs
getPrograms = fmap programs get

-- | Make somesing in monad Global
make :: Global a -> IO a
make = (`evalStateT` Info initLSymbols initPrograms)

-- Instances of Database for reading/writing logical symbols in monad Global
instance Database String LSymbols Global where
  load = liftIO . (load :: String -> IO LSymbols)
  save db = liftIO . (save db :: String -> IO ())

-- Instances of Database for reading/writing programs in monad Global
instance Database String Programs Global where
  load dir = getLSymbols >>= \x -> liftIO (load (dir, x) :: IO Programs)
  save db dir = getLSymbols >>= \x -> liftIO (save db (dir, x) :: IO ())

-- Instances of Database for reading/writing a program of logical symbol in monad Global
instance Database (String, LSymbol) Program Global where
  load (dir, s) = getLSymbols >>= \x -> liftIO (load (dir, s, x) :: IO Program)
  save db (dir, s) = getLSymbols >>= \x -> liftIO (save db (dir, s, x) :: IO ())


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
