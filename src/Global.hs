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
      make
    )
where

-- External imports
import           Control.Monad.State
import qualified Data.Map            as Map

-- Internal imports
import           Database
import           LSymbol
import           Program

------------------------------------------------------------------------------------------
-- Data and type declaration

-- | Type of monad Globe
type Global = StateT Info IO

-- | Type of global informational structure
data Info = Info
  {
    lsymbols :: LSymbols, -- ^ database of logical symbols
    programs :: Programs, -- ^ database of programs
    pvars    :: PVars     -- ^ database of program variables
  }

------------------------------------------------------------------------------------------
-- Base instances

instance LSymbol.Base Global where
  getLSymbols= lsymbols <$> get
  setLSymbols db = modify (\info -> info { lsymbols = db })

instance Program.Base Global where
  getPrograms = programs <$> get
  setPrograms db = modify (\info -> info { programs = db })

instance Program.Vars Global where
  getPVars = pvars <$> get
  setPVars db = modify (\info -> info { pvars = db })

instance NameSpace Global

------------------------------------------------------------------------------------------
-- Functions

-- | Make somesing in monad Global
make :: Global a -> IO a
make = (`evalStateT` Info initLSymbols initPrograms initPVars)
