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
import           DebugInfo
import           Database
import           LSymbol
import           Program
import           Program.Handler

------------------------------------------------------------------------------------------
-- Data and type declaration

-- | Type of monad Globe
type Global = StateT InfoS IO

-- | Type of global informational structure
data InfoS = Info
  {
    lsymbols     :: LSymbols,         -- ^ database of logical symbols
    programs     :: ProgramsS,        -- ^ database of programs
    pvars        :: PVars,            -- ^ database of program variables
    handlerState :: Program.Handler.State Global SrcInfo
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

instance Program.Handler.Base Global SrcInfo where
  getState = handlerState <$> get
  setState s = modify (\info -> info { handlerState = s })

instance NameSpace Global

------------------------------------------------------------------------------------------
-- Functions

-- | Make somesing in monad Global
make :: Global a -> IO a
make = (`evalStateT` Info initLSymbols initPrograms initPVars initState)
