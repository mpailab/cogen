{-|
Module      : Problem
Description :
Copyright   : (c) Grigoriy Bokov, 2017
License     : GPL-3
Maintainer  : bokov@intsys.msu.ru
Stability   : experimental
Portability : POSIX
-}
module Problem
    (
      -- exports
      Problem
    )
where

-- External imports
import           Control.Monad.State

-- Internal imports

type Problem a = State Int a

-- data Info = Info
--   {
--     lsymbols :: L.Symbols,
--     programs :: Programs,
--     routines :: Routines
--   }
--
-- getLSymbols :: Problem Rule
-- getLSymbols = fmap lsymbols get
--
-- getPrograms :: Problem Program
-- getPrograms = fmap programs get
--
-- getRoutines :: Problem Routines
-- getRoutines = fmap routines get
--
-- putLSymbols :: L.Symbols -> Problem ()
-- putLSymbols x = modify (\info -> info { lsymbols = x })
--
-- putPrograms :: Programs -> Problem ()
-- putPrograms x = modify (\info -> info { programs = x })
--
-- putRoutines :: Routines -> Problem ()
-- putRoutines x = modify (\info -> info { routines = x })
