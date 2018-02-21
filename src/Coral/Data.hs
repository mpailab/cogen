{-|
Module      : Coral.Data
Description : Basic data types and classes for language Coral
Copyright   : (c) Grigoriy Bokov, 2017
License     : GPL-3
Maintainer  : bokov@intsys.msu.ru
Stability   : experimental
Portability : POSIX
-}
module Coral.Data
    (
      -- exports
      P.Program, P.Programs, P.Base,
      P.getDB, P.setDB, P.init,
      S.PSymbol, S.PTerm
    )
where

  -- External imports

-- Internal imports
import qualified Coral.Program as P
import qualified Coral.Symbol as S

------------------------------------------------------------------------------------------
-- Data and type declaration

------------------------------------------------------------------------------------------
-- Functions
