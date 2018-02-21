{-|
Module      : Coral.Utils
Description : Auxiliary utilites for Coral language
Copyright   : (c) Grigoriy Bokov, 2018
License     : GPL-3
Maintainer  : bokov@intsys.msu.ru
Stability   : experimental
Portability : POSIX
-}
module Coral.Utils
    (
      -- exports
      Write,
      write,
      (+>+)
    )
where

  -- External imports
import           Control.Monad

-- Internal imports
import           LSymbol

------------------------------------------------------------------------------------------
-- Data and type declaration

class Write a where
  write :: LSymbol.Base m => a -> m String

infixr 5 +>+
(+>+) :: LSymbol.Base m => m String -> m String -> m String
(+>+) = liftM2 (++)

------------------------------------------------------------------------------------------
-- Functions
