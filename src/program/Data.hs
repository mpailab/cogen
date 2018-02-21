{-|
Module      : Program
Description : Representation of programs
Copyright   : (c) Grigoriy Bokov, 2017
License     : GPL-3
Maintainer  : bokov@intsys.msu.ru
Stability   : experimental
Portability : POSIX

A program is a collection of instructions that organizes as a tree in which every node represents an instruction performing a specific task. Each task is described by programs terms. Every instruction contains a list of numbers of depended program variables and jump to the next program fragment (except the acting instruction which is a terminal of program).
-}
module Program.Data
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
  write :: LSymbolsBase m => a -> m String

infixr 5 +>+
(+>+) :: LSymbolsBase m => m String -> m String -> m String
(+>+) = liftM2 (++)

------------------------------------------------------------------------------------------
-- Functions
