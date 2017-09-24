{-|
Module      : Compiler
Description : Implementation of the rules compiler
Copyright   : (c) Grigoriy Bokov, 2017
                  Gleb Kalachev, 2017
License     : GPL-3
Maintainer  : sample@email.com
Stability   : experimental
Portability : POSIX

Module contains an implementation of the rules compiler
-}
module Compiler
    (
      -- exports
      compile
    )
where

-- External imports
import           Control.Monad
import           Control.Monad.State
import           Data.List

-- Internal imports
import           Compiler.Info
import           Compiler.Program
import           LSymbol
import           Rule
import           Term

-- | Compile an inference rule
compile :: Rule -> IO ()
compile rule = do
  let file = "database/programs"
  save (symbol rule) Empty file
