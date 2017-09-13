{-|
Module      : Compiler
Description : Implementation of the rules compiler
Copyright   : (c) Grigoriy Bokov, 2017
                  Gleb Kalachev, 2017
License     : GPL-3
Maintainer  : sample@email.com
Stability   : experimental
Portability : POSIX

Модуль содержит реализацию компилятора приемов.
-}
module Compiler
    (
      -- exports
      compile
    )
where

import Control.Monad

  -- Internal imports
import           Rule
import           Term
import           Compiler.Tree
import           Compiler.Tree.Database

compile :: Rule -> IO ()
compile rule = do
  let file = "database/tree.db"
  saveTree (symbol rule) (Terminal (filters rule)) file
