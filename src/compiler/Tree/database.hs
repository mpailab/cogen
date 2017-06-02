{-# LANGUAGE ScopedTypeVariables #-}

{-|
Module      : Compiler.Tree.Database
Description : Database of compiler's decision trees
Copyright   : (c) Grigoriy Bokov, 2017
                  Gleb Kalachev, 2017
License     : GPL-3
Maintainer  : sample@email.com
Stability   : experimental
Portability : POSIX

Модуль содержит реализацию базы данных разрешающих деревьев компилятора приемов,
а  также интерфейсы для работы с ней.
-}
module Compiler.Tree.Database
    (
      -- exports
      initialize, load, save, finalize
    )
where

  -- External imports
import           Compiler.Tree
import           Control.Exception
import           Data.Map          (Map, (!))
import qualified Data.Map          as Map

-- Internal imports
import           Term

type DatabaseItem = Tree

newtype Database = Db (Map TermSym DatabaseItem) deriving(Show, Read)

initialize :: String -> IO Database
initialize name = do
  contents::Either IOError String <- try $ readFile name
  case contents of
     Left _         -> return (Db Map.empty)
     Right contents -> return (read contents)

finalize :: String -> Database -> IO ()
finalize str db = writeFile str $! show db

load :: Database -> TermSym -> (Database,Tree)
load (Db db) sym = (Db db, db!sym)

save :: Database -> TermSym -> Tree -> Database
save (Db db) sym tree = Db (Map.insert sym tree db)
