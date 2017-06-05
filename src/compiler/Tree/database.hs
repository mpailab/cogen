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
      Compiler.Tree.Database.init, open, close,
      load, loadTree,
      save, saveTree
    )
where

  -- External imports
import           Compiler.Tree
import           Control.Exception
import           Data.Map          ((!))
import qualified Data.Map          as DB

-- Internal imports
import           Term

-- Type of database is a map from logical symbols to decision trees
type Database = DB.Map LSymbol Tree

-- Database initialization
init :: Database
init = DB.empty

-- Open a database saved in the file 'file'
open :: String -> IO Database
open file = do
  contents::Either IOError String <- try $ readFile file
  case contents of
     Left _         -> return DB.empty
     Right contents -> return (read contents)

-- Close the database 'db', i.e., save the database 'db' to the file 'file'
close :: Database -> String -> IO ()
close db file = writeFile file $! show db

-- Load a decision tree corresponding to the logical symbol 'sym'
-- from the database 'db'
load :: LSymbol -> Database -> Tree
load sym db = db!sym

-- Load a decision tree corresponding to the logical symbol 'sym'
-- from the file 'file'
loadTree :: LSymbol -> String -> IO Tree
loadTree sym file = do
  db <- open file
  return (load "example" db)

-- Save the logical symbol 'sym' and the decision tree 'tree'
-- to the database 'db'
save :: LSymbol -> Tree -> Database -> IO Database
save sym tree db = return (DB.insert sym tree db)

-- Save the logical symbol 'sym' and the decision tree 'tree'
-- to the file 'file'
saveTree :: LSymbol -> Tree -> String -> IO ()
saveTree sym tree file = do
  db <- open file
  db <- save sym tree db
  close db file
