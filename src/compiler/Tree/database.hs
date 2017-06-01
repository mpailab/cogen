{-# LANGUAGE ScopedTypeVariables #-}

module Compiler.Tree.Database
    (
      initialize, load, save, finalize
    ) where

import Compiler.Tree
import Terms
import Data.Map (Map,(!))
import Control.Exception
import qualified Data.Map as Map

type DatabaseItem = Tree

data Database =
  Db (Map TermSym DatabaseItem)
  deriving(Show, Read)

initialize :: String -> IO Database
initialize name = do
  contents::Either IOError String <- try $ readFile name
  case contents of
     Left _ -> return (Db Map.empty)
     Right contents -> return (read contents)
finalize :: String -> Database -> IO ()
finalize str db = writeFile str (show db)

load :: Database -> TermSym -> (Database,Tree)
save :: Database -> TermSym -> Tree -> Database

load (Db db) sym = (Db db, db!sym)
save (Db db) sym tree = Db (Map.insert sym tree db)
