{-# LANGUAGE MultiParamTypeClasses #-}

{-|
Module      : Database
Description : Class Database
Copyright   : (c) Grigoriy Bokov, 2017
License     : GPL-3
Maintainer  : bokov@intsys.msu.ru
Stability   : experimental
Portability : POSIX

Class Database combines functions that use (read, write, modify, encode and etc.) external databases
-}
module Database
    (
      -- exports
      Database, load, save
    )
where

-- External imports
import           Control.Monad.IO.Class

-- | Class of functions
class MonadIO m => Database a b m where

  -- | Load the database from a given file
  load :: a -> m b

  -- | Save a database to a given file
  save :: b -> a -> m ()
