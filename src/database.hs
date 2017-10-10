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

-- | Class of functions
class Monad m => Database a m where

  -- | Load the database from a given file
  load :: String -> m a

  -- | Save a database to a given file
  save :: a -> String -> m ()
