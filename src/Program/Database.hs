{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeSynonymInstances  #-}

{-|
Module      : Program.Database
Description : Database of programs
Copyright   : (c) Grigoriy Bokov 2017-2018
License     : GPL-3
Maintainer  : bokov@intsys.msu.ru
Stability   : experimental
Portability : POSIX
-}
module Program.Database
    (
      -- exports
      load,
      save
    )
where

  -- External imports
import           Control.Exception
import           Control.Monad
import           Control.Monad.IO.Class
import qualified Data.Map               as M
import           System.Directory
import           System.FilePath
import           System.IO

-- Internal imports
import           Database
import           LSymbol
import           Program
import           Program.Parser
import           Program.Writer
import           Utils

------------------------------------------------------------------------------------------
-- Data types and clases declaration

-- | Database instance for programs
instance (LSymbol.Base m, Program.Base m, MonadIO m) => Database String Programs m where

  -- | Load the database of programs from a given directory
  load dir = do
    newPrograms
    dir_content <- liftIO (try (listDirectory dir) :: IO (Either IOError [FilePath]))
    case dir_content of
       Left _            -> getPrograms
       Right dir_content -> mapM_ f dir_content >> getPrograms
       where
         f :: (LSymbol.Base m, Program.Base m, MonadIO m) => FilePath -> m ()
         f file = do
           db <- getPrograms
           lsym_db <- getLSymbols
           content <- liftIO (try (readFile file) :: IO (Either IOError FilePath))
           case content of
              Left _        -> return ()
              Right content -> join $ liftM2 addProgram
                                             (getLSymbol (read $ takeBaseName file))
                                             (parse content)

  -- | Save a database of programs to a given directory
  save db dir = do
    liftIO $ createDirectoryIfMissing True dir
    mapM_ f (M.assocs db)
    where
      f :: (LSymbol.Base m, Program.Base m, MonadIO m) => (LSymbol, Program) -> m ()
      f (s, p) = join $ liftM2 writeFileM (dir +>+ nameLSymbol s +<+ ".db") (write p)

-- | Database instance for program
instance (LSymbol.Base m, Program.Base m, MonadIO m) =>
         Database (String, LSymbol) Program m where

  -- | Load a program of logical symbol from a database saved in given directory
  load (dir, s) = do
    file <- dir +>+ "/" +>+ nameLSymbol s +<+ ".db"
    content <- liftIO (try (readFile file) :: IO (Either IOError FilePath))
    case content of
      Left _        -> return Empty
      Right content -> parse content

  -- | Save a program of logical symbol to a database saved in given directory
  save p (dir, s) = do
    liftIO $ createDirectoryIfMissing True dir
    join $ liftM2 writeFileM (dir +>+ "/" +>+ nameLSymbol s +<+ ".db") (write p)

------------------------------------------------------------------------------------------
-- Functions

writeFileM :: MonadIO m => FilePath -> String -> m ()
writeFileM x = liftIO . writeFile x
