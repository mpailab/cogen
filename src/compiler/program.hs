{-|
Module      : Compiler.Program
Description : Representation of programs in compiler
Copyright   : (c) Grigoriy Bokov, 2017
License     : GPL-3
Maintainer  : bokov@intsys.msu.ru
Stability   : experimental
Portability : POSIX

A program in compiler is a collection of instructions that organizes as a tree
in which every node represents an instruction performing a specific task.
Each task is described from programs terms. All instructions contain
a list of numbers of depended program variables and jump to the next program
fragment (except the acting instruction which is a terminal of program).
-}
module Compiler.Program
    (
      -- exports
      Program(..),
      open, close, get, put, load, save
    )
where

  -- External imports
import           Control.Exception
import           Control.Monad
import qualified Data.Map          as DB
import           System.Directory
import           System.FilePath
import           System.IO

-- Internal imports
import           LSymbol
import           Term

-- | Type of program in compiler
data Program

  -- | Assigning instruction iterates terms with respect to a given condition
  --   and assigns them to a given program variable
  = Assign
    {
      variables :: [Int],  -- ^ list of numbers of depended program variables
      variable  :: Int,    -- ^ number of a program variable
      generate  :: Term,   -- ^ generator of terms
      condition :: Term,   -- ^ condition for iterating of terms
      jump      :: Program -- ^ jump to next program fragment
    }

  -- | Branching instruction jumps to a given program fragment
  --   with respect to a given condition
  | Branch
    {
      variables :: [Int],   -- ^ list of numbers of depended program variables
      condition :: Term,    -- ^ condition for the branch
      branch    :: Program, -- ^ branch to program fragment
      jump      :: Program  -- ^ jump to next program fragment
    }

  -- | Switching instruction jumps to a program fragment defined by an evaluated number
  | Switch
    {
      variables :: [Int],     -- ^ list of numbers of depended program variables
      evaluate  :: Term,      -- ^ evaluation of number of program unit
      cases     :: [Program], -- ^ list of program units
      jump      :: Program    -- ^ jump to next program fragment
    }

  -- | Acting instruction performs a given action and terminates the program
  | Action
    {
      variables :: [Int], -- ^ list of numbers of depended program variables
      perform   :: Term   -- ^ performing of an action
    }

  -- | Empty represents an auxiliary program instruction
  | Empty

  deriving(Eq, Read, Show)

-- | Type of programs database
type Database = DB.Map LSymbol Program

-- | Initialize a database from one saved in given directory
open :: String -> IO Database
open dir = do
  dir_content <- try (listDirectory dir) :: IO (Either IOError [FilePath])
  case dir_content of
     Left _            -> return DB.empty
     Right dir_content -> foldM f DB.empty dir_content
     where
       f :: Database -> FilePath -> IO Database
       f db file = do
         content <- try (readFile file) :: IO (Either IOError FilePath)
         case content of
            Left _        -> return db
            Right content -> return (put (read $ takeBaseName file) (read content) db)

-- | Close a database and save it in given directory
close :: Database -> String -> IO ()
close db dir = do
  createDirectoryIfMissing True dir
  mapM_ f (DB.assocs db)
  where
    f :: (LSymbol, Program) -> IO ()
    f (sym, prog) = writeFile (dir ++ show sym ++ ".db") (show prog)

-- | Get a program of logical symbol from a database
get :: LSymbol -> Database -> Maybe Program
get sym db = case DB.lookup sym db of
  Just prog -> return prog
  Nothing   -> return Empty

-- | Load a program of logical symbol from a database saved in given directory
load :: LSymbol -> String -> IO Program
load sym dir = do
  content <- try (readFile (dir ++ show sym ++ ".db")) :: IO (Either IOError FilePath)
  case content of
    Left _        -> return Empty
    Right content -> return (read content)

-- | Put a program of logical symbol to a database
put :: LSymbol -> Program -> Database -> Database
put = DB.insert

-- | Save a program of logical symbol to a database saved in given directory
save :: LSymbol -> Program -> String -> IO ()
save sym prog dir = do
  createDirectoryIfMissing True dir
  writeFile (dir ++ show sym ++ ".db") (show prog)
