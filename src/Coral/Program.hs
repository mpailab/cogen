{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}

{-|
Module      : Coral.Program
Description : Representation of Coral programs
Copyright   : (c) Grigoriy Bokov, 2018
License     : GPL-3
Maintainer  : bokov@intsys.msu.ru
Stability   : experimental
Portability : POSIX

A Coral program is a collection of instructions that organizes as a tree in which every node represents an instruction performing a specific task. Each task is described by programs terms.
-}
module Coral.Program
    (
      -- exports
      Program(..), Programs, Base,
      getDB, setDB, Coral.Program.init, get, add
    )
where

  -- External imports
import           Control.Exception
import           Control.Monad
import           Data.Char
import qualified Data.Map          as M
import           Data.Maybe
import qualified Data.Text         as T
import           System.Directory
import           System.FilePath
import           System.IO
import           Text.Regex.Posix

-- Internal imports
import           Coral.Symbol
import           Coral.Utils
import           Database
import           LSymbol (LSymbol, LSymbols)
import qualified LSymbol as LSymbol
import           Term

------------------------------------------------------------------------------------------
-- Data and type declaration

-- | Type of program in compiler
data Program

  -- | Assigning instruction iterates terms with respect to a given condition
  --   and assigns them to a given program variable
  = Assign
    {
      pattern_  :: PTerm,  -- ^ assigned pattern
      generate  :: PTerm,  -- ^ generator of list of terms
      condition :: PTerm,  -- ^ condition for iterating of terms
      jump      :: Program -- ^ jump to next program fragment
    }

  -- | Branching instruction jumps to a given program fragment
  --   with respect to a given condition
  | Branch
    {
      condition :: PTerm,   -- ^ condition for the branch
      branch    :: Program, -- ^ branch to program fragment
      jump      :: Program  -- ^ jump to next program fragment
    }

  -- | Switching instruction jumps to a program fragment defined by a given expression
  | Switch
    {
      expression :: PTerm,              -- ^ expression
      condition  :: PTerm,              -- ^ condition for switching
      cases      :: [(PTerm, Program)], -- ^ list of pairs (pattern, program fragment)
      jump       :: Program             -- ^ jump to next program fragment
    }

  -- | Acting instruction performs a given action with respect to a given condition
  | Action
    {
      action    :: PTerm,  -- ^ action
      condition :: PTerm,  -- ^ condition of action
      jump      :: Program -- ^ jump to next program fragment
    }

  -- | Empty represents an auxiliary program instruction
  | Empty

  deriving(Eq)

-- | Type of programs database
type Programs = M.Map LSymbol Program

-- | Base class of programs
--   Minimal complete definition: getDB, setDB
class Monad m => Base m where

  -- | Get a database of programs
  getDB :: m Programs

  -- | Set a database of programs
  setDB :: Programs -> m ()

  -- | Init a database of programs
  init :: m Programs
  init = let db = M.empty in setDB db >> return db

  -- | Get the program of logical symbol
  get :: LSymbol -> m Program
  get s = (fromJust . M.lookup s) <$> getDB

  -- | Add a program of logical symbol to a database
  add :: LSymbol -> Program -> m ()
  add s p = getDB >>= setDB . M.insert s p

------------------------------------------------------------------------------------------
-- Write instances

instance Write Program where
  write prog = writeProgram 0 prog

writeIndent :: LSymbol.Base m => Int -> m String
writeIndent 0 = return ""
writeIndent n = writeIndent (n-1) >>= \x -> return (' ' : ' ' : x)

writeWhere :: LSymbol.Base m => Int -> [PTerm] -> m String
writeWhere ind [] = return ""
writeWhere ind (t:ts) = writeIndent ind +>+ write t +>+ pure "\n" +>+ writeWhere ind ts

-- | Write a program fragment corresponding to a given indent
writeProgram :: LSymbol.Base m => Int -> Program -> m String

-- | Write an assigning instruction of program fragment corresponding to a given indent
writeProgram ind (Assign pat (List :> [val]) (T (B True)) jump) =
  writeIndent ind +>+ write pat +>+ pure " = " +>+ write val +>+ pure "\n" +>+
  writeProgram ind jump

writeProgram ind (Assign pat (List :> [val]) (And :> conds) jump) =
  writeIndent ind +>+ write pat +>+ pure " = " +>+ write val +>+ pure "\n" +>+
  writeIndent ind +>+ pure "  where\n" +>+
  writeWhere (ind+2) conds +>+
  writeProgram ind jump

writeProgram ind (Assign pat (List :> [val]) cond jump) =
  writeIndent ind +>+ write pat  +>+ pure " = " +>+ write val  +>+
  pure " where " +>+ write cond  +>+ pure "\n" +>+
  writeProgram ind jump

writeProgram ind (Assign pat gen (T (B True)) jump) =
  writeIndent ind +>+ write pat  +>+ pure " <- " +>+ write gen  +>+ pure "\n" +>+
  writeProgram ind jump

writeProgram ind (Assign pat gen (And :> conds) jump) =
  writeIndent ind +>+ write pat  +>+ pure " <- " +>+ write gen  +>+ pure "\n" +>+
  writeIndent ind +>+ pure "  where\n" +>+
  writeWhere (ind+2) conds +>+
  writeProgram ind jump

writeProgram ind (Assign pat gen cond jump) =
  writeIndent ind +>+ write pat  +>+ pure " <- " +>+ write gen  +>+
  pure " where " +>+ write cond  +>+ pure "\n" +>+
  writeProgram ind jump

-- | Write a branching instruction of program fragment corresponding to a given indent
writeProgram ind (Branch cond br jump) =
  writeIndent ind +>+ pure "if " +>+ write cond  +>+ pure "\n" +>+
  writeIndent ind +>+ pure "do\n" +>+
  writeProgram (ind+1) br +>+
  writeProgram ind jump

-- | Write a switching instruction of program fragment corresponding to a given indent
writeProgram ind (Switch expr (T (B True)) cs jump) =
  writeIndent ind +>+ pure "case " +>+ write expr  +>+ pure " of\n" +>+
  writeSwitchCases (ind+1) cs +>+
  writeProgram ind jump

writeProgram ind (Switch expr (And :> conds) cs jump) =
  writeIndent ind +>+ pure "case " +>+ write expr  +>+ pure " of\n" +>+
  writeIndent ind +>+ pure "  where\n" +>+
  writeWhere (ind+2) conds +>+
  writeSwitchCases (ind+1) cs +>+
  writeProgram ind jump

writeProgram ind (Switch expr cond cs jump) =
  writeIndent ind +>+ pure "case " +>+ write expr  +>+ pure " of\n" +>+
  pure " where " +>+ write cond  +>+ pure "\n" +>+
  writeSwitchCases (ind+1) cs +>+
  writeProgram ind jump

-- | Write an acting instruction of program fragment corresponding to a given indent
writeProgram ind (Action act (T (B True)) jump) =
  writeIndent ind +>+ write act  +>+ pure "\n" +>+
  writeProgram ind jump

writeProgram ind (Action act (And :> cs) jump) =
  writeIndent ind +>+ write act  +>+ pure "\n" +>+
  writeIndent ind +>+ pure "  where\n" +>+
  writeWhere (ind+2) cs +>+
  writeProgram ind jump

writeProgram ind (Action act cond jump) =
  writeIndent ind +>+ write act  +>+
  pure " where " +>+ write cond  +>+ pure "\n" +>+
  writeProgram ind jump

-- | Write an empty program fragment corresponding to a given indent
writeProgram ind Empty =
  writeIndent ind +>+ pure "done\n"

writeSwitchCases :: LSymbol.Base m => Int -> [(PTerm, Program)] -> m String
writeSwitchCases ind ((pat,prog):cs) =
  writeIndent ind +>+ write pat  +>+ pure "\n" +>+
  writeIndent ind +>+ pure "do\n" +>+
  writeProgram (ind+1) prog +>+
  writeSwitchCases ind cs
writeSwitchCases ind [] = return ""

------------------------------------------------------------------------------------------
-- Database instances

-- | Database instance for programs
instance (LSymbol.Base m, Base m) => Database String Programs m where

  -- | Load the database of programs from a given directory
  load dir = do
    dir_content <- try (listDirectory dir) -- :: m (Either IOError [FilePath])
    case dir_content of
       Left _            -> Coral.Program.init
       Right dir_content -> foldM f Coral.Program.init dir_content
       where
         f :: (LSymbol.Base m, Base m) => FilePath -> m ()
         f file = do
           db <- getDB
           lsym_db <- LSymbol.getDB
           content <- try (readFile file) -- :: IO (Either IOError FilePath)
           case content of
              Left _        -> return ()
              Right content -> add (LSymbol.get (read $ takeBaseName file))
                                   (parse content lsym_db)

  -- | Save a database of programs to a given directory
  save db (dir, lsym_db) = do
    createDirectoryIfMissing True dir
    mapM_ f (M.assocs db)
    where
      f :: (LSymbol, Program) -> IO ()
      f (s, p) = writeFile (dir ++ name s lsym_db ++ ".db") (write p lsym_db)

-- | Database instance for program
instance Database (String, LSymbol, LSymbols) Program IO where

  -- | Load a program of logical symbol from a database saved in given directory
  load (dir, s, lsym_db) = do
    let file = dir ++ "/" ++ name s lsym_db ++ ".db"
    content <- try (readFile file) :: IO (Either IOError FilePath)
    case content of
      Left _        -> return Empty
      Right content -> return (parse content lsym_db)

  -- | Save a program of logical symbol to a database saved in given directory
  save p (dir, s, lsym_db) = do
    createDirectoryIfMissing True dir
    writeFile (dir ++ "/" ++ name s lsym_db ++ ".db") (write p lsym_db)

------------------------------------------------------------------------------------------
-- Functions
