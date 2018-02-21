{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}

{-|
Module      : Program
Description : Representation of programs
Copyright   : (c) Grigoriy Bokov, 2017
License     : GPL-3
Maintainer  : bokov@intsys.msu.ru
Stability   : experimental
Portability : POSIX

A program is a collection of instructions that organizes as a tree in which every node represents an instruction performing a specific task. Each task is described by programs terms. Every instruction contains a list of numbers of depended program variables and jump to the next program fragment (except the acting instruction which is a terminal of program).
-}
module Program
    (
      -- exports
      Program(..), Programs,
      initPrograms
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
import           Database
import           LSymbol
import           Program.Parser
import           Program.PSymbol   (PTerm)
import qualified Program.PSymbol   as P
import           Term
import Program.Data

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

------------------------------------------------------------------------------------------
-- Write instances

instance Write Program where
  write prog = writeProgram 0 prog

writeIndent :: LSymbolsBase m => Int -> m String
writeIndent 0 = return ""
writeIndent n = (' ' : ' ' : ) <$> writeIndent (n-1)

writeWhere :: LSymbolsBase m => Int -> [PTerm] -> m String
writeWhere ind [] = return ""
writeWhere ind (t:ts) = writeIndent ind +>+ write t +>+ pure "\n" +>+ writeWhere ind ts

-- | Write a program fragment corresponding to a given indent
writeProgram :: Int -> Program -> m String

-- | Write an assigning instruction of program fragment corresponding to a given indent
writeProgram ind (Assign pat (P.List :> [val]) (T (P.B True)) jump) =
  writeIndent ind +>+ write pat +>+ pure " = " +>+ write val +>+ pure "\n" +>+
  writeProgram ind jump

writeProgram ind (Assign pat (P.List :> [val]) (P.And :> conds) jump) =
  writeIndent ind +>+ write pat +>+ pure " = " +>+ write val +>+ pure "\n" +>+
  writeIndent ind +>+ pure "  where\n" +>+
  writeWhere (ind+2) conds +>+
  writeProgram ind jump

writeProgram ind (Assign pat (P.List :> [val]) cond jump) =
  writeIndent ind +>+ write pat  +>+ pure " = " +>+ write val  +>+
  " where " +>+ write cond  +>+ pure "\n" +>+
  writeProgram ind jump

writeProgram ind (Assign pat gen (T (P.B True)) jump) =
  writeIndent ind +>+ write pat  +>+ pure " <- " +>+ write gen  +>+ pure "\n" +>+
  writeProgram ind jump

writeProgram ind (Assign pat gen (P.And :> conds) jump) =
  writeIndent ind +>+ write pat  +>+ pure " <- " +>+ write gen  +>+ pure "\n" +>+
  writeIndent ind +>+ pure "  where\n" +>+
  writeWhere (ind+2) conds +>+
  writeProgram ind jump

writeProgram ind (Assign pat gen cond jump) =
  writeIndent ind +>+ write pat  +>+ pure " <- " +>+ write gen  +>+
  " where " +>+ write cond  +>+ pure "\n" +>+
  writeProgram ind jump

-- | Write a branching instruction of program fragment corresponding to a given indent
writeProgram ind (Branch cond br jump) =
  writeIndent ind +>+ pure "if " +>+ write cond  +>+ pure "\n" +>+
  writeIndent ind +>+ pure "do\n" +>+
  writeProgram (ind+1) br +>+
  writeProgram ind jump

-- | Write a switching instruction of program fragment corresponding to a given indent
writeProgram ind (Switch expr (T (P.B True)) cs jump) =
  writeIndent ind +>+ pure "case " +>+ write expr  +>+ pure " of\n" +>+
  writeSwitchCases (ind+1) cs +>+
  writeProgram ind jump

writeProgram ind (Switch expr (P.And :> conds) cs jump) =
  writeIndent ind +>+ pure "case " +>+ write expr  +>+ pure " of\n" +>+
  writeIndent ind +>+ pure "  where\n" +>+
  writeWhere (ind+2) conds +>+
  writeSwitchCases (ind+1) cs +>+
  writeProgram ind jump

writeProgram ind (Switch expr cond cs jump) =
  writeIndent ind +>+ pure "case " +>+ write expr  +>+ pure " of\n" +>+
  " where " +>+ write cond  +>+ pure "\n" +>+
  writeSwitchCases (ind+1) cs +>+
  writeProgram ind jump

-- | Write an acting instruction of program fragment corresponding to a given indent
writeProgram ind (Action act (T (P.B True)) jump) =
  writeIndent ind +>+ write act  +>+ pure "\n" +>+
  writeProgram ind jump

writeProgram ind (Action act (P.And :> cs) jump) =
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

writeSwitchCases :: LSymbolsBase m => Int -> [(PTerm, Program)] -> m String
writeSwitchCases ind ((pat,prog):cs) =
  writeIndent ind +>+ write pat  +>+ pure "\n" +>+
  writeIndent ind +>+ pure "do\n" +>+
  writeProgram (ind+1) prog +>+
  writeSwitchCases ind cs
writeSwitchCases ind [] = return ""

------------------------------------------------------------------------------------------
-- Database instances

-- | Database instance for programs
instance Database (String, LSymbols) Programs IO where

  -- | Load the database of programs from a given directory
  load (dir, lsym_db) = do
    dir_content <- try (listDirectory dir) :: IO (Either IOError [FilePath])
    case dir_content of
       Left _            -> return M.empty
       Right dir_content -> foldM f M.empty dir_content
       where
         f :: Programs -> FilePath -> IO Programs
         f db file = do
           content <- try (readFile file) :: IO (Either IOError FilePath)
           case content of
              Left _        -> return db
              Right content -> return (addProgram s (parse content lsym_db) db)
              where
                  s = lsymbol (read $ takeBaseName file) lsym_db

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

-- | Init a database of programs
initPrograms :: Programs
initPrograms = M.empty

-- | Get the program of logical symbol
program :: LSymbol -> Programs -> Program
program s db = fromJust $ M.lookup s db

-- | Add a program of logical symbol to a database
addProgram :: LSymbol -> Program -> Programs -> Programs
addProgram = M.insert
