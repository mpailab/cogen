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
import           System.Directory
import           System.FilePath
import           System.IO

-- Internal imports
import           Database
import           LSymbol
import           Program.PSymbol

------------------------------------------------------------------------------------------
-- Data and type declaration

-- | Type of program in compiler
data Program

  -- | Assigning instruction iterates terms with respect to a given condition
  --   and assigns them to a given program variable
  = Assign
    {
      variables :: [Int],  -- ^ list of numbers of depended program variables
      variable  :: Int,    -- ^ number of a program variable
      generate  :: PTerm,  -- ^ generator of terms
      condition :: PTerm,  -- ^ condition for iterating of terms
      jump      :: Program -- ^ jump to next program fragment
    }

  -- | Branching instruction jumps to a given program fragment
  --   with respect to a given condition
  | Branch
    {
      variables :: [Int],   -- ^ list of numbers of depended program variables
      condition :: PTerm,   -- ^ condition for the branch
      branch    :: Program, -- ^ branch to program fragment
      jump      :: Program  -- ^ jump to next program fragment
    }

  -- | Switching instruction jumps to a program fragment defined by an evaluated number
  | Switch
    {
      variables :: [Int],     -- ^ list of numbers of depended program variables
      evaluate  :: PTerm,     -- ^ evaluation of number of program unit
      cases     :: [Program], -- ^ list of program units
      jump      :: Program    -- ^ jump to next program fragment
    }

  -- | Acting instruction performs a given action and terminates the program
  | Action
    {
      variables :: [Int], -- ^ list of numbers of depended program variables
      perform   :: PTerm  -- ^ performing of an action
    }

  -- | Empty represents an auxiliary program instruction
  | Empty

  deriving(Eq)

-- | Type of programs database
type Programs = M.Map LSymbol Program

------------------------------------------------------------------------------------------
-- Show instances

-- | Show instance for Program
instance Show Program where
  show = showProgram ""

-- | Show a program fragment corresponding to a given indent
showProgram :: String -> Program -> String

-- | Show an assigning instruction of program fragment corresponding to a given indent
showProgram ind (Assign vl v g c p) = let x = var v :: PSymbol
  in ind ++ show x ++ " <- " ++ show g ++ " | " ++ show c ++ "\n" ++ showProgram ind p

-- | Show a branching instruction of program fragment corresponding to a given indent
showProgram ind (Branch vl c b p) =
  ind ++ show c ++ "\n" ++ showProgram (' ':' ':ind) b ++ showProgram ind p

-- | Show a switching instruction of program fragment corresponding to a given indent
showProgram ind (Switch vl e cl p) =
  foldl f (ind ++ "case " ++ show e ++ " of\n") $ zip cl [1 .. length cl]
  where
    f :: String -> (Program, Int) -> String
    f x (y,i) = x ++ (' ':ind) ++ show i ++ ":\n" ++ showProgram (' ':' ':ind) y

-- | Show an acting instruction of program fragment corresponding to a given indent
showProgram ind (Action vl t) = ind ++ show t

-- | Show an empty program fragment corresponding to a given indent
showProgram ind Empty = ""

------------------------------------------------------------------------------------------
-- Read instances

-- | Read instance for Program
instance Read Program where
  readsPrec p = readProgram p 0

-- | Read a program fragment corresponding to a given indent
readProgram :: Int -> Int -> ReadS Program
readProgram p ind r =  readAssign p ind r
                    ++ readBranch p ind r
                    ++ readSwitch p ind r
                    ++ readAction p ind r
                    ++ readEmpty  p ind r

-- | Read an assigning instruction of program fragment corresponding to a given indent
readAssign :: Int -> Int -> ReadS Program
readAssign i ind r =
  [ (Assign [] (eval x) g c p, w) | s@(a:_) <- [skipSpaces r ind], a /= ' ',
                                    (x, ' ':'<':'-':' ':t) <- readPSymbol i s,
                                    (g, ' ':'|':' ':u) <- readPTerm i t,
                                    (c, '\n':v) <- readPTerm i u,
                                    (p, w) <- readProgram i ind v ]

-- | Read a branching instruction of program fragment corresponding to a given indent
readBranch :: Int -> Int -> ReadS Program
readBranch i ind r =
  [ (Branch [] c b p, v) | s@(a:_) <- [skipSpaces r ind], a /= ' ',
                           (c, '\n':t) <- readPTerm i s,
                           (b, u) <- readProgram i (ind+2) t,
                           (p, v) <- readProgram i ind u ]

-- | Read a case of switching instruction corresponding to a given indent
readSwitchCases :: Int -> Int -> ReadS [Program]
readSwitchCases i ind r =  [ (p:cl, v) | (' ':s@(a:_)) <- [skipSpaces r ind], isDigit a,
                                         (x, ':':'\n':t) <- lex s, all isDigit x,
                                         (p, u) <- readProgram i (ind+2) t,
                                         (cl, v) <- readSwitchCases i ind u ]
                        ++ [ ([], r) | (a:s) <- [skipSpaces r ind],  a /= ' ' ]

-- | Read a switching instruction of program fragment corresponding to a given indent
readSwitch :: Int -> Int -> ReadS Program
readSwitch i ind r =
  [ (Switch [] e cl p, v) | ('c':'a':'s':'e':' ':s) <- [skipSpaces r ind],
                            (e, ' ':'o':'f':'\n':t) <- readPTerm i s,
                            (cl, u) <- readSwitchCases i ind t,
                            (p, v) <- readProgram i ind u  ]

-- | Read an acting instruction of program fragment corresponding to a given indent
readAction :: Int -> Int -> ReadS Program
readAction i ind r =
  [ (Action [] t, u) | s@(a:_) <- [skipSpaces r ind], a /= ' ',
                       (t, '\n':u) <- readPTerm i s, isAction t ]

-- | Read an empty program fragment corresponding to a given indent
readEmpty :: Int -> Int -> ReadS Program
readEmpty i ind "" = [(Empty, "")]

-- | Skip in a string a given number of spaces
skipSpaces :: String -> Int -> String
skipSpaces str 0       = str
skipSpaces (' ':str) n = skipSpaces str (n-1)

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
              Right content -> return (addProgram s (read content) db)
              where
                  s = lsymbol (read $ takeBaseName file) lsym_db

  -- | Save a database of programs to a given directory
  save db (dir, lsym_db) = do
    createDirectoryIfMissing True dir
    mapM_ f (M.assocs db)
    where
      f :: (LSymbol, Program) -> IO ()
      f (s, p) = writeFile (dir ++ name s lsym_db ++ ".db") (show p)

-- | Database instance for program
instance Database (String, LSymbol, LSymbols) Program IO where

  -- | Load a program of logical symbol from a database saved in given directory
  load (dir, s, lsym_db) = do
    let file = dir ++ "/" ++ name s lsym_db ++ ".db"
    content <- try (readFile file) :: IO (Either IOError FilePath)
    case content of
      Left _        -> return Empty
      Right content -> return (read content)

  -- | Save a program of logical symbol to a database saved in given directory
  save p (dir, s, lsym_db) = do
    createDirectoryIfMissing True dir
    writeFile (dir ++ "/" ++ name s lsym_db ++ ".db") (show p)

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
