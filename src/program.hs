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
      pattern   :: PTerm,  -- ^ assigned pattern
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
-- Parser instances

-- | Parser instance for Program
instance Parser Program where
  parse_ = parseProgram 0
  write  = writeProgram 0

-- | Parse a program fragment corresponding to a given indent
parseProgram :: Int -> ParserS Program
parseProgram ind str db
  =  parseAssign ind str db
  ++ parseBranch ind str db
  ++ parseSwitch ind str db
  ++ parseAction ind str db
  ++ parseEmpty  ind str db

-- | Skip in a string a given number of indents
skipIndent :: Int -> String -> String
skipIndent 0 str = str
skipIndent n (' ':' ':str) = skipIndent (n-1) str

-- | Parse a given indent
parseIndent :: Int -> String -> [String]
parseIndent ind str
  =  [(a:_) <- [skipIndent ind], not isSeparator a ]
  ++ [(a:_) <- parseIndent ind $ skip " *\\n" str, not isSeparator a ]

-- | Parse a where statement of program fragment corresponding to a given indent
parseWhere :: Int -> ParserS PTerm
parseWhere ind s0 db
  =  [ (x, s1) | (x,s1) <- parsePTerm (skip " +where +" s0) db, isBool t ]
  ++ [ (x, s2) | s1 <- parseIndent ind s0,
                 (x,s2) <- parseWhere (ind+1) (skip "where" s1) db ]
  ++ [ (P.and x y, s3) | s1 <- parseIndent ind s0,
                         (x,s2) <- parsePTerm s1 db, isBool t,
                         (y,s3) <- parseWhere ind s2 db ]
  ++ [ ([], s0) | (a:_) <- parseIndent (ind-2) s0, not isSeparator a ]

-- | Parse an assigning instruction of program fragment corresponding to a given indent
parseAssign :: Int -> ParserS Program
parseAssign ind s0 db
  =  [ (Assign pat (P.list val) cond jump, s5) |
       s1 <- parseIndent ind s0,
       (pat,s2) <- parsePTerm s1 db,
       (val,s3) <- parsePTerm (skip " += +" s2) db,
       (cond,s4) <- parseWhere (ind+1) s3 db,
       (jump,s5) <- parseProgram ind s4 db ]
  ++ [ (Assign pat gen cond jump, s5) |
       s1 <- parseIndent ind s0,
       (pat,s2) <- parsePTerm s1 db,
       (gen,s3) <- parsePTerm (skip " +<- +" s2) db,
       (cond,s4) <- parseWhere (ind+1) s3 db,
       (jump,s5) <- parseProgram ind s4 db ]

-- | Parse a branching instruction of program fragment corresponding to a given indent
parseBranch :: Int -> ParserS Program
parseBranch ind s0 db
  =  [ (Branch cond br jump, s5) |
       s1 <- parseIndent ind s0,
       (cond,s2) <- parsePTerm (skip "if " s1) db, isBool cond,
       s3 <- parseIndent ind s2,
       (br,s4) <- parseProgram (ind+1) (skip "do" s3) db,
       (jump,s5) <- parseProgram ind s4 db ]

-- | Parse a case of switching instruction corresponding to a given indent
parseSwitchCases :: Int -> ParserS [Program]
parseSwitchCases ind s0 db
  =  [ ((pat,prog):cs, s5) |
       s1 <- parseIndent ind s0,
       (pat,s2) <- parsePTerm s1 db,
       s3 <- parseIndent ind s2,
       (prog,s4) <- parseProgram (ind+1) (skip "do" s3) db,
       (cs,s5) <- parseSwitchCases ind s4 db ]
  ++ [ (lift [], s0) |
       (a:_) <- parseIndent (ind-2) s0, not isSeparator a ]

-- | Parse a switching instruction of program fragment corresponding to a given indent
parseSwitch :: Int -> ParserS Program
parseSwitch ind s0 db
  =  [ (Switch expr cs jump, s4) |
       s1 <- parseIndent ind s0,
       (expr,s2) <- parsePTerm (skip "case " s1) db,
       (cs,s3) <- parseSwitchCases (ind+1) (skip " *of" s2) db,
       (jump,s4) <- parseProgram ind s3 db ]

-- | Parse an acting instruction of program fragment corresponding to a given indent
parseAction :: Int -> ParserS Program
parseAction ind s0 db
  =  [ (Action act cond jump, s4) |
       s1 <- parseIndent ind s0,
       (act,s2) <- parsePTerm s1 db, isAction act,
       (cond,s3) <- parseWhere (ind+1) s2 db,
       (jump,s4) <- parseProgram ind s3 db ]

-- | Parse an empty program fragment corresponding to a given indent
parseEmpty :: Int -> ParserS Program
parseEmpty ind s0 db
  =  [ (Empty, s2) |
       s1 <- parseIndent ind s0,
       s2 <- [skip "done" s1] ]

writeIndent :: Int -> String
writeIndent 0 = ""
writeIndent n = ' ' : ' ' : writeIndent (n-1)

writeWhere :: Int -> [PTerm] -> LSymbols -> String
writeWhere ind (t:ts) db = writeIndent ind ++ write t db ++ "\n" ++ writeWhere ind ts db
writeWhere ind [] db = ""

-- | Write a program fragment corresponding to a given indent
writeProgram :: Int -> Program -> LSymbols -> String

-- | Write an assigning instruction of program fragment corresponding to a given indent
writeProgram ind (Assign pat (List :> [val]) (And :> cs) jump) db =
  writeIndent ind ++ write pat db ++ " = " ++ write val db  ++ "\n" ++
  writeIndent ind ++ "  where\n" ++
  writeWhere (ind+2) cs db ++
  writeProgram ind jump db

writeProgram ind (Assign pat (List :> [val]) cond jump) db =
  writeIndent ind ++ write pat db  ++ " = " ++ write val db  ++
  " where " ++ write cond db  ++
  writeProgram ind jump db

writeProgram ind (Assign pat gen (And :> cs) jump) db =
  writeIndent ind ++ write pat db  ++ " <- " ++ write gen db  ++ "\n" ++
  writeIndent ind ++ "  where\n" ++
  writeWhere (ind+2) cs db ++
  writeProgram ind jump db

writeProgram ind (Assign pat gen cond jump) db =
  writeIndent ind ++ write pat db  ++ " <- " ++ write gen db  ++
  " where " ++ write cond db  ++
  writeProgram ind jump db

-- | Write a branching instruction of program fragment corresponding to a given indent
writeProgram ind (Branch cond br jump) db =
  writeIndent ind ++ "if " ++ write cond db  ++ "\n" ++
  writeIndent ind ++ "do\n" ++
  writeProgram (ind+1) (br,db) ++
  writeProgram ind jump db

-- | Write a switching instruction of program fragment corresponding to a given indent
writeProgram ind (Switch expr cs jump) db =
  writeIndent ind ++ "case " ++ write expr db  ++ " of\n" ++
  writeSwitchCases (ind+1) cs db ++
  writeProgram ind jump db
  where
    writeSwitchCases :: Int -> [(PTerm, Program)] -> LSymbols -> String
    writeSwitchCases ind ((pat,prog):cs) db =
      writeIndent ind ++ write pat db  ++ "\n" ++
      writeIndent ind ++ "do\n" ++
      writeProgram (ind+1) (prog,db) ++
      writeSwitchCases ind cs db
    writeSwitchCases ind [] db = ""

-- | Write an acting instruction of program fragment corresponding to a given indent
writeProgram ind (Action act (And :> cs) jump) db =
  writeIndent ind ++ write act db  ++ "\n" ++
  writeIndent ind ++ "  where\n" ++
  writeWhere (ind+2) cs db ++
  writeProgram ind jump db

writeProgram ind (Action act cond jump) db =
  writeIndent ind ++ write act db  ++
  " where " ++ write cond db  ++
  writeProgram ind jump db

-- | Write an empty program fragment corresponding to a given indent
writeProgram ind Empty db =
  writeIndent ind ++ "done"

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
