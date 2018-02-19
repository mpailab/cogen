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
import           Program.PSymbol   (PTerm, parsePTerm)
import qualified Program.PSymbol   as P
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

------------------------------------------------------------------------------------------
-- Parser instances

-- | Parser instance for Program
instance Parser Program where
  write  = writeProgram 0

  parse_ x db = case f (map g (lines x)) db of
    [(prog,[])] -> [(prog,"")]
    [] -> error ("\n" ++ (show . addWhere . pullWhere . foldWhere . pullChunk . skipEmpty) (map g (lines x)) ++ "\n")
    y -> error (h y)
    where
      h = foldr (\(p,z) u -> "\nprog: " ++ write p db ++ "\ntail: " ++ show z ++ u) "\n"

      f :: ParserS Program (Int, String)
      f = parseProgram . addWhere . pullWhere . foldWhere . pullChunk . skipEmpty

      g :: String -> (Int, String)
      g (' ':' ':x) = let (i,y) = g x in (i+1,y)
      g x           = (0, T.unpack $ T.strip $ T.pack x)

      skipEmpty :: [(Int, String)] -> [(Int, String)]
      skipEmpty ((_,""):s) = skipEmpty s
      skipEmpty (x:s)      = (x:(skipEmpty s))
      skipEmpty []         = []

      pullChunk :: [(Int, String)] -> [(Int, String)]
      pullChunk ((i,a):y@((j,b):s))
        | i < j-1 = pullChunk ((i,a++" "++b):s)
        | otherwise = ((i,a):(pullChunk y))
      pullChunk (x:s) = (x:(pullChunk s))
      pullChunk [] = []

      foldWhere :: [(Int, String)] -> [(Int, String)]
      foldWhere ((i,a):y@((j,b):(k,c):(l,d):s))
        | i == j-1 && j == k-1 && k == l && b =~ "where"
          = foldWhere ((i,a):(j,b):(k,c++" and "++d):s)
        | otherwise = ((i,a):(foldWhere y))
      foldWhere (x:s) = (x:(foldWhere s))
      foldWhere [] = []

      pullWhere :: [(Int, String)] -> [(Int, String)]
      pullWhere ((i,a):y@((j,b):(k,c):s))
        | i == j-1 && j == k-1 && b == "where"
          = pullWhere ((i,a ++ " where " ++ c):s)
        | otherwise = ((i,a):(pullWhere y))
      pullWhere (x:s) = (x:(pullWhere s))
      pullWhere [] = []

      addWhere :: [(Int, String)] -> [(Int, String)]
      addWhere ((i,a):y@((j,b):s))
        | a /= "do" && a /= "done" && not (elem "where" (words a)) && b /= "do"
          = addWhere ((i,a ++ " where True"):y)
        | otherwise = ((i,a):(addWhere y))
      addWhere (x:s) = (x:(addWhere s))
      addWhere [] = []

-- | Parse a program fragment
parseProgram :: ParserS Program (Int, String)
parseProgram x db
  =  parseAssign x db
  ++ parseBranch x db
  ++ parseSwitch x db
  ++ parseAction x db
  ++ parseEmpty  x db

-- | Parse a list of program fragments
parsePrograms :: ParserS [(PTerm, Program)] (Int, String)
parsePrograms ((i,a):(j,"do"):s) db
  = [ ((pat,prog):cs, t) | i == j,
      (pat,"") <- parsePTerm a db,
      (prog,u) <- parseProgram s db,
      (cs,t) <- if null u || fst (head u) < i then [([],u)] else parsePrograms u db ]
parsePrograms _ db = []

-- | Parse an assigning instruction
parseAssign :: ParserS Program (Int, String)
parseAssign ((i,a):s) db
  =  [ (Assign pat (P.list val) cond jump, t) |
       [s1, s2, s3] <- [cut a ["=", "where"]],
       (pat,"") <- parsePTerm s1 db,
       (val,"") <- parsePTerm s2 db,
       (cond,"") <- parsePTerm s3 db,
       (jump,t) <- parseProgram s db ]
  ++ [ (Assign pat gen cond jump, t) |
       [s1, s2, s3] <- [cut a ["<-", "where"]],
       (pat,"") <- parsePTerm s1 db,
       (gen,"") <- parsePTerm s2 db,
       (cond,"") <- parsePTerm s3 db,
       (jump,t) <- parseProgram s db ]
parseAssign _ db = []

-- | Parse a branching instruction
parseBranch :: ParserS Program (Int, String)
parseBranch ((i,a):(j,"do"):s@((k,_):_)) db
  =  [ (Branch cond br jump, t) | i == j && j == k-1,
       ["", s1] <- [cut a ["if"]],
       (cond,"") <- parsePTerm s1 db,
       (br,u) <- parseProgram s db,
       (jump,t) <- parseProgram u db ]
parseBranch _ db = []

-- | Parse a switching instruction
parseSwitch :: ParserS Program (Int, String)
parseSwitch x@((i,a):(j,b):s) db
  =  [ (Switch expr cond cs jump, t) |
       ["", s1, "", s2] <- [cut a ["case", "of", "where"]],
       (expr,"") <- parsePTerm s1 db,
       (cond,"") <- parsePTerm s2 db,
       (cs,u) <- if i == j-1 then parsePrograms ((j,b):s) db else [([],(j,b):s)],
       (jump,t) <- parseProgram u db ]
parseSwitch _ db = []

-- | Parse an acting instruction
parseAction :: ParserS Program (Int, String)
parseAction ((i,a):s) db
  =  [ (Action act cond jump, t) |
       [s1, s2] <- [cut a ["where"]],
       (act,"") <- parsePTerm s1 db, P.isAction act,
       (cond,"") <- parsePTerm s2 db,
       (jump,t) <- parseProgram s db ]
parseAction _ db = []

-- | Parse an empty program fragment corresponding to a given indent
parseEmpty :: ParserS Program (Int, String)
parseEmpty ((i,"done"):s) db = [ (Empty, s) ]
parseEmpty _ db              = []

cut :: String -> [String] -> [String]
cut s0 (p:ps) = let (s1,_,s2) = (s0 =~ (" *"++p++" *") :: (String,String,String))
                in s1:(cut s2 ps)
cut s0 [] = [s0]

writeIndent :: Int -> String
writeIndent 0 = ""
writeIndent n = ' ' : ' ' : writeIndent (n-1)

writeWhere :: Int -> [PTerm] -> LSymbols -> String
writeWhere ind (t:ts) db = writeIndent ind ++ write t db ++ "\n" ++ writeWhere ind ts db
writeWhere ind [] db = ""

-- | Write a program fragment corresponding to a given indent
writeProgram :: Int -> Program -> LSymbols -> String

-- | Write an assigning instruction of program fragment corresponding to a given indent
writeProgram ind (Assign pat (P.List :> [val]) (T (P.B True)) jump) db =
  writeIndent ind ++ write pat db ++ " = " ++ write val db  ++ "\n" ++
  writeProgram ind jump db

writeProgram ind (Assign pat (P.List :> [val]) (P.And :> conds) jump) db =
  writeIndent ind ++ write pat db ++ " = " ++ write val db  ++ "\n" ++
  writeIndent ind ++ "  where\n" ++
  writeWhere (ind+2) conds db ++
  writeProgram ind jump db

writeProgram ind (Assign pat (P.List :> [val]) cond jump) db =
  writeIndent ind ++ write pat db  ++ " = " ++ write val db  ++
  " where " ++ write cond db  ++ "\n" ++
  writeProgram ind jump db

writeProgram ind (Assign pat gen (T (P.B True)) jump) db =
  writeIndent ind ++ write pat db  ++ " <- " ++ write gen db  ++ "\n" ++
  writeProgram ind jump db

writeProgram ind (Assign pat gen (P.And :> conds) jump) db =
  writeIndent ind ++ write pat db  ++ " <- " ++ write gen db  ++ "\n" ++
  writeIndent ind ++ "  where\n" ++
  writeWhere (ind+2) conds db ++
  writeProgram ind jump db

writeProgram ind (Assign pat gen cond jump) db =
  writeIndent ind ++ write pat db  ++ " <- " ++ write gen db  ++
  " where " ++ write cond db  ++ "\n" ++
  writeProgram ind jump db

-- | Write a branching instruction of program fragment corresponding to a given indent
writeProgram ind (Branch cond br jump) db =
  writeIndent ind ++ "if " ++ write cond db  ++ "\n" ++
  writeIndent ind ++ "do\n" ++
  writeProgram (ind+1) br db ++
  writeProgram ind jump db

-- | Write a switching instruction of program fragment corresponding to a given indent
writeProgram ind (Switch expr (T (P.B True)) cs jump) db =
  writeIndent ind ++ "case " ++ write expr db  ++ " of\n" ++
  writeSwitchCases (ind+1) cs db ++
  writeProgram ind jump db

writeProgram ind (Switch expr (P.And :> conds) cs jump) db =
  writeIndent ind ++ "case " ++ write expr db  ++ " of\n" ++
  writeIndent ind ++ "  where\n" ++
  writeWhere (ind+2) conds db ++
  writeSwitchCases (ind+1) cs db ++
  writeProgram ind jump db

writeProgram ind (Switch expr cond cs jump) db =
  writeIndent ind ++ "case " ++ write expr db  ++ " of\n" ++
  " where " ++ write cond db  ++ "\n" ++
  writeSwitchCases (ind+1) cs db ++
  writeProgram ind jump db

-- | Write an acting instruction of program fragment corresponding to a given indent
writeProgram ind (Action act (T (P.B True)) jump) db =
  writeIndent ind ++ write act db  ++ "\n" ++
  writeProgram ind jump db

writeProgram ind (Action act (P.And :> cs) jump) db =
  writeIndent ind ++ write act db  ++ "\n" ++
  writeIndent ind ++ "  where\n" ++
  writeWhere (ind+2) cs db ++
  writeProgram ind jump db

writeProgram ind (Action act cond jump) db =
  writeIndent ind ++ write act db  ++
  " where " ++ write cond db  ++ "\n" ++
  writeProgram ind jump db

-- | Write an empty program fragment corresponding to a given indent
writeProgram ind Empty db =
  writeIndent ind ++ "done\n"

writeSwitchCases :: Int -> [(PTerm, Program)] -> LSymbols -> String
writeSwitchCases ind ((pat,prog):cs) db =
  writeIndent ind ++ write pat db  ++ "\n" ++
  writeIndent ind ++ "do\n" ++
  writeProgram (ind+1) prog db ++
  writeSwitchCases ind cs db
writeSwitchCases ind [] db = ""

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
