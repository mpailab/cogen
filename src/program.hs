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
      PSymbol,
      Program(..)
    )
where

  -- External imports
import           Control.Exception
import           Control.Monad
import           Data.Char
import qualified Data.Map          as M
import           System.Directory
import           System.FilePath
import           System.IO

-- Internal imports
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
type Programs = M.Map Int Program

------------------------------------------------------------------------------------------
-- Show instances

-- Show instance for Program
instance Show Program where
  show prog = show_ prog ""

show_ :: Program -> String -> String

show_ (Assign vl v g c p) ind = let x = var v :: PSymbol
  in ind ++ show v ++ " <- " ++ show g ++ " | " ++ show c ++ "\n" ++ show_ p ind

show_  (Branch vl c b p) ind =
  ind ++ show c ++ "\n" ++ show_ b (' ':' ':ind) ++ show_ p ind

show_ (Switch vl e cl p) ind =
  foldl f (ind ++ "case " ++ show e ++ " of\n") $ zip cl [1 .. length cl]
  where
    f :: String -> (Program, Int) -> String
    f x (y,i) = x ++ (' ':ind) ++ show i ++ ":\n" ++ show_ y (' ':' ':ind)

show_ (Action vl t) ind = ind ++ show t

show_ Empty ind = ""

------------------------------------------------------------------------------------------
-- Read instances

-- Read instance for Program
instance Read Program where
  readsPrec p = readProgram p 0

readProgram :: Int -> Int -> ReadS Program
readProgram p ind r =  readAssign p ind r
                    ++ readBranch p ind r
                    ++ readSwitch p ind r
                    ++ readAction p ind r
                    ++ readEmpty  p ind r

readAssign :: Int -> Int -> ReadS Program
readAssign i ind r =
  [ (Assign [] (eval x) g c p, w) | s@(a:_) <- [skipSpaces r ind], a /= ' ',
                                    (x, ' ':'<':'-':' ':t) <- readPSymbol i s,
                                    (g, ' ':'|':' ':u) <- readPTerm i t,
                                    (c, '\n':v) <- readPTerm i u,
                                    (p, w) <- readProgram i ind v ]

readBranch :: Int -> Int -> ReadS Program
readBranch i ind r =
  [ (Branch [] c b p, v) | s@(a:_) <- [skipSpaces r ind], a /= ' ',
                           (c, '\n':t) <- readPTerm i s,
                           (b, u) <- readProgram i (ind+2) t,
                           (p, v) <- readProgram i ind u ]

readSwitchCases :: Int -> Int -> ReadS [Program]
readSwitchCases i ind r =  [ (p:cl, v) | (' ':s@(a:_)) <- [skipSpaces r ind], isDigit a,
                                         (x, ':':'\n':t) <- lex s, all isDigit x,
                                         (p, u) <- readProgram i (ind+2) t,
                                         (cl, v) <- readSwitchCases i ind u ]
                        ++ [ ([], r) | (a:s) <- [skipSpaces r ind],  a /= ' ' ]

readSwitch :: Int -> Int -> ReadS Program
readSwitch i ind r =
  [ (Switch [] e cl p, v) | ('c':'a':'s':'e':' ':s) <- [skipSpaces r ind],
                            (e, ' ':'o':'f':'\n':t) <- readPTerm i s,
                            (cl, u) <- readSwitchCases i ind t,
                            (p, v) <- readProgram i ind u  ]

readAction :: Int -> Int -> ReadS Program
readAction i ind r =
  [ (Action [] t, u) | s@(a:_) <- [skipSpaces r ind], a /= ' ',
                       (t, '\n':u) <- readPTerm i s, isAction t ]

readEmpty :: Int -> Int -> ReadS Program
readEmpty i ind "" = [(Empty, "")]

skipSpaces :: String -> Int -> String
skipSpaces str 0       = str
skipSpaces (' ':str) n = skipSpaces str (n-1)

-- import Text.Parsec hiding (State)
-- import Text.Parsec.Indent
-- import Control.Monad.State
--
-- type ProgramParser = ParsecT String () (State SourcePos) Program
--
-- parse :: ProgramParser -> String -> Either ParseError Program
-- parse p str = runIndent "" $ runParserT p () "" str
--
-- aProgram :: ProgramParser
-- aProgram = do
--   b <- withBlock NamedList aName anItem
--   spaces
--   return b

-- An example of how to parse an indented tree of data in Haskell using Parsec and indents.
--
-- > import Control.Applicative
-- > import Data.Char (isSpace)
-- > import Data.Either.Utils (forceEither)
-- > import Data.Monoid
-- > import System.Environment (getArgs)
-- > import Text.Parsec hiding (many, optional, (<|>))
-- > import Text.Parsec.Indent
-- A basic tree structure:
--
-- > data Tree = Node [Tree] | Leaf String
-- A simple serialization function to easily check the result of our parsing:
--
-- > serializeIndentedTree tree = drop 2 $ s (-1) tree
-- >   where
-- >     s i (Node children) = "\n" <> (concat $ replicate i "    ") <> (concat $ map (s (i+1)) children)
-- >     s _ (Leaf text)     = text <> " "
-- Our main function and some glue:
--
-- > main = do
-- >     args <- getArgs
-- >     input <- if null args then return example else readFile $ head args
-- >     putStrLn $ serializeIndentedTree $ forceEither $ parseIndentedTree input
-- >
-- > parseIndentedTree input = runIndent "" $ runParserT aTree () "" input
-- The actual parser:
--
-- Note that the indents package works by storing a SourcePos in a State monad. Its combinators don't actually consume indentation, they just compare the column numbers. So where we consume spaces is very important.
--
-- > aTree = Node <$> many aNode
-- >
-- > aNode = spaces *> withBlock makeNode aNodeHeader aNode
-- >
-- > aNodeHeader = many1 aLeaf <* spaces
-- >
-- > aLeaf = Leaf <$> (many1 (satisfy (not . isSpace)) <* many (oneOf " \t"))
-- >
-- > makeNode leaves nodes = Node $ leaves <> nodes
-- An example tree:
--
-- > example = unlines [
-- >     "lorem ipsum",
-- >     "    dolor",
-- >     "    sit amet",
-- >     "    consectetur",
-- >     "        adipiscing elit dapibus",
-- >     "    sodales",
-- >     "urna",
-- >     "    facilisis"
-- >   ]
-- The result:
--
-- % runhaskell parseIndentedTree.lhs
-- lorem ipsum
--     dolor
--     sit amet
--     consectetur
--         adipiscing elit dapibus
--     sodales
-- urna
--     facilisis

------------------------------------------------------------------------------------------
-- Functions
--
-- -- | Initialize a database from one saved in given directory
-- openProgramDB :: String -> IO Programs
-- openProgramDB dir = do
--   dir_content <- try (listDirectory dir) :: IO (Either IOError [FilePath])
--   case dir_content of
--      Left _            -> return M.empty
--      Right dir_content -> foldM f M.empty dir_content
--      where
--        f :: Programs -> FilePath -> IO Programs
--        f db file = do
--          content <- try (readFile file) :: IO (Either IOError FilePath)
--          case content of
--             Left _        -> return db
--             Right content -> return (putProgram (read $ takeBaseName file) (read content) db)
--
-- -- | Close a database and save it in given directory
-- closeProgramDB :: Programs -> String -> IO ()
-- closeProgramDB db dir = do
--   createDirectoryIfMissing True dir
--   mapM_ f (M.assocs db)
--   where
--     f :: (L.Symbol, Program) -> IO ()
--     f (sym, prog) = writeFile (dir ++ show sym ++ ".db") (show prog)
--
-- -- | Get a program of logical symbol from a database
-- getProgram :: L.Symbol -> Programs -> Maybe Program
-- getProgram sym db = case M.lookup sym db of
--   Just prog -> return prog
--   Nothing   -> return Empty
--
-- -- | Load a program of logical symbol from a database saved in given directory
-- loadProgram :: L.Symbol -> String -> IO Program
-- loadProgram sym dir = do
--   content <- try (readFile (dir ++ show sym ++ ".db")) :: IO (Either IOError FilePath)
--   case content of
--     Left _        -> return Empty
--     Right content -> return (read content)
--
-- -- | Put a program of logical symbol to a database
-- putProgram :: L.Symbol -> Program -> Programs -> Programs
-- putProgram = M.insert
--
-- -- | Save a program of logical symbol to a database saved in given directory
-- saveProgram :: L.Symbol -> Program -> String -> IO ()
-- saveProgram sym prog dir = do
--   createDirectoryIfMissing True dir
--   writeFile (dir ++ "/" ++ show sym ++ ".db") (show prog)
