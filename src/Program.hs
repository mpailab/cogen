{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}

{-|
Module      : Program
Description : Basic data types and classes for Coral programs
Copyright   : (c) Grigoriy Bokov 2017-2018
License     : GPL-3
Maintainer  : bokov@intsys.msu.ru
Stability   : experimental
Portability : POSIX

A Coral program is a collection of instructions that organizes as a tree in which every node represents an instruction performing a specific task. Each task is described by programs terms.

Program symbols are special symbol used for composing of program terms which are base elements of programs.

In order to add new program symbol need:
  1. add new constructor in data PSymbol,
  2. update Parse instances for PSymbol and PTerm in Program.Parser module,
  3. update Write instances for PSymbol and PTerm in module Program.Writer,
  4. add new evaluating functions in Program.Handler module.
-}
module Program
    (
      -- exports
      addProgram,
      getProgram,
      getPrograms,
      getPVar,
      getPVars,
      getPVarIfExist,
      initPrograms,
      initPVars,
      namePVar,
      newPrograms,
      newPVars,
      NameSpace,
      Header(..),
      PTerminal(..),
      Program(..),
      Command(..),
      Program.Base,
      Program.Vars,
      Programs,
      PExpr',
      PBool(..),
      PEntry(..),
      PExpr,
      PMType(..),
      PTerm(..),
      PVars,
      setPrograms,
      setPVars
    )
where

  -- External imports
import           Control.Monad
import           Control.Monad.State
import           Data.Array
import           Data.Char
import qualified Data.Map            as M
import           Data.Maybe
import           Text.Regex.Posix

-- Internal imports
import           Expr
import           LSymbol
import           Utils
import           Term

------------------------------------------------------------------------------------------
-- Data types and clases declaration

data PTerminal
  = X Int           -- ^ program variable
  | S LSymbol       -- ^ logic symbol
  | AnySymbol       -- ^ '_' symbol means any argument
  | AnySequence     -- ^ '__' means any sequence of program expressions
  | PV [PTerm]      -- ^ any of subterm variants
  -- | Func Int PExpr'  -- ^ function call
  | Frag [Command] -- ^ program fragment
  | ExtVar Int      -- ^ variable name, used only in fragments
  | IfElse PBool PExpr' PExpr'
  | CaseOf PExpr' [(PExpr', PExpr')]
  | Fun Program
  deriving (Eq, Ord,Show)

data PEntry
  = Ptr Int PExpr'
  | Ref Int PExpr'
  | Inside PExpr'
  deriving (Eq, Ord, Show)

-- | Type of program term
type PTerm = Term PTerminal

data PBool
  = Const Bool          -- ^ Boolean constant (True or False)
  | Equal PTerm PTerm   -- ^ statement A eq B
  | NEqual PTerm PTerm  -- ^ statement A ne B
  | In PTerm PExpr'     -- ^ statement A in B
  | Not PBool           -- ^ statement not A
  | And [PBool]         -- ^ statement A and B
  | Or [PBool]          -- ^ statement A or B
  | BVar Int            -- ^ Boolean global variable
  deriving (Eq, Ord, Show)

type PExpr' = Expr' PTerminal PEntry

-- | Type of program expressions
type PExpr = Expr PTerminal PEntry PBool

-- | type of assignment statement
data PMType = PMSelect -- ^ match patterns with list elements (l1,...,lN <- right)
            | PMUnord  -- ^ match list pattern with list of elements in any order (left ~= right)
            | PMAppend -- ^ appends right part to variable (left << right)
            deriving (Eq, Ord)

instance Show PMType where
  show PMAppend = " << "
  show PMUnord = " ~= "
  show PMSelect = " <- "

data Header = Header
  {
    name      :: String,
    arguments :: [PTerminal]
  }
  deriving (Eq, Ord,Show)

-- | Type of program statement
data Command

  -- | Assigning instruction iterates terms with respect to a given condition
  --   and assigns them to a given program variable
  = Assign
    {
      pmtype    :: PMType, -- ^ type of pattern matching in assignment
      pattern_  :: PExpr',  -- ^ assigned pattern
      generate  :: PExpr',  -- ^ generator of list of terms
      condition :: PBool   -- ^ condition for iterating of terms
    }

  -- | Branching instruction jumps to a given program fragment
  --   with respect to a given condition
  | Branch
    {
      condition :: PBool,   -- ^ condition for the branch
      branch    :: [Command]  -- ^ branch to program fragment
    }

  -- | Switching instruction jumps to a program fragment defined by a given expression
  | Switch
    {
      expression :: PExpr',              -- ^ expression
      condition  :: PBool,              -- ^ condition for switching
      cases      :: [(PExpr', PBool, [Command])]  -- ^ list of pairs (pattern, program fragment)
    }

  -- | Acting instruction performs a given action with respect to a given condition
  | Action
    {
      action    :: PExpr',  -- ^ action
      condition :: PBool   -- ^ condition of action
    }

  -- | program fragment variable with delayed substitution
  | DelayedFrag PTerm

  deriving(Eq,Ord,Show)

-- | Type of program : header + command list
data Program = Program Header [Command]
  | Empty
  deriving(Eq,Ord,Show)

-- | Type of programs database
type Programs = M.Map LSymbol Program

-- | Init a database of programs
initPrograms :: Programs
initPrograms = M.empty

-- | Base class of programs
class Monad m => Base m where
  {-# MINIMAL getPrograms, setPrograms #-}

  -- | Get a database of programs
  getPrograms :: m Programs

  -- | Set a database of programs
  setPrograms :: Programs -> m ()

  -- | Init a new database of programs
  newPrograms :: m Programs
  newPrograms = let db = initPrograms in setPrograms db >> return db

  -- | Get the program of logical symbol
  getProgram :: LSymbol -> m (Maybe Program)
  getProgram s = M.lookup s <$> getPrograms

  -- | Add a program of logical symbol to a database
  addProgram :: LSymbol -> Program -> m ()
  addProgram s p = getPrograms >>= setPrograms . M.insert s p

-- | Type for database of program variables
data PVars = PVars
  {
    names   :: Array Int String, -- ^ names of variable
    numbers :: M.Map String Int, -- ^ numbers of variables
    curNum  :: Int               -- ^ number of first free variable
  }

-- | Init a database of program variables
initPVars :: PVars
initPVars = PVars (array (1,0) []) M.empty 1

-- | Class of program variables
class Monad m => Vars m where
  {-# MINIMAL getPVars, setPVars #-}

  -- | Get a database of program variables
  getPVars :: m PVars

  -- | Set a database of program variables
  setPVars :: PVars -> m ()

  -- | Init a new database of program variables
  newPVars :: m PVars
  newPVars = let db = initPVars in setPVars db >> return db

  -- | Get the name of a program variable by its number
  namePVar :: Int -> m (Maybe String)
  namePVar n = getPVars >>= \db ->
    let m = curNum db
        x = if 0 < n && n < m then Just (names db ! (m - n)) else Nothing
    in return x

  -- | Get a program variable by its name
  getPVarIfExist :: String -> m (Maybe PTerminal)
  getPVarIfExist name = getPVars >>= \db -> return $ X <$> M.lookup name (numbers db)

  -- | Get a program variable by its name
  getPVar :: String -> m PTerminal
  getPVar name = getPVars >>= \db -> case M.lookup name (numbers db) of
    Just n  -> return (X n)
    Nothing -> let n = curNum db
                   new_db = PVars (listArray (1,n) (name : elems (names db)))
                                  (M.insert name n (numbers db))
                                  (n + 1)
               in setPVars new_db >> return (X n)

-- | Class for namespace of logical symbols and program variables
class (LSymbol.Base m, Program.Vars m) => NameSpace m

------------------------------------------------------------------------------------------
-- Functions
