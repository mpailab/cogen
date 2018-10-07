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
      Program(..),
      Command(..),
      Program.Base,
      Program.Vars,
      Programs,
      PAssign(..),
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
import           Term
import           Utils

------------------------------------------------------------------------------------------
-- Data types and clases declaration

-- | type of assignment statement
data PAssign
  = Select -- ^ match patterns with list elements (l1,...,lN <- right)
  | Unord  -- ^ match list pattern with list of elements in any order (left ~= right)
  | Append -- ^ appends right part to variable (left << right)
  deriving (Eq, Ord)

instance Show PAssign where
  show Select = " <- "
  show Unord  = " ~= "
  show Append = " << "

data Header = Header
  {
    name :: LSymbol,
    args :: [Var]
  }
  deriving (Eq, Ord,Show)

-- | Type of program statement
data Command

  -- | Assigning instruction iterates terms with respect to a given condition
  --   and assigns them to a given program variable
  = Assign
    {
      assign    :: PAssign, -- ^ type of pattern matching in assignment
      pattern_  :: PExpr,   -- ^ assigned pattern
      generate  :: PExpr,   -- ^ generator of list of terms
      condition :: PBool    -- ^ condition for iterating of terms
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
      expression :: PExpr,              -- ^ expression
      condition  :: PBool,              -- ^ condition for switching
      cases      :: [(PExpr, PBool, [Command])] -- ^ list of cases (p,c,f), where
                                                -- p is a pattern of the case
                                                -- c is a condition of the case
                                                -- f is a list of commands of the case
    }

  -- | Acting instruction performs a given action with respect to a given condition
  | Action
    {
      action    :: PExpr,  -- ^ action
      condition :: PBool   -- ^ condition of action
    }

  deriving (Eq,Ord,Show)

-- | Type of program : header + command list
data Program
  = Program Header [Command]
  | Empty
  deriving (Eq,Ord,Show)

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
