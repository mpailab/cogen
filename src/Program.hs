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
      initPrograms,
      initPVars,
      namePVar,
      newPrograms,
      newPVars,
      NameSpace,
      Program(..),
      Program.Base,
      Program.Vars,
      Programs,
      PVars,
      PTerm(..),
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
import           LSymbol
import           Utils

------------------------------------------------------------------------------------------
-- Data types and clases declaration

-- | Type of program terms
data PTerm = X Int                          -- ^ program variable
           | I Int                          -- ^ integer constant
           | B Bool                         -- ^ boolean constant
           | S LSymbol                      -- ^ user-defined logical symbol
           | Term PTerm PTerm               -- ^ logical term
           | List [PTerm]                   -- ^ list of terms
           | Tuple [PTerm]                  -- ^ tuple of terms
           | Not PTerm                      -- ^ logical negation
           | And [PTerm]                    -- ^ logical and
           | Or [PTerm]                     -- ^ logical or
           | Equal PTerm PTerm              -- ^ equality of objects
           | NEqual PTerm PTerm             -- ^ negation of equality of objects
           | In PTerm PTerm                 -- ^ including for elements of set
           | Args PTerm                     -- ^ arguments of term
           | Replace PTerm                  -- !
           deriving (Eq, Ord)

-- | Type of program
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
  getPVar :: String -> m PTerm
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
