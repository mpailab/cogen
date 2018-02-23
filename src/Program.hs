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
      Program.Base,
      getProgram,
      getPrograms,
      initPrograms,
      newPrograms,
      Program(..),
      Programs,
      PSymbol(..),
      PTerm,
      setPrograms
    )
where

  -- External imports
import           Control.Monad
import           Control.Monad.State
import           Data.Char
import qualified Data.Map            as M
import           Data.Maybe
import           Text.Regex.Posix

-- Internal imports
import           LSymbol
import           Term

------------------------------------------------------------------------------------------
-- Data types and clases declaration

-- | Type of program symbols
data PSymbol = X Int                    -- ^ program variable
             | I Int                    -- ^ integer constant
             | B Bool                   -- ^ boolean constant
             | S LSymbol                -- ^ user-defined logical symbol
             | List                     -- ^ list of terms
             | Tuple                    -- ^ tuple of terms
             | Not                      -- ^ logical negation
             | And                      -- ^ logical and
             | Or                       -- ^ logical or
             | Equal                    -- ^ equality of objects
             | NEqual                   -- ^ negation of equality of objects
             | In                       -- ^ including for elements of set
             | Args                     -- ^ arguments of term
             | Replace                  -- !
             deriving (Eq, Ord)

keywordsAction :: [PSymbol]
keywordsAction = [Replace]

-- | Type of program terms
type PTerm = Term PSymbol

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
  getProgram :: LSymbol -> m Program
  getProgram s = (fromJust . M.lookup s) <$> getPrograms

  -- | Add a program of logical symbol to a database
  addProgram :: LSymbol -> Program -> m ()
  addProgram s p = getPrograms >>= setPrograms . M.insert s p

------------------------------------------------------------------------------------------
-- Functions

-- | Does a program term correspond to an action
isAction :: PTerm -> Bool
isAction (x :> _) = x `elem` keywordsAction
