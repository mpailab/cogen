{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE DeriveFunctor         #-}

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
      newLocalVar,
      initPrograms,
      initPVars,
      namePVar,
      newPrograms,
      newPVars,
      removeDbgInfo,
      NameSpace,
      Header(..),
      Program(..),
      --Command(..),
      Program.Base,
      Program.Vars,
      Programs,
      ProgramS,
      ProgramsS,
      --PAssign(..),
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
import           DebugInfo

------------------------------------------------------------------------------------------
-- Data types and clases declaration

data Header = Header
  {
    name      :: Var,
    arguments :: [Var]
  }
  deriving (Eq, Ord,Show)

-- | Type of program : header + command list
data Program d
  = Program Header [Command d]
  | Empty
  deriving (Eq,Ord,Show,Functor)

-- | Type of programs database
type Programs d = M.Map LSymbol (Program d)

type ProgramS = Program SrcInfo
type ProgramsS = Programs SrcInfo

removeDbgInfo :: Program d -> Program ()
removeDbgInfo = fmap (\_ -> ())

-- | Init a database of programs
initPrograms :: ProgramsS
initPrograms = M.empty

-- | Base class of programs
class Monad m => Base m where
  {-# MINIMAL getPrograms, setPrograms #-}

  -- | Get a database of programs
  getPrograms :: m ProgramsS

  -- | Set a database of programs
  setPrograms :: ProgramsS -> m ()

  -- | Init a new database of programs
  newPrograms :: m ProgramsS
  newPrograms = let (db :: ProgramsS) = initPrograms in setPrograms db >> return db

  -- | Get the program of logical symbol
  getProgram :: LSymbol -> m (Maybe ProgramS)
  getProgram s = M.lookup s <$> getPrograms

  -- | Add a program of logical symbol to a database
  addProgram :: LSymbol -> ProgramS -> m ()
  addProgram s p = getPrograms >>= setPrograms . M.insert s p

-- | Type for database of program variables
data PVars = PVars
  {
    names    :: Array Int String,   -- ^ names of variable
    numbers  :: [M.Map String Int], -- ^ numbers of variables
    curNum   :: Int                 -- ^ number of first free variable
  }

-- | Init a database of program variables
initPVars :: PVars
initPVars = PVars (array (1,0) []) [M.empty] 1

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

  newContext :: m ()
  newContext = getPVars >>= \db -> setPVars $ db {numbers = (head $ numbers db):numbers db}

  endContext :: m ()
  endContext = getPVars >>= \db -> setPVars $ db {numbers = tail (numbers db)}

  newLocalVar :: d -> String -> m (Expr d)
  newLocalVar dd name = getPVars >>= \db ->
              let n = curNum db
                  new_db = PVars (listArray (1,n) (name : elems (names db)))
                                  (M.insert name n (head $ numbers db):tail (numbers db))
                                  (n + 1)
              in setPVars new_db >> return (Expr (Var' n) dd)

  -- | Get the name of a program variable by its number
  namePVar :: Int -> m (Maybe String)
  namePVar n = getPVars >>= \db ->
    let m = curNum db
        x = if 0 < n && n < m then Just (names db ! (m - n)) else Nothing
    in return x

  -- | Get a program variable by its name
  getPVarIfExist :: d -> String -> m (Maybe (Expr d))
  getPVarIfExist dd name = getPVars >>= \db -> return $ (\v -> Expr (Var' v) dd) <$> M.lookup name (head $ numbers db)

  -- | Get a program variable by its name
  getPVar :: d -> String -> m (Expr d)
  getPVar dd name = getPVars >>= \db -> case M.lookup name (head $ numbers db) of
    Just n  -> return (Expr (Var' n) dd)
    Nothing -> let n = curNum db
                   new_db = PVars (listArray (1,n) (name : elems (names db)))
                                  (M.insert name n (head $ numbers db):tail (numbers db))
                                  (n + 1)
               in setPVars new_db >> return (Expr (Var' n) dd)

-- | Class for namespace of logical symbols and program variables
class (LSymbol.Base m, Program.Vars m) => NameSpace m

------------------------------------------------------------------------------------------
-- Functions
