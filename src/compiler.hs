{-|
Module      : Compiler
Description : Implementation of the rules compiler
Copyright   : (c) Grigoriy Bokov, 2017
                  Gleb Kalachev, 2017
License     : GPL-3
Maintainer  : sample@email.com
Stability   : experimental
Portability : POSIX

Модуль содержит реализацию компилятора приемов.
-}
module Compiler
    (
      -- exports
      compile
    )
where

-- External imports
import           Control.Monad

-- Internal imports
import           Compiler.Tree
import           Compiler.Tree.Database
import           Rule
import           Term

-- |Type of filters depending on its usage
data FiltersRank = FiltersRank
  {
    context        :: [Term],
    unknownFilters :: [Term]
  }

-- |Get a ranked filters for a rule
rankFilters :: Rule -> IO FiltersRank
rankFilters rule = do
  return FiltersRank { context = filters rule, unknownFilters = [] }

-- |Type of specifiers depending on its usage
data SpecifiersRank = SpecifiersRank
  {
    simple            :: [Term],
    unknownSpecifiers :: [Term]
  }

-- |Get a ranked specifiers for a rule
rankSpecifiers :: Rule -> IO SpecifiersRank
rankSpecifiers rule = do
  return SpecifiersRank { simple = [], unknownSpecifiers = [] }

-- |Type of decision programs
data Program
  -- | Constructor for an identifying program
  = IdentProgram   { commands :: [Term], varsMap :: [(LSymbol, LSymbol)] }
  -- | Condtructor for a program of filters
  | FilterProgram  { commands :: [Term], varsMap :: [(LSymbol, LSymbol)] }
  -- | Condtructor for a checking program
  | CheckProgram   { commands :: [Term] }
  -- | Condtructor for a replacing program
  | ReplaceProgram { commands :: [Term] }
  -- | Condtructor for an empty program
  | EmptyProgram   { commands :: [Term] }

-- |Generate an identifying program
genIdentProg :: Rule -> IO Program
genIdentProg rule = do
  return IdentProgram { commands = [], varsMap = [] }

-- |Generate a program of filters
genFilterProg :: FiltersRank -> Program -> IO Program
genFilterProg f_rank i_prog = do
  return FilterProgram { commands = context f_rank, varsMap = [] }

-- |Generate a checking program
genCheckProg :: Rule -> SpecifiersRank -> Program -> Program -> IO Program
genCheckProg rule s_rank i_prog f_prog = do
  return CheckProgram { commands = [] }

-- |Generate a replacing program
genReplaceProg :: Rule -> Program -> Program -> IO Program
genReplaceProg rule i_prog f_prog = do
  return ReplaceProgram { commands = [] }

-- |Generate a decision tree for a rule
genTree :: Rule -> Program -> Program -> Program -> Program -> IO Tree
genTree rule i_prog f_prog c_prog r_prog = do
  return (Terminal (commands f_prog))

-- |Compile an inference rule
compile :: Rule -> IO ()
compile rule = do
  let file = "database/tree.db"
  f_rank <- rankFilters rule
  s_rank <- rankSpecifiers rule
  i_prog <- genIdentProg rule
  f_prog <- genFilterProg f_rank i_prog
  c_prog <- genCheckProg rule s_rank i_prog f_prog
  r_prog <- genReplaceProg rule i_prog f_prog
  tree <- genTree rule i_prog f_prog c_prog r_prog
  saveTree (symbol rule) tree file
