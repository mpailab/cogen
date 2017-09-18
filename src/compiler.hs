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
import           Data.List

-- Internal imports
import           Compiler.Tree
import           Compiler.Tree.Database
import           Rule
import           Term

-- |Type of theorems
data Theorem
  -- | Constructor for a replacing theorem
  = ReplaceTheorem
    {
      getBounds   :: [LSymbol], -- ^ a list of bouned variables
      getPremises :: [Term],    -- ^ a list of premises
      getFrom     :: Term,      -- ^ a substituted term
      getTo       :: Term       -- ^ a substituting term
    }
  -- | Constructor for another theorems
  | OtherTheorem

-- |Parse a term as the theorem of an inference rule
parseReplaceTheorem :: Term -> Bool -> IO Theorem
parseReplaceTheorem (Fun "forall" t) is_right = do
  (bs, ps, from, to) <- parse t
  return (ReplaceTheorem bs ps from to)
  where
    -- Parse a list of terms as the replacing theorem
    parse :: Monad m => [Term] -> m ([LSymbol], [Term], Term, Term)

    -- Parse a bounded variable
    parse (Var x : s) = do
      (bs, ps, from, to) <- parse s
      return (x:bs, ps, from, to)

    -- Switch to a list of premises
    parse (Const "if" : s) = parse s

    -- Parse a conclusion
    parse (Const "then" : s) = case s of
      (Fun "equal" [from, to] : ss) -> if is_right
        then return ([], [], from, to)
        else return ([], [], to, from)
      _                             -> error "Failed theorem conclusion"

    -- Parse a premise
    parse (p : s) = do
      (bs, ps, from, to) <- parse s
      return (bs, p:ps, from, to)

-- |Get a theorem for an inference rule
getTheorem :: Rule -> IO Theorem
getTheorem rule = case Rule.header rule of
  Const "firstsubterm"  -> parseReplaceTheorem (theorem rule) False
  Const "secondsubterm" -> parseReplaceTheorem (theorem rule) True
  _                     -> return OtherTheorem

-- |Type of filters depending on its usage
data FiltersRank = FiltersRank
  {
    getContext   :: [Term], -- ^ a list of filters containing the logical symbol 'kontekst'
    otherFilters :: [Term]  -- ^ a list of another filters
  }

-- |Get a ranked filters for a rule
rankFilters :: Rule -> IO FiltersRank
rankFilters rule = do
  let (cfs, ofs) = partition (`isContainLSymbol` "kontekst") (filters rule)
  return (FiltersRank cfs ofs)

-- |Type of specifiers depending on its usage
data SpecifiersRank = SpecifiersRank
  {
    simple            :: [Term],
    unknownSpecifiers :: [Term]
  }

-- |Get a ranked specifiers for a rule
rankSpecifiers :: Rule -> IO SpecifiersRank
rankSpecifiers rule = return (SpecifiersRank [] [])

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
genIdentProg rule = return (IdentProgram [] [])

-- |Generate a program of filters
genFilterProg :: FiltersRank -> Program -> IO Program
genFilterProg f_rank i_prog = return (FilterProgram (getContext f_rank) [])

-- |Generate a checking program
genCheckProg :: Rule -> SpecifiersRank -> Program -> Program -> IO Program
genCheckProg rule s_rank i_prog f_prog = return (CheckProgram [])

-- |Generate a replacing program
genReplaceProg :: Rule -> Program -> Program -> IO Program
genReplaceProg rule i_prog f_prog = return (ReplaceProgram [])

-- |Generate a decision tree for a rule
genTree :: Rule -> Program -> Program -> Program -> Program -> IO Tree
genTree rule i_prog f_prog c_prog r_prog = return (Terminal (commands f_prog))

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
