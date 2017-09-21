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
import           Control.Monad.State
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
parseReplaceTheorem :: Term -> Bool -> Theorem
parseReplaceTheorem (Fun "forall" t) is_right = ReplaceTheorem bs ps from to
  where
    (bs, ps, from, to) = parse t

    -- Parse a list of terms as the replacing theorem
    parse :: [Term] -> ([LSymbol], [Term], Term, Term)

    -- Parse a bounded variable
    parse (Var x : s) = let (bs, ps, from, to) = parse s in (x:bs, ps, from, to)

    -- Switch to a list of premises
    parse (Const "if" : s) = parse s

    -- Switch to a conclusion
    parse (Const "then" : s) = parse s

    -- Parse a conclusion
    parse (Fun "equal" [from, to] : s) = if is_right
      then ([], [], from, to)
      else ([], [], to, from)
    parse (Fun "ekvivalentno" [from, to] : s) = if is_right
      then ([], [], from, to)
      else ([], [], to, from)

    -- Parse a premise
    parse (p : s) = let (bs, ps, from, to) = parse s in (bs, p:ps, from, to)

    parse [] = error "Failed theorem conclusion"

-- |Get a theorem for an inference rule
getTheorem :: Rule -> Theorem
getTheorem rule = case Rule.header rule of
  Const "firstsubterm"  -> parseReplaceTheorem (theorem rule) False
  Const "secondsubterm" -> parseReplaceTheorem (theorem rule) True
  _                     -> OtherTheorem

-- |Type of filters depending on its usage
data FiltersRank = FiltersRank
  {
    getUncondFilters  :: [Term], -- ^ list of filters which does not depend on theorem variables
    getContextFilters :: [Term], -- ^ list of filters containing the logical symbol 'kontekst'
    otherFilters      :: [Term]  -- ^ list of another filters
  }

-- | Does a term represent an unconditional filter
isUncondFilter :: Term -> Bool
isUncondFilter t = case t of
  Fun "scanLevel" [Const "one"] -> True
  _                             -> False

-- |Get a ranked filters for a rule
rankFilters :: Rule -> IO FiltersRank
rankFilters rule = do
  let (cfs, fs)  = partition (`isContainLSymbol` "kontekst") (filters rule)
  let (ufs, ofs) = partition isUncondFilter fs
  return (FiltersRank ufs cfs ofs)

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

-- | Type of program information
data ProgInfo = ProgInfo
  {
    getTerms  :: [Term], -- ^ list of program terms
    getVarNum :: Int     -- ^ number of first free program variable
  }

-- | Identify a term
identTerm :: Term                 -- ^ theorem's term
          -> Term                 -- ^ program term
          -> State ProgInfo Bool  -- ^ return value and info

-- Case of theorem's variable
identTerm (Var _) _ = return True

-- General case
identTerm tt pt  = do
  info <- get
  let st = Fun "equal" [Fun "symbol" [pt], Const (elementName (Term.header tt))]
  let new_info = ProgInfo (getTerms info ++ [st]) (getVarNum info)
  let (res, info) = foldr f (True, new_info) (operands tt)
  put info
  return res
  where
    -- Identify a subterm of current theorems's term
    f :: Term -> (Bool, ProgInfo) -> (Bool, ProgInfo)
    f x (r,i) = (r && nr, ni)
      where
        (nr, ni) = runState (identTerm x p) (ProgInfo (getTerms i ++ [o]) (getVarNum i + 1))
        o = Fun "equal" [p, Fun "operand" [pt]]
        p = Var ("p" ++ show (getVarNum i))

-- |Generate an identifying program
genIdentProg :: Rule -> IO Program
genIdentProg rule = return (IdentProgram tl [])
  where
    tl = getTerms (execState (identTerm (getFrom thrm) pt) info)
    thrm = getTheorem rule
    info = ProgInfo [Fun "current_term" [pt]] (i+1)
    pt = Var ("p" ++ show i)
    i = 1

-- |Generate a program of filters
genFilterProg :: FiltersRank -> Program -> IO Program
genFilterProg f_rank i_prog = do
  let fs = getUncondFilters f_rank
  return (FilterProgram fs [])

-- |Generate a checking program
genCheckProg :: Rule -> SpecifiersRank -> Program -> Program -> IO Program
genCheckProg rule s_rank i_prog f_prog = return (CheckProgram [])

-- |Generate a replacing program
genReplaceProg :: Rule -> Program -> Program -> IO Program
genReplaceProg rule i_prog f_prog = return (ReplaceProgram [Const "replacing"])

-- |Generate a decision tree for a rule
genTree :: Rule -> Program -> Program -> Program -> Program -> IO Tree
genTree rule i_prog f_prog c_prog r_prog = do
  let t_node = Terminal (commands r_prog)
  let br_node_i = Branch [(commands i_prog, t_node)]
  let br_node_f = Branch [(commands f_prog, br_node_i)]
  return br_node_f

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
