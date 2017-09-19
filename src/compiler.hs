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

    -- Switch to a conclusion
    parse (Const "then" : s) = parse s

    -- Parse a conclusion
    parse (Fun "equal" [from, to] : s) = if is_right
      then return ([], [], from, to)
      else return ([], [], to, from)
    parse (Fun "ekvivalentno" [from, to] : s) = if is_right
      then return ([], [], from, to)
      else return ([], [], to, from)

    -- Parse a premise
    parse (p : s) = do
      (bs, ps, from, to) <- parse s
      return (bs, p:ps, from, to)

    parse [] = error "Failed theorem conclusion"

-- |Get a theorem for an inference rule
getTheorem :: Rule -> IO Theorem
getTheorem rule = case Rule.header rule of
  Const "firstsubterm"  -> parseReplaceTheorem (theorem rule) False
  Const "secondsubterm" -> parseReplaceTheorem (theorem rule) True
  _                     -> return OtherTheorem

-- |Type of filters depending on its usage
data FiltersRank = FiltersRank
  {
    getUncondFilters  :: [Term], -- ^ a list of filters which does not depend on theorem variables
    getContextFilters :: [Term], -- ^ a list of filters containing the logical symbol 'kontekst'
    otherFilters      :: [Term]  -- ^ a list of another filters
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

-- |Generate an identifying program
genIdentProg :: Rule -> IO Program
genIdentProg rule = do
  thrm <- getTheorem rule
  let from = getFrom thrm
  let to   = getTo thrm
  let i = 1;
  let tt = Var ("p" ++ show i)
  let ctrm = Fun "current_term" [tt]
  (tl,_) <- ident from tt [ctrm] (i+1)
  return (IdentProgram tl [])
    where
      ident :: Monad m => Term -> Term -> [Term] -> Int -> m ([Term], Int)
      ident (Var _) cur_trm t_list idx = return (t_list, idx)
      ident trm cur_trm t_list idx = do
        let sterm = Fun "equal" [Fun "symbol" [cur_trm], Const (elementName (Term.header trm))]
        let sub_trms = operands trm
        let l = concat $ zipWith f sub_trms [0 .. length sub_trms - 1]
        return (t_list ++ (sterm : l), idx)
        where
          f :: Term -> Int -> [Term]
          f x i = do
            let nt = Var ("p" ++ show idx)
            let oterm = Fun "equal" [nt, Fun "operand" [cur_trm, Const (show i)]]
            (sub_tl,new_idx) <- ident x nt [oterm] (idx+1)
            let idx = new_idx
            sub_tl

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
