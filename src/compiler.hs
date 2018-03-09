{-|
Module      : Compiler
Description : Implementation of the rules compiler
Copyright   : (c) Grigoriy Bokov, 2017
                  Gleb Kalachev, 2017
License     : GPL-3
Maintainer  : sample@email.com
Stability   : experimental
Portability : POSIX

Module contains an implementation of the rules compiler
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
import           Data.List.Utils
import           Data.Maybe

-- Internal imports
import           Compiler.Info
import           Compiler.Program
import           LSymbol
import           Rule
import           Term

-- | Parse a term as the theorem of an inference rule
parseReplaceTheorem :: Term -> Bool -> State Info ()
parseReplaceTheorem (Forall :> ts) is_right = do
  let (bs, ps, from, to) = parse ts
  addInfoUnit Bounds bs
  addInfoUnit Premises ps
  addInfoUnit From from
  addInfoUnit To to
  where
    -- Parse a list of terms as the replacing theorem
    parse :: [Term] -> ([LSymbol], -- ^ a list of bouned variables
                        [Term],    -- ^ a list of premises
                        Term,      -- ^ a substituted term
                        Term)      -- ^ a substituting term

    -- Parse a bounded variable
    parse (Var x : s) = let (bs, ps, from, to) = parse s in (x:bs, ps, from, to)

    -- Switch to a list of premises
    parse (Const If : s) = parse s

    -- Switch to a conclusion
    parse (Const Then : s) = parse s

    -- Parse a conclusion
    parse (Equal :> [from, to] : s) = if is_right
      then ([], [], from, to)
      else ([], [], to, from)
    parse (Equivalence :> [from, to] : s) = if is_right
      then ([], [], from, to)
      else ([], [], to, from)

    -- Parse a premise
    parse (p : s) = let (bs, ps, from, to) = parse s in (bs, p:ps, from, to)

    parse [] = error "Failed theorem conclusion"

-- | Parse the theorem of inference rule
parseTheorem :: State Info ()
parseTheorem = do
  rule <- getRule
  case Rule.header rule of
    Const LeftToRight -> parseReplaceTheorem (theorem rule) True
    Const RightToLeft -> parseReplaceTheorem (theorem rule) False

-- | Modify the theorem of inference rule
modifyTheorem :: State Info ()
modifyTheorem = modify id

-- | Bind the theorem of inference rule to a logical symbol
bindTheorem :: State Info ()
bindTheorem = do
  rule <- getRule
  addInfoUnit BindSymbol (symbol rule)

-- | Prepare the theorem of inference rule
prepTheorem :: State Info ()
prepTheorem = parseTheorem >> modifyTheorem >> bindTheorem

-- | Prepare the filters of inference rule
prepFilters :: State Info ()
prepFilters = modify id

-- | Prepare the specifiers of inference rule
prepSpecifiers :: State Info ()
prepSpecifiers = modify id

-- | Identify a term
identTerm :: Term          -- ^ theorem's term
          -> Int           -- ^ number of program variable of the term
          -> State Info () -- ^ information structure

-- Case of theorem's variable
identTerm (Var x) n = do
  mb_m <- getInfoUnit x
  if isJust mb_m
    then let m = fromJust mb_m
         in addProgChunk (Branch [m, n] (Equal :> [Var $ P m, Var $ P n]) Empty Empty)
    else addInfoUnit x n

-- General case
identTerm t n  = do
  let p = P n
  let v = Var p
  addProgChunk (Branch [n] (Equal :> [Header :> [v], Const (Term.header t)]) Empty Empty)
  foldM_ (\prevs st ->
    do
      m <- newProgVarNum
      addInfoUnit (P m) (Operand :> [v])
      addProgChunk (Assign [n] m (Operands :> [v]) (cond prevs m) Empty)
      identTerm st m
      return (prevs ++ [m]))
    [] (operands t)
  where
    cond []  _ = Const LTrue
    cond [j] i = Neg :> [Equal :> [Var $ P i, Var $ P j]]
    cond  s  i = And :> map (\j -> Neg :> [Equal :> [Var $ P i, Var $ P j]]) s

-- | Make an identifying program of inference rule
makeIdentProg :: State Info ()
makeIdentProg = do
  t <- getInfoUnit From
  identTerm (fromJust t) 1

-- | Make a filtering program of inference rule
makeFilterProg :: State Info ()
makeFilterProg = modify id

-- | Make a checking program of inference rule
makeCheckProg :: State Info ()
makeCheckProg = modify id

replaceTerm :: Term            -- ^ theorem's term
            -> [Int]
            -> State Info (Term, [Int]) -- ^ information structure

-- Case of theorem's variable
replaceTerm (Var x) s = do
  mb_n <- getInfoUnit x
  let n = fromJust mb_n
  state $ \info -> ((Var $ P n, n:s), info)

-- Case of constant
replaceTerm t@(Const _) s = state $ \info -> ((t,s), info)

-- General case
replaceTerm t s = do
  x <- forM (operands t) (`replaceTerm` s)
  let (ts,vs) = unzip x
  state $ \info -> ((Term.header t :> ts, concat vs), info)

-- | Make a replacing program of inference rule
makeReplaceProg :: State Info ()
makeReplaceProg = do
  t <- getInfoUnit To
  (pt,vs) <- replaceTerm (fromJust t) []
  addProgChunk (Action (sort $ uniq vs) (Replacing :> [pt]))

linkProgChunks :: [Program] -> State Info Program

linkProgChunks (Assign vl v g c Empty : Branch _ bc Empty Empty : ps) = do
  let new_c = case c of
                And :> ts   -> And :> (ts ++ [bc])
                Const LTrue -> bc
                _           -> And :> [c, bc]
  linkProgChunks (Assign vl v g new_c Empty : ps)

linkProgChunks (Assign vl v g c Empty : ps) = do
  prog <- linkProgChunks ps
  state $ \info -> (Assign vl v g c prog, info)

linkProgChunks (Branch vl c Empty Empty : ps) = do
  prog <- linkProgChunks ps
  state $ \info -> (Branch vl c prog Empty, info)

linkProgChunks (Switch vl e cl Empty : ps) = do
  prog <- linkProgChunks ps
  state $ \info -> (Switch vl e cl prog, info)

linkProgChunks (Action vl t : ps) = do
  prog <- linkProgChunks ps
  state $ \info -> (Action vl t, info)

linkProgChunks [] = state $ \info -> (Empty, info)

-- | Link program fragments of inference rule
linkProgram :: State Info (Program, LSymbol)
linkProgram = do
  chunks <- getProgChunks
  prog <- linkProgChunks chunks
  mb_sym <- getInfoUnit BindSymbol
  let sym = fromJust mb_sym
  state $ \info -> ((prog, sym), info)

-- | Make a program of inference rule
make :: Rule -> State Info (Program, LSymbol) -> (Program, LSymbol)
make rule = (`evalState` initInfo rule)

-- | Compile an inference rule
compile :: Rule -> IO ()
compile rule = do
  let file = "database/programs"
  let (prog, sym) = make rule $ do prepFilters
                                   prepSpecifiers
                                   prepTheorem
                                   makeIdentProg
                                   makeFilterProg
                                   makeCheckProg
                                   makeReplaceProg
                                   linkProgram
  saveProgram sym prog file
