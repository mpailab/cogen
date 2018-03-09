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
import qualified Program             as P
import           Rule
import qualified Symbol              as L
import           Term

-- | Parse the theorem of inference rule
parseTheorem :: Compiler ()
parseTheorem = do
  rule <- getRule
  case Rule.header rule of
    Const LeftToRight -> parseReplaceTheorem (theorem rule) True
    Const RightToLeft -> parseReplaceTheorem (theorem rule) False

-- | Modify the theorem of inference rule
modifyTheorem :: Compiler ()
modifyTheorem = modify id

-- | Bind the theorem of inference rule to a logical symbol
bindTheorem :: Compiler ()
bindTheorem = do
  rule <- getRule
  addInfoUnit BindSymbol (symbol rule)

-- | Prepare the theorem of inference rule
prepTheorem :: Compiler ()
prepTheorem = parseTheorem >> modifyTheorem >> bindTheorem

-- | Prepare the filters of inference rule
prepFilters :: Compiler ()
prepFilters = modify id

-- | Prepare the specifiers of inference rule
prepSpecifiers :: Compiler ()
prepSpecifiers = modify id

-- | Identify a term
identTerm :: Term          -- ^ theorem's term
          -> Int           -- ^ number of program variable of the term
          -> Compiler () -- ^ information structure

-- Case of theorem's variable
identTerm (Var x) n = do
  mb_m <- getInfoUnit x
  if isJust mb_m
    then let m = fromJust mb_m
         in addProgChunk (Branch [m, n] (P.equal (P.var m) (P.var n)) Empty Empty)
    else addInfoUnit x n

-- General case
identTerm t n  = do
  let v = P.var n
  addProgChunk (Branch [n] (P.equal (P.header v) (P.cons (Term.header t))) Empty Empty)
  foldM_ (\prevs st ->
    do
      m <- newProgVarNum
      addProgChunk (Assign [n] m (P.operands v) (cond prevs m) Empty)
      identTerm st m
      return (prevs ++ [m]))
    [] (operands t)
  where
    cond []  _ = P.cons True
    cond [j] i = P.neq (P.var i) (P.var j)
    cond  s  i = P.and $ map (P.neq (P.var i) . P.var) s

-- | Make an identifying program of inference rule
makeIdentProg :: Compiler ()
makeIdentProg = do
  t <- getInfoUnit From
  identTerm (fromJust t) 1

-- | Make a filtering program of inference rule
makeFilterProg :: Compiler ()
makeFilterProg = modify id

-- | Make a checking program of inference rule
makeCheckProg :: Compiler ()
makeCheckProg = modify id

replaceTerm :: Term            -- ^ theorem's term
            -> [Int]
            -> Compiler (Term, [Int]) -- ^ information structure

-- Case of theorem's variable
replaceTerm (Var x) s = do
  mb_n <- getInfoUnit x
  let n = fromJust mb_n
  state $ \info -> ((P.var n, n:s), info)

-- Case of constant
replaceTerm t@(Const _) s = state $ \info -> ((t,s), info)

-- General case
replaceTerm t s = do
  x <- forM (operands t) (`replaceTerm` s)
  let (ts,vs) = unzip x
  state $ \info -> ((Term.header t :> ts, concat vs), info)

-- | Make a replacing program of inference rule
makeReplaceProg :: Compiler ()
makeReplaceProg = do
  t <- getInfoUnit To
  (pt,vs) <- replaceTerm (fromJust t) []
  addProgChunk (Action (sort $ uniq vs) (Replacing :> [pt]))

linkProgChunks :: [Program] -> Compiler Program

linkProgChunks (Assign vl v g c Empty : Branch _ bc Empty Empty : ps) =
  linkProgChunks (Assign vl v g (P.and c bc) Empty : ps)

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
linkProgram :: Compiler (Program, LSymbol)
linkProgram = do
  chunks <- getProgChunks
  prog <- linkProgChunks chunks
  mb_sym <- getInfoUnit BindSymbol
  let sym = fromJust mb_sym
  state $ \info -> ((prog, sym), info)

-- | Make a program of inference rule
make :: Rule -> Compiler (Program, LSymbol) -> (Program, LSymbol)
make rule = (`evalState` initInfo rule)

-- | Compile an inference rule to program
compile :: Rule -> Global Program
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
