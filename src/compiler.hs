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
  addInfoUnit Symbol (symbol rule)

-- | Prepare the theorem of inference rule
prepTheorem :: State Info ()
prepTheorem = parseTheorem >> modifyTheorem >> bindTheorem

-- | Prepare the filters of inference rule
prepFilters :: State Info ()
prepFilters = modify id

-- | Prepare the specifiers of inference rule
prepSpecifiers :: State Info ()
prepSpecifiers = modify id

-- | Make an identifying program of inference rule
makeIdentProg :: State Info ()
makeIdentProg = modify id

-- | Make a filtering program of inference rule
makeFilterProg :: State Info ()
makeFilterProg = modify id

-- | Make a checking program of inference rule
makeCheckProg :: State Info ()
makeCheckProg = modify id

-- | Make a replacing program of inference rule
makeReplaceProg :: State Info ()
makeReplaceProg = modify id

-- | Link program fragments of inference rule
linkProgram :: State Info (Program, LSymbol)
linkProgram = do
  progs <- getProgChunks
  let prog = case progs of
               (p:ps) -> p
               []     -> Empty
  mb_sym <- getInfoUnit Symbol
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
