{-|
Module      : Compiler.Utils
Description : Representation of programs in compiler
Copyright   : (c) Grigoriy Bokov, 2017
License     : GPL-3
Maintainer  : bokov@intsys.msu.ru
Stability   : experimental
Portability : POSIX
-}
module Compiler.Utils
    (
      -- exports
    )
where

-- Internal imports


-- | Parse a term as the theorem of an inference rule
parseReplaceTheorem :: Term -> Bool -> Compiler ()
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
