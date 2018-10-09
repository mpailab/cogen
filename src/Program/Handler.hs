c{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleInstances         #-}
{-# LANGUAGE LambdaCase                #-}
{-# LANGUAGE MultiParamTypeClasses     #-}
{-# LANGUAGE Rank2Types                #-}
{-# LANGUAGE TypeSynonymInstances      #-}

{-|
Module      : Program.Handler
Description : Programs interpreter
Copyright   : (c) Grigoriy Bokov, 2018
License     : GPL-3
Maintainer  : bokov@intsys.msu.ru
Stability   : experimental
Portability : POSIX
-}
module Program.Handler
    (
      -- exports
      handle
    )
where

-- External imports
import           Control.Monad.State
import           Data.Foldable
import qualified Data.Map            as M

-- Internal imports
import           LSymbol
import           Program
import           Term
import Expr

------------------------------------------------------------------------------------------
-- Data types and clases declaration

data Info = Info
  {
    args :: Args,
    swap :: SExpr
  }

-- type Handler m a = StateT Info m (Args -> (a, Args))
type Handler m a = StateT Info m a

-- handle :: (Monad m, Function a) => Program -> m a
-- handle (Program (Header name args) cs) =
--   let f = execState (run cs) (Info (M.fromList $ zip args args) M.empty)
--   in return (apply f [] args)

handle :: (Monad m, Function m a b) => Program -> b
handle (Program (Header name vars) cs) =
  apply (\x -> execState (run cs) (Info x M.empty return)) [] vars

------------------------------------------------------------------------------------------
-- Functions

getArg :: Monad m => Var -> Handler m (Maybe Expr)
getArg x = get >>= M.lookup x args

setVal :: Monad m => Int -> Expr -> Handler m ()
setVal x v = modify (\info -> info {args = M.insert x v (args info)})

-- swapArg :: Monad m => Int -> Expr -> Handler m ()
-- swapArg x y = modify (\info -> info {args = M.insert x y (args info)})


eval :: Monad m => Expr -> Handler m Expr

eval (Var i) = getArg i >>= \case
  Just x  -> return x
  Nothing -> error "Evaluating error: a program variable is not initialized.\n"

eval (Ptr _ _) = error "Evaluating error: can't take a pointer in the evaluating mode. Try take a reference x@(...).\n"

eval (Ref i e) = eval e >>= \x -> setArg i >> return x

eval x@(Sym _) = return x

eval x@(Int _) = return x

eval Any = error "Evaluating error: can't evaluate any expression.\n"

eval AnySeq = error "Evaluating error: can't evaluate any sequence of expressions.\n"

eval x@(Const _) = return x

eval (Equal x y) = Const <$> liftM2 (==) (eval x) (eval y)

eval (NEqual x y) = Const <$> liftM2 (!=) (eval x) (eval y)

eval (In x y) = eval x >>= \ex -> eval y >>= \case
  (Alt   ey) -> return (elem ex ey)
  (Tuple ey) -> return (elem ex ey) -- tuples are handled as lists
  (List  ey) -> return (elem ex ey)
  (Set   ey) -> return (elem ex ey) -- sets are handled as lists
  _          -> error "Evaluating error: a wrong in-expression.\n"

eval (Not x) = Bool <$> Prelude.not <$> (evalB x)

eval (And xs) = Bool <$> (foldrM (\y -> return . (y &&)) True) <$> mapM evalB xs

eval (Or xs) = Bool <$> (foldrM (\y -> return . (y ||)) False) <$> mapM evalB xs

eval (IfElse c t f) = evalB c >>= \case
  True  -> eval t
  False -> eval f

eval (CaseOf e []) = error "Evaluating error: can't evaluate an empty case-expression.\n"
eval (CaseOf e ((p,c):cs)) = ident e p >>= \case
  True  -> eval c
  False -> eval (CaseOf e cs)

eval (Term (T x)) = Term <$> T <$> eval x
eval (Term (x :> xs)) = Term <$> liftM2 (:>) (eval x) (mapM eval xs)

eval (Alt xs)   = Alt   <$> mapM eval xs

eval (Tuple xs) = Tuple <$> mapM eval xs

eval (List xs)  = List  <$> mapM eval xs

eval (Set xs)   = Set   <$> mapM eval xs

evalB :: Monad m => Expr -> Handler m Bool
evalB e = eval e >>= \case
  Bool c -> return c
  _      -> error "Evaluating error: a wrong Boolean expression.\n"

ident :: Monad m => Expr -> Expr -> Handler m Bool

ident (Alt xs) y = f (\x -> ident x y) xs
ident x (Alt ys) = f (ident x) ys

ident (Var i) y = getArg i >>= \case
  Just x  -> setSwap (swapAssignment i) >> ident x y
  Nothing -> eval y >>= setAssignment i >> return True

ident x (Var i) = getAssignment i >>= \case
  Just y  -> setSwap (swapAssignment i) >> ident x y
  Nothing -> eval x >>= setAssignment i >> return True

ident (Ptr i x) y = ident x y >>= \case
  True  -> eval y >>= \e ->
           getSwap >>= \sw ->
           setAssignment i (Swap e sw) >> return True
  False -> return False

ident x (Ptr i y) = ident x y >>= \case
  True  -> eval x >>= \e ->
           getSwap >>= \sw ->
           setAssignment i (Swap e sw) >> return True
  False -> return False

ident (Ref i x) y = ident x y >>= \case
  True  -> eval y >>= setAssignment i >> return True
  False -> return False

ident x (Ref i y) = ident x y >>= \case
  True  -> eval x >>= setAssignment i >> return True
  False -> return False

ident (IfElse c t f) y = eval c >>= \case
  Bool True  -> ident t y
  Bool False -> ident f y
  otherwise  -> return False

ident x (IfElse c t f) = eval c >>= \case
  Bool True  -> ident x t
  Bool False -> ident x f
  otherwise  -> return False

ident (CaseOf e []) y = return False
ident (CaseOf e ((p,x):cs)) y = ident e p >>= \case
  True  -> ident x y
  False -> ident (CaseOf e cs) y

ident x (CaseOf e []) = return False
ident x (CaseOf e ((p,y):cs)) = ident e p >>= \case
  True  -> ident x y
  False -> ident x (CaseOf e cs)

ident (Swap x sw) y = setSwap sw >> ident x y
ident x (Swap y sw) = setSwap sw >> ident x y

ident (Call n args) y = getFun n >>= \f -> f <$> mapM eval args >>= \e -> ident e y
ident x (Call n args) = getFun n >>= \f -> f <$> mapM eval args >>= \e -> ident x e

ident (Sym x) y = eval y >>= \case
  Sym s -> return (s == x)
  _     -> return False

ident x (Sym y) = eval x >>= \case
  Sym s -> return (s == y)
  _     -> return False

ident (Int x) y = eval y >>= \case
  Int i -> return (i == x)
  _     -> return False

ident x (Int y) = eval x >>= \case
  Int i -> return (i == y)
  _     -> return False

ident Any _ = return True
ident _ Any = return True

ident (Term (T x)) (Term (T y)) = ident x y
ident (Term (T x)) (Term y) = eval x >>= \case
  Term t    -> ident t y
  otherwise -> return False
ident (Term x) (Term (T y)) = eval y >>= \case
  Term t    -> ident x t
  otherwise -> return False
ident (Term (x :> xs)) (Term (y :> ys)) = ident (List (x:xs)) (List (y:ys))

ident (Tuple xs) (Tuple ys) = g xs ys

ident (List xs) (List ys) = g xs ys

ident (Set xs) (Set ys) = g xs ys

ident _ _ = error "Error: An unsupported identification.\n"

where
  f x ys  = mapM x ys >>= foldrM (\z -> return . (z ||)) False
  g xs ys = zipWithM ident xs ys >>= foldrM (\z -> return . (z &&)) True

-- | Run a list of commands
run :: Monad m => [Command] -> Handler m LExpr

-- Handle an assignment command of the form 'p <- g | c', where
--  p is a program expression denoting a list of patterns;
--  g is a program expression denoting a list of values;
--  c is a program expression denoting a condition.
--
-- The program expressions p, g, c are evaluated as follows:
-- 'eval p' is a list of logical expressions in which some of leaves are program variables;
-- 'eval g' is a list of logical expressions;
-- 'eval c' is a logical constant true or false.
--
-- Note that we does not handle an assignment command of the form 'p = g | c', since it is equivalent to '[p] <- [g] | c'.
--
-- We handle a command 'p <- g | c' as follows. First, we eval p as a list of expressions [p1,...,pn] and g as a list of logical expressions [g1,...,gm], and reduce 'p <- g | c' to '[p1,...,pn] <- [g1,...,gm] | c'. If n is less than or equal to m, we try to handle '[p1,...,pn] <- [g1,...,gm] | c' in the following steps:
-- 1. if n > 0, match p1 with g1 and handle '[p2,...,pn] <- [g2,...,gm] | c';
-- 2. if n = 0, eval c;
-- 3. handle '[p1,...,pn] <- [g2,...,gm] | c'.
--
run ((Assign Select p g c) : cs) =
  case p of
    List ps -> case g of
      List gs   -> f ps gs
      otherwise -> eval g >>= \case
        (List gs) -> f ps gs
        _         -> error "Unsupported value in a Select-assignment command\n"
    otherwise -> error "Unsupported pattern in a Select-assignment command\n"
  where
    f :: [Expr] -> [Expr] -> Handler m Expr
    f [] _ = do
      -- check the condition
      cond <- evalB c
      -- run the next commands if necessary
      when cond (run cs)

    f (x:xs) (y:ys) = when (length ps <= length gs) do
      -- save current state of Handler
      state <- get
      -- identify the first pattern with the first value
      is_match <- ident x y
      -- continue the identification of remaining patterns
      when is_match (saveAssignments *> f xs ys)
      -- restore current state of Handler
      put state
      -- identify the list of patterns with the tail of values
      f (x:xs) ys

-- Handle an assignment command of the form 'p ~= g | c', where
--  p is a program expression denoting a list of patterns;
--  g is a program expression denoting a list of values;
--  c is a program expression denoting a condition.
--
-- The program expressions p, g, c are evaluated as follows:
-- 'eval p' is a list of logical expressions in which some of leaves are program variables;
-- 'eval g' is a list of logical expressions;
-- 'eval c' is a logical constant true or false;
--
-- We handle a command 'p ~= g | c' as follows. First, we eval p as a list of expressions [p1,...,pn], g as a list of logical expressions [g1,...,gm], and reduce 'p ~= g | c' to '[p1,...,pn] ~= [g1,...,gm] | c'. If n is less than or equal to m+1 and there is at most one pi = p@__, where __ is a program sequence-variable, we try to handle '[p1,...,pn] ~= [g1,...,gm] | c' in the following steps:
-- 1. if 'p1 != p@__', match p1 with g1 and handle '[p2,...,pn] ~= [g2,...,gm] | c';
-- 2. if 'p1 = p@__' and n > 1, match p2 with g1 and handle '[p1,p3,...,pn] ~= [g2,...,gm] | c';
-- 3. if 'p1 = p@__' and n = 1, assing p the list [g1,...,gm] and eval c;
-- 4. if n = 0, eval c;
-- 5. handle '[p1,...,pn] ~= [g2,...,gm,g1] | c'.
--
run ((Assign Unord p g c) : cs) =
  eval p >>= \case
    (List ps) -> (eval g >>= \case
      (List gs) -> when (length ps <= length gs + 1) (f ps gs (length gs))
      _         -> error "Unsupported value in PMUnord assignment command")
    _         -> error "Unsupported pattern in PMUnord assignment command"
  where
    f [] [] 0 = do
      -- check the condition
      cond <- evalB c
      -- run the next commands if necessary
      when cond (run cs)

    f _ _ 0 = return

    f [Ref i AnySeq] ys n = do
      -- assign a program variable with the number i the list ys
      ident (Var i) (List ys)
      -- check the condition
      cond <- evalB c
      -- run the next commands if necessary
      when cond (run cs)

    f (z@(Ref i AnySeq):x:xs) (y:ys) n = do
      -- save current state of Handler
      state <- get
      -- identify the first pattern with the first value
      is_match <- ident x y
      -- continue the identification of remaining patterns
      when is_match (saveAssignments *> f (z:xs) ys (length ys))
      -- restore current state of Handler
      put state
      -- identify the list of patterns with the shifted values
      f (z:x:xs) (ys ++ [y]) (n-1)

    f (x:xs) (y:ys) n = do
      -- save current state of Handler
      state <- get
      -- identify the first pattern with the first value
      is_match <- ident x y
      -- continue the identification of remaining patterns
      when is_match (saveAssignments *> f xs ys (length ys))
      -- restore current state of Handler
      put state
      -- identify the list of patterns with the shifted values
      f (x:xs) (ys ++ [y]) (n-1)

-- Handle an assignment command of the form 'p << g | c', where
--  p is a program variable whose values are lists of logical expressions;
--  g is a program expression denoting a list of values;
--  c is a program expression denoting a condition.
--
-- The program expressions p, g, c are evaluated as follows:
-- if p is assigned, 'eval p' is a list of logical expressions;
-- 'eval g' is a list of logical expressions;
-- 'eval c' is a logical constant true or false;
--
-- We handle a command 'p << g | c' as follows. First, we eval p as a list of logical expressions [p1,...,pn], g as a list of logical expressions [g1,...,gm], and c as a logical constant C. If C is true, we assign p the new value [p1,...,pn,g1,...,gm].
--
run ((Assign Append p g c) : cs) = do
  cond <- evalB c
  when cond (isAssigned p >>= \case
    True  -> liftM2 (++) (eval p) (eval g)
    False -> assign p <$> (eval g))
  run cs

-- Handle a branch command of the form:
-- if c
-- do
--   branch commands
-- next commands
run ((Branch c b) : cs) = do
  state <- get         -- save current state of Handler
  cond <- evalB c      -- evaluate the condition
  when cond (run b)    -- run the branch commands if the condition holds
  put state            -- restore current state of Handler
  run cs               -- run the next commands

-- Handle a switch command of the form:
-- case e | c
-- p1 | c1
--   commands of the first case
-- ...
-- pn | cn
--   commands of the lase case
-- next commands
run ((Switch e c i) : cs) = do
  cond <- evalB c
  if cond
  then do
    expr <- eval e
    f i               -- handle a case corresponing to the expression
  else
    run cs            -- run the next commands
  where
    f [] = run cs
    f ((ip,ic,ib):is) = do
      -- save current state of Handler
      state <- get
      -- identify the first pattern with the logical expression
      is_match <- ident ip expr
      -- check the condition and run the commands of the first case
      when is_match (saveAssignments *> evalB ic >>= \cond -> when cond (run ib))
      -- restore current state of Handler
      put state
      -- run the next case
      f is

-- Handle an acting command
run ((Action a c) : cs) = do
  cond <- evalB c      -- run the condition
  when cond (make a)   -- make the action if the condition holds
  run cs               -- run the next commands

-- Handle an empty command
run Empty = return Expr.NONE
