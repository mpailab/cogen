{-# LANGUAGE ExistentialQuantification #-}
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
import           Control.Applicative
import qualified Data.Map            as M

-- Internal imports
import           LSymbol
import           Program
import           Term
import Expr

------------------------------------------------------------------------------------------
-- Data types and clases declaration

-- class Handler m a where
--   handle :: Expr -> a

-- instance Monad m => Handler m (m Expr) where
--   handle e = evalState (eval e) (Info M.empty)

-- instance (Monad m, Handler m a) => Handler m (Expr -> a) where
--   handle (Fun (x:xs) cmds) e = let cmd = Assign Simple x a NONE
--                                in handle (Fun xs (cmd:cmds))

type Args = M.Map Var Expr

data Info = Info
  {
    args :: Args
    -- swap :: SExpr
  }

type Handler m a = StateT Info m a
handle :: MonadPlus m => Expr -> m Expr
handle e = evalStateT (eval e) (Info M.empty)

------------------------------------------------------------------------------------------
-- Functions

getVal :: Monad m => Var -> Handler m (Maybe Expr)
getVal x = M.lookup x <$> args <$> get

setVal :: Monad m => Int -> Expr -> Handler m ()
setVal x v = modify (\info -> info {args = M.insert x v (args info)})

-- swapArg :: Monad m => Int -> Expr -> Handler m ()
-- swapArg x y = modify (\info -> info {args = M.insert x y (args info)})


eval :: MonadPlus m => Expr -> Handler m Expr

eval (Var i) = getVal i >>= \case
  Just x  -> return x
  Nothing -> error "Evaluating error: a program variable is not initialized.\n"

eval (Ptr _ _) = error "Evaluating error: can't take a pointer in the evaluating mode. Try take a reference x@(...).\n"

eval (Ref i e) = eval e >>= \x -> setVal i x >> return x

eval x@(Sym _) = return x

eval x@(Int _) = return x

eval Any = error "Evaluating error: can't evaluate any expression.\n"

eval AnySeq = error "Evaluating error: can't evaluate any sequence of expressions.\n"

eval x@(Bool _) = return x

eval (Equal x y) = Bool <$> liftM2 (==) (eval x) (eval y)

eval (NEqual x y) = Bool <$> liftM2 (/=) (eval x) (eval y)

eval (In x y) = eval x >>= \ex -> eval y >>= \case
  (Alt   ey) -> return $ Bool (elem ex ey)
  (Tuple ey) -> return $ Bool (elem ex ey) -- tuples are handled as lists
  (List  ey) -> return $ Bool (elem ex ey)
  (Set   ey) -> return $ Bool (elem ex ey) -- sets are handled as lists
  _          -> error "Evaluating error: a wrong in-expression.\n"

eval (Not x) = Bool <$> Prelude.not <$> evalB x

eval (And xs) = Bool . and <$> mapM evalB xs

eval (Or xs) = Bool . or <$> mapM evalB xs

eval (IfElse c t f) = evalB c >>= \case
  True  -> eval t
  False -> eval f

eval (CaseOf e []) = error "Evaluating error: can't evaluate an empty case-expression.\n"
eval (CaseOf e cs) = msum $ (\(p,c) -> ident e p >> eval c) <$> cs --p >>= \case
  -- True  -> eval c
  -- False -> eval (CaseOf e cs)

--eval (Term (T x)) = Term <$> T <$> eval x
--eval (Term (x :> xs)) = Term <$> liftM2 (:>) (eval x) (mapM eval xs)
eval (Term t) = Term <$> traverse eval t

eval (Alt xs)   = Alt   <$> mapM eval xs

eval (Tuple xs) = Tuple <$> mapM eval xs

eval (List xs)  = List  <$> mapM eval xs

eval (Set xs)   = Set   <$> mapM eval xs

evalB :: MonadPlus m => Expr -> Handler m Bool
evalB e = eval e >>= \case
  Bool c -> return c
  _      -> error "Evaluating error: a wrong Boolean expression.\n"

identB :: MonadPlus m => Expr -> Expr -> Handler m Bool
identB x y = optional (ident x y) >>= \case
  Just _  -> return True
  Nothing -> return False

ident1 :: MonadPlus m => Expr -> Expr -> Handler m ()
ident1 x y = optional (ident x y) >>= \case
  Just _  -> return ()
  Nothing -> mzero

ident :: MonadPlus m => Expr -> Expr -> Handler m ()

ident (Alt xs) y = msum $ (\x -> ident x y) <$> xs
ident x (Alt ys) = msum $ ident x <$> ys

ident (Var i) y = getVal i >>= \case
  Just x  -> {-??? setSwap (swapAssignment i) >> -} ident x y
  Nothing -> eval y >>= setVal i -- >> return True

ident x (Var i) = getVal i >>= \case
  Just y  -> {-setSwap (swapAssignment i) >>-} ident x y
  Nothing -> eval x >>= setVal i -- >> return True

ident (Ptr i (PSet (PSetter _ g))) y = do
  x <- g
  ident1 x y

  -- pointers not compiling
-- ident (Ptr i x) y = do
--   ident x y
--   e <- eval y
--   setVal i (PSet (PSetter { setp = setVal i, getp = getVal i }))
    -- e <- eval y
  --          getSwap >>= \sw ->
  --          setAssignment i (Swap e sw) >> return True


-- ident x (Ptr i y) = ident x y >>= \case
--   True  -> eval x >>= \e ->
--            getSwap >>= \sw ->
--            setAssignment i (Swap e sw) >> return True
--   False -> return False

ident (Ref i x) y = ident x y >> eval y >>= setVal i

ident x (Ref i y) = ident x y >> eval x >>= setVal i

ident (IfElse c t f) y = eval c >>= \case
  Bool True  -> ident t y
  Bool False -> ident f y
  otherwise  -> error "Boolean expression expected"

ident x (IfElse c t f) = eval c >>= \case
  Bool True  -> ident x t
  Bool False -> ident x f
  otherwise  -> error "Boolean expression expected"

ident (CaseOf e cs) y = msum $ (\(x,p) -> ident1 e p >> ident x y) <$> cs

ident x (CaseOf e cs) = msum $ (\(y,p) -> ident1 e p >> ident x y) <$> cs

-- ident (Swap x sw) y = setSwap sw >> ident x y -- ^ now there is no Swap constructor in Expr
-- ident x (Swap y sw) = setSwap sw >> ident x y

ident x@(Call n args) y = eval x >>= \e -> ident e y

ident x y@(Call n args) = eval y >>= ident x

ident (Sym x) y = eval y >>= \case
  Sym s -> guard (s == x)
  _     -> mzero

ident x (Sym y) = eval x >>= \case
  Sym s -> guard (s == y)
  _     -> mzero

ident (Int x) y = eval y >>= \case
  Int i -> guard (i == x)
  _     -> mzero

ident x (Int y) = eval x >>= \case
  Int i -> guard (i == y)
  _     -> mzero

ident Any _ = return ()
ident _ Any = return ()

ident (Term (T x)) (Term (T y)) = ident x y
ident (Term (T x)) y@(Term _) = eval x >>= \case
  Term t    -> ident (Term t) y
  otherwise -> mzero

ident x@(Term _) (Term (T y)) = eval y >>= \case
  Term t    -> ident x (Term t)
  otherwise -> mzero

ident (Term (x :> xs)) (Term (y :> ys)) = ident (List $ x:(Term <$> xs)) (List $ y:(Term <$> ys))

ident (Tuple xs) (Tuple ys) = identSeq xs ys

ident (List xs) (List ys) = identSeq xs ys

ident (Set xs) (Set ys) = identSeq xs ys

ident _ _ = error "Error: An unsupported identification.\n"

-- | identification of 2 sequences.
-- | not efficient implementation in enumerating case
identSeq :: MonadPlus m => [Expr] -> [Expr] -> Handler m ()
identSeq [] [] = return ()
identSeq (x:_) [] = mzero
identSeq [] (y:_) = mzero
identSeq (x:xs) (y:ys) = ident x y >> identSeq xs ys

--identSeq xs ys = zipWithM ident xs ys >>= foldrM (\z -> return . (z &&)) True
-- | identification with one of alternatives
--identAlt :: MonadPlus m => Expr -> [Expr] -> Handler m ()
--identAlt x ys  = mapM x ys >>= foldrM (\z -> return . (z ||)) False

-- | Run a list of commands
run :: MonadPlus m => [Command] -> Handler m Expr

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
    f :: MonadPlus m => [Expr] -> [Expr] -> Handler m Expr
    f [] _ = do
      -- check the condition
      evalB c >>= guard
      -- run the next commands if necessary
      run cs

    f (x:xs) (y:ys) = guard (length xs <= length ys) >> do
          -- save current state of Handler
          state <- get
          -- identify the first pattern with the first value
          is_match <- identB x y
          -- continue the identification of remaining patterns
          when is_match $ (f xs ys) >> return () --else return ()
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
      (List gs) -> guard (length ps <= length gs + 1) >> (f ps gs (length gs))
      _         -> error "Unsupported value in PMUnord assignment command")
    _         -> error "Unsupported pattern in PMUnord assignment command"
  where
    f :: MonadPlus m => [Expr] -> [Expr] -> Int -> Handler m Expr
    f [] [] 0 = do
      -- check the condition
      cond <- evalB c
      -- run the next commands if necessary
      guard cond >> run cs

    f _ _ 0 = return NONE

    f [Ref i AnySeq] ys n = do
      -- assign a program variable with the number i the list ys
      ident (Var i) (List ys)
      -- check the condition
      cond <- evalB c
      -- run the next commands if necessary
      guard cond >> run cs

    f (z@(Ref i AnySeq):x:xs) (y:ys) n = do
      -- save current state of Handler
      state <- get
      -- identify the first pattern with the first value
      is_match <- identB x y
      -- continue the identification of remaining patterns
      (guard is_match >> f (z:xs) ys (length ys)) <|> return NONE
      -- restore current state of Handler
      put state
      -- identify the list of patterns with the shifted values
      f (z:x:xs) (ys ++ [y]) (n-1)

    f (x:xs) (y:ys) n = do
      -- save current state of Handler
      state <- get
      -- identify the first pattern with the first value
      is_match <- identB x y
      -- continue the identification of remaining patterns
      (guard is_match >> f xs ys (length ys)) <|> return NONE
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
-- !!!!!!!! reimplement with functions
-- run ((Assign Append p g c) : cs) = do
--   guard <$> evalB c
--   case p of
--     Var v ->
--       getVal v >>= \case
--         Just _  -> liftM2 (++) (eval p) (eval g)
--         Nothing -> assign p <$> (eval g)
--     _ -> error "left side of `<<` must be a variable"
--   run cs

-- Handle a branch command of the form:
-- if c
-- do
--   branch commands
-- next commands
run ((Branch c b) : cs) = do
  state <- get         -- save current state of Handler
  cond <- evalB c      -- evaluate the condition
  (guard cond >> run b) <|> return NONE    -- run the branch commands if the condition holds
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
    f i               -- handle a case corresponing to the expression
  else
    run cs            -- run the next commands
  where
    f :: MonadPlus m => [(Expr,Expr,[Command])] -> Handler m Expr
    f [] = run cs
    f ((ip,ic,ib):is) = do
      -- save current state of Handler
      state <- get
      -- identify the first pattern with the logical expression
      is_match <- (eval e >>= identB ip)
      -- check the condition and run the commands of the first case
      (guard is_match >> {-saveAssignments *>-} evalB ic >>= guard >> run ib) <|> return NONE
      -- restore current state of Handler
      put state
      -- run the next case
      f is

-- Handle an acting command
-- make not defined
-- run ((Apply a c) : cs) = do
--   cond <- evalB c      -- run the condition
--   when cond (make a)   -- make the action if the condition holds
--   run cs               -- run the next commands

-- Handle an empty command
run [] = return Expr.NONE
