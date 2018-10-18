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
import qualified Data.Map            as M

-- Internal imports
import           Expr
import           LSymbol
import           Program
import           Term
import           Program.BuiltIn

------------------------------------------------------------------------------------------
-- Data types and clases declaration

class Handle a where
  handle :: Expr -> a

instance Handle Expr where
  handle e = evalState (eval e) (Info M.empty)

instance Handle a => Handle (Expr -> a) where
  handle (Fun (x:xs) cmds) e = let cmd = Assign Simple x e NONE
                               in handle (Fun xs (cmd:cmds))

type Values = M.Map Var Expr

newtype Info = Info
  {
    vals :: Values
    -- swap :: SExpr
  }

type Handler m a = StateT Info m a

------------------------------------------------------------------------------------------
-- Functions

getVal :: Monad m => Var -> Handler m (Maybe Expr)
getVal x = get >>= \info -> return (M.lookup x (vals info))

setVal :: Monad m => Int -> Expr -> Handler m ()
setVal x v = modify (\info -> info {vals = M.insert x v (vals info)})

-- swapArg :: Monad m => Int -> Expr -> Handler m ()
-- swapArg x y = modify (\info -> info {args = M.insert x y (args info)})


eval :: Monad m => Expr -> Handler m Expr

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
  (Alt   ey) -> return (Bool (ex `elem` ey))
  (Tuple ey) -> return (Bool (ex `elem` ey)) -- tuples are handled as lists
  (List  ey) -> return (Bool (ex `elem` ey))
  (Set   ey) -> return (Bool (ex `elem` ey)) -- sets are handled as lists
  _          -> error "Evaluating error: a wrong in-expression.\n"

eval (Not x) = Bool . Prelude.not <$> evalB x

eval (And xs) = Bool . and <$> mapM evalB xs

eval (Or xs) = Bool . or <$> mapM evalB xs

eval (IfElse c t f) = evalB c >>= \case
  True  -> eval t
  False -> eval f

eval (CaseOf e []) = error "Evaluating error: can't evaluate an empty case-expression.\n"
eval (CaseOf e ((p,c):cs)) = ident e p >>= \case
  NONE      -> eval (CaseOf e cs)
  _ -> eval c

eval (Term (T x)) = Term . T <$> eval x
eval (Term (x :> xs)) = Term <$> liftM2 (:>) (eval x) (map (\(Term t) -> t) <$> mapM (eval . Term) xs)
eval (Term (x :>> xs)) = Term <$> liftM2 (:>) (eval x) ((\(List l) -> map (\(Term t) -> t) l) <$> eval xs)

eval (Alt xs)   = Alt   <$> mapM eval xs

eval (Tuple xs) = Tuple <$> mapM eval xs

eval (List xs)  = List  <$> mapM eval xs

eval (Set xs)   = Set   <$> mapM eval xs

eval (Call f args) = evalCall f args

eval (Fun _ _) = error "Evaluating error: can't evaluate a function definition.\n"

eval NONE = error "Evaluating error: can't evaluate an undefined expression.\n"

evalB :: Monad m => Expr -> Handler m Bool
evalB e = eval e >>= \case
  Bool c -> return c
  _      -> error "Evaluating error: a wrong Boolean expression.\n"

evalCall :: Monad m => Expr -> [Expr] -> Handler m Expr
evalCall ff@(Sym (IL s)) as = case getBuiltInFunc s of
  Just (BuiltInFunc n f)  -> if n > length as then return $ Call ff as
                             else if n == length as
                             then mapM eval as >>= f
                             else mapM eval af >>= f >>= \x -> return $ Call x al
                             where (af,al) = splitAt n as
  Nothing -> error "Evaluating error: call to nonexisting built-in function.\n"
evalCall (Var i) as = getVal i >>= \case
  Just f  -> evalCall f as
  Nothing -> error "Evaluating error: can't call an undefined function.\n"
evalCall (Fun [] cs) [] = run cs
evalCall (Fun (x:xs) cs) (a:as) = let c = Assign Simple x a NONE
                                     in evalCall (Fun xs (c:cs)) as
evalCall e as = return e  -- ^ it's very strange behaviour : 1` x = 1; maybe report an error in this case?


identAlt :: Monad m => (Expr -> Handler m Expr) -> [Expr] -> Handler m Expr
identAlt f [] = return NONE
identAlt f (x:xs) = f x >>= \case
  NONE -> identAlt f xs
  e    -> return e

ident :: Monad m => Expr -> Expr -> Handler m Expr

ident (Alt xs) y = identAlt (`ident`y) xs
ident x (Alt ys) = identAlt (ident x) ys

ident Any y = return y
ident x Any = return x

ident (Var i) y = getVal i >>= \case
  Just x  -> ident x y
  Nothing -> setVal i y >> return y

ident x (Var i) = getVal i >>= \case
  Just y  -> ident x y
  Nothing -> setVal i x >> return x

ident (Ptr i x) y = ident x y >>= \case
  NONE -> return NONE
  e    -> setVal i e >> return e

ident x (Ptr i y) = ident x y >>= \case
  NONE -> return NONE
  e    -> setVal i e >> return e

ident (Ref i x) y = ident x y >>= \case
  NONE -> return NONE
  e    -> setVal i e >> return e

ident x (Ref i y) = ident x y >>= \case
  NONE -> return NONE
  e    -> setVal i e >> return e

ident (IfElse c t f) y = eval c >>= \case
  Bool True  -> ident t y
  Bool False -> ident f y
  _  -> return NONE

ident x (IfElse c t f) = eval c >>= \case
  Bool True  -> ident x t
  Bool False -> ident x f
  _  -> return NONE

ident (CaseOf e []) y = return NONE
ident (CaseOf e ((p,x):cs)) y = ident e p >>= \case
  NONE      -> ident (CaseOf e cs) y
  _ -> ident x y

ident x (CaseOf e []) = return NONE
ident x (CaseOf e ((p,y):cs)) = ident e p >>= \case
  NONE      -> ident x (CaseOf e cs)
  _ -> ident x y

ident x@(Call _ _) y = eval x >>= \e -> ident e y
ident x y@(Call _ _) = eval y >>= \e -> ident x e

ident (Sym x) (Sym y) = if x == y then return (Sym x) else return NONE

ident (Int x) (Int y) = if x == y then return (Int x) else return NONE

ident (Term (T x)) (Term (T y)) = Term . T <$> ident x y
ident (Term (T x))   y@(Term _) = ident x y
ident   x@(Term _) (Term (T y)) = ident x y
ident (Term (x :> xs)) (Term (y :> ys)) =
  ident (List (x : map Term xs)) (List (y : map Term ys)) >>= \(List (e:es)) ->
  return (Term (e :> map (\(Term t) -> t) es))

ident (Tuple xs) (Tuple ys) = identLists xs ys >>= \case
  [NONE] -> return NONE
  es     -> return (Tuple es)

ident (List xs) (List ys) = identLists xs ys >>= \case
  [NONE] -> return NONE
  es     -> return (List es)

ident (Set xs) (Set ys) = identLists xs ys >>= \case
  [NONE] -> return NONE
  es     -> return (Set es)

ident _ _ = error "Identifying error: An unsupported identification.\n"

identLists :: Monad m => [Expr] -> [Expr] -> Handler m [Expr]
identLists [] [] = return []
identLists [] _  = error "Identifying error: can't identify lists with different lengths.\n"
identLists _  [] = error "Identifying error: can't identify lists with different lengths.\n"
identLists (x:xs) (y:ys) = ident x y >>= \case
  NONE -> return [NONE]
  e    -> identLists xs ys >>= \es -> return (e:es)

-- | Run a list of commands
run :: Monad m => [Command] -> Handler m Expr

run (Assign Simple l r c : cmds) = do
  state <- get
  ident l r >>= \case
    NONE -> put state >> return NONE
    e    -> evalB c >>= \case
      True  -> run cmds >>= \case {NONE -> return e; x -> return x}
      False -> put state >> return NONE

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
run (Assign Select l r c : cs) =
  case l of
    List ls -> case r of
      List rs   -> g ls rs
      _ -> eval r >>= \case
        (List rs) -> g ls rs
        _         -> error "Unsupported value in a Select-assignment command\n"
    _ -> error "Unsupported pattern in a Select-assignment command\n"
  where
    f :: Monad m => [Expr] -> [Expr] -> Handler m Expr
    f [] _ =
      -- check the condition
      evalB c >>= \case
        -- run the next commands
        True  -> run cs >>= \case {NONE -> return (List [NONE]); x -> return x}
        False -> return NONE

    f (x:xs) (y:ys) =
      if length xs <= length ys then do
        -- save current state of Handler
        state <- get
        -- identify the first pattern with the first value
        ident x y >>= \case
          -- restore current state of Handler and
          -- identify the list of patterns with the tail of values
          NONE -> put state >> f (x:xs) ys
          -- continue the identification of remaining patterns
          e    -> f xs ys >>= \case
            -- restore current state of Handler
            NONE -> put state >> return NONE
            List (NONE:es) -> return (List (NONE:e:es))
            x -> return x
      else
        return NONE

    g :: Monad m => [Expr] -> [Expr] -> Handler m Expr
    g xs ys = f xs ys >>= \case
      List (NONE:es) -> return (List es)
      x -> return x

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
run (Assign Unord l r c : cs) =
  case l of
    List ls -> case r of
      List rs   -> g ls rs
      _ -> eval r >>= \case
        (List rs) -> g ls rs
        _         -> error "Unsupported value in a Unord-assignment command\n"
    _ -> eval l >>= \case
      List ls -> case r of
        List rs   -> g ls rs
        _ -> eval r >>= \case
          (List rs) -> g ls rs
          _         -> error "Unsupported value in a Unord-assignment command\n"
      _ -> error "Unsupported pattern in a Unord-assignment command\n"
  where
    f :: Monad m => [Expr] -> [Expr] -> Int -> Handler m Expr
    f [] [] 0 =
      -- check the condition
      evalB c >>= \case
      -- run the next commands
        True  -> run cs >>= \case {NONE -> return (List [NONE]); x -> return x}
        False -> return NONE

    f [Ref i AnySeq] ys n =
      -- check the condition
      evalB c >>= \case
        True  -> do
          -- assign a program variable with the number i the list ys
          setVal i (List ys)
          -- run the next commands
          run cs >>= \case {NONE -> return (List [NONE]); x -> return x}
        False -> return NONE

    f (z@(Ref i AnySeq):x:xs) (y:ys) n = do
      -- save current state of Handler
      state <- get
      -- identify the first pattern with the first value
      ident x y >>= \case
        -- restore current state of Handler and
        -- identify the list of patterns with the shifted values
        NONE -> put state >> f (z:x:xs) (ys ++ [y]) (n-1)
        -- continue the identification of remaining patterns
        _ -> f (z:xs) ys (length ys) >>= \case
          -- restore current state of Handler
          NONE -> put state >> return NONE
          x -> return x

    f (x:xs) (y:ys) n = do
      -- save current state of Handler
      state <- get
      -- identify the first pattern with the first value
      ident x y >>= \case
        -- restore current state of Handler and
        -- identify the list of patterns with the shifted values
        NONE -> put state >> f (x:xs) (ys ++ [y]) (n-1)
        -- continue the identification of remaining patterns
        _ -> f xs ys (length ys) >>= \case
          -- restore current state of Handler
          NONE -> put state >> return NONE
          x -> return x

    f _ _ 0 = return NONE

    g :: Monad m => [Expr] -> [Expr] -> Handler m Expr
    g xs ys = do
      let n = length ys
      if length xs <= n + 1
        then f xs ys n >>= \case {List [NONE] -> return r; x -> return x}
        else return NONE

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
run (Assign Append (Var i) r@(List rs) c : cs) =
  -- check the condition
  evalB c >>= \case
    True  -> do
      mb_val <- getVal i
      val <- case mb_val of
        Just (List ls) -> return (List (ls ++ rs))
        Nothing        -> return r
      setVal i val
      -- run the next commands
      run cs >>= \case {NONE -> return val; x -> return x}
    False -> return NONE

-- Handle a branch command of the form:
-- if c
-- do
--   branch commands
-- next commands
run (Branch c b : cs) =
  -- evaluate the condition
  evalB c >>= \case
    True  -> run b   -- run the branch commands if the condition holds
    False -> run cs  -- run the next commands otherwise

-- Handle a switch command of the form:
-- case e | c
-- p1 | c1
--   commands of the first case
-- ...
-- pn | cn
--   commands of the lase case
-- next commands
run (Switch e c i : cs) =
  evalB c >>= \case
    True  -> f i    -- handle a case corresponing to the expression
    False -> run cs -- run the next commands
  where
    f [] = run cs
    f ((ip,ic,ib):is) = do
      -- save current state of Handler
      state <- get
      -- identify the first pattern with the logical expression
      ident ip e >>= \case
        -- restore current state of Handler and
        -- run the next case
        NONE -> put state >> f is
        -- check the condition and run the commands of the first case
        _ -> evalB ic >>= \case
          True  -> run ib
          -- restore current state of Handler and
          -- run the next case
          False -> put state >> f is

-- Handle an acting command
run (Apply a c : cs) =
  evalB c >>= \case
    True  -> eval a >>= \res -> run cs >>= \case {NONE -> return res; x -> return x}
    False -> run cs

-- Handle an empty command
run [] = return NONE
