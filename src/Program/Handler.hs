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
import           Control.Applicative
import           Control.Monad.State
import           Data.Foldable
import qualified Data.Map            as M

-- Internal imports
import           Expr
import           LSymbol
import           Program
import           Program.BuiltIn
import           Term

------------------------------------------------------------------------------------------
-- Data types and clases declaration

class Handle a where
  handle :: Expr -> a

instance Monad m => Handle (m Expr) where
  handle e = evalHandler (eval e) (Info M.empty)

instance Handle a => Handle (Expr -> a) where
  handle (Fun (x:xs) cmds) e = let cmd = Assign Simple x e NONE
                               in handle (Fun xs (cmd:cmds))

type Values = M.Map Var Expr

newtype Info = Info
  {
    vals :: Values
    -- swap :: SExpr
  }

data Eval a
  = Value { getValue :: a }
  | Error { getError :: String }

instance Functor Eval where
  fmap f (Value a)   = Value (f a)
  fmap f (Error err) = Error err

newtype Handler m a = Handler { runHandler :: Info -> m (Eval a, Info) }

evalHandler :: Monad m => Handler m a -> Info -> m a
evalHandler m s = runHandler m s >>= \ ~(a, _) -> case a of
  Value x   -> return x
  Error err -> error $ "Handler error: " ++ err ++ ".\n"
{-# INLINE evalHandler #-}

instance Functor m => Functor (Handler m) where
  fmap f (Handler m) = Handler $ \ s -> fmap (\ ~(a, s') -> (f <$> a, s')) (m s)
  {-# INLINE fmap #-}

instance Monad m => Applicative (Handler m) where
  pure a = Handler $  \s -> return (Value a, s)
  {-# INLINE pure #-}

  Handler mf <*> Handler mx = Handler $ mf >=> \case
    (Value f, s') -> mx s' >>= \case
        (Value x, s'') -> return (Value (f x), s'')
        (Error err, s'') -> return (Error err, s'')
    (Error err, s') -> return (Error err, s')
  {-# INLINE (<*>) #-}

instance Monad m => Alternative (Handler m) where
  empty = Handler $ \ s -> return (Error "an empty alternative", s)
  {-# INLINE empty #-}

  Handler m <|> Handler n = Handler $ \ s -> m s >>= \ case
    x@(Value _, _) -> return x
    _              -> n s
  {-# INLINE (<|>) #-}

instance Monad m => Monad (Handler m) where
  m >>= k  = Handler $ runHandler m >=> \ case
    (Value a, s') -> runHandler (k a) s'
    (Error err, s') -> return (Error err, s')
  {-# INLINE (>>=) #-}

  return a = Handler $ \ s -> return (Value a, s)
  {-# INLINE return #-}

  fail str = Handler $ \ s -> return (Error str, s)
  {-# INLINE fail #-}

instance Monad m => MonadPlus (Handler m) where
  mzero = Handler $ \ s -> return (Error "an empty alternative", s)
  {-# INLINE mzero #-}

  Handler m `mplus` Handler n = Handler $ \ s -> m s >>= \ case
    x@(Value _, _) -> return x
    _              -> n s
  {-# INLINE mplus #-}

instance MonadTrans Handler where
  lift m = Handler $ \ s -> m >>= \a -> return (Value a, s)
  {-# INLINE lift #-}

instance MonadIO m => MonadIO (Handler m) where
  liftIO = lift . liftIO
  {-# INLINE liftIO #-}

instance Monad m => MonadState Info (Handler m) where
  state f = Handler $ \ s -> let ~(a,s') = f s in return (Value a, s')
  get = state $ \ s -> (s, s)
  put s = state $ const ((), s)

------------------------------------------------------------------------------------------
-- Functions

getVal :: Monad m => Var -> Handler m (Maybe Expr)
getVal x = get >>= \info -> return (M.lookup x (vals info))

setVal :: Monad m => Int -> Expr -> Handler m Expr
setVal x v = modify (\info -> info {vals = M.insert x v (vals info)}) >> return v

putError :: Monad m => forall a . String -> Handler m a
putError str = Handler $ \ s -> return (Error str, s)

-- swapArg :: Monad m => Int -> Expr -> Handler m ()
-- swapArg x y = modify (\info -> info {args = M.insert x y (args info)})


eval :: Monad m => Expr -> Handler m Expr

eval (Var i) = getVal i >>= \case
  Just x  -> return x
  Nothing -> putError "a variable is not initialized"

eval (Ptr _ _) = putError "can't take a pointer in the evaluating mode"

eval (Ref i e) = eval e >>= setVal i

eval x@(Sym _) = return x

eval x@(Int _) = return x

eval Any = putError "can't evaluate any expression"

eval AnySeq = putError "can't evaluate any sequence of expressions"

eval x@(Bool _) = return x

eval (Equal x y) = Bool <$> liftM2 (==) (eval x) (eval y)

eval (NEqual x y) = Bool <$> liftM2 (/=) (eval x) (eval y)

eval (In x y) = eval x >>= \ex -> eval y >>= \case
  (Alt   ey) -> return (Bool (ex `elem` ey))
  (Tuple ey) -> return (Bool (ex `elem` ey)) -- tuples are handled as lists
  (List  ey) -> return (Bool (ex `elem` ey))
  (Set   ey) -> return (Bool (ex `elem` ey)) -- sets are handled as lists
  _          -> putError "a wrong in-expression"

eval (Not x) = Bool . Prelude.not <$> evalB x

eval (And xs) = Bool . and <$> mapM evalB xs

eval (Or xs) = Bool . or <$> mapM evalB xs

eval (IfElse c t f) = evalB c >>= \case
  True  -> eval t
  False -> eval f

eval (CaseOf e []) = putError "can't evaluate an empty case-expression"
eval (CaseOf e ((p,c):cmds)) = (ident e p >> eval c) <|> eval (CaseOf e cmds)

eval (Term (T x)) = Term . T <$> eval x
eval (Term (x :> xs)) = Term <$> liftM2 (:>) (eval x)
  (fmap getTerm <$> mapM (eval . Term) xs)
eval (Term (x :>> y)) = Term <$> liftM2 (:>) (eval x) (map getTerm . getList <$> eval y)

eval (Alt xs)   = Alt   <$> mapM eval xs

eval (Tuple xs) = Tuple <$> mapM eval xs

eval (List xs)  = List  <$> mapM eval xs

eval (Set xs)   = Set   <$> mapM eval xs

eval (Call f args) = evalCall f args

eval (Fun _ _) = putError "can't evaluate a function definition"

eval NONE = putError "can't evaluate an undefined expression"

evalB :: Monad m => Expr -> Handler m Bool
evalB e = eval e >>= \case
  Bool c -> return c
  _      -> putError "a wrong Boolean expression"

evalL :: Monad m => Expr -> Handler m [Expr]
evalL e = eval e >>= \case
  List l -> return l
  _      -> putError "a wrong list expression"

evalCall :: Monad m => Expr -> [Expr] -> Handler m Expr
evalCall ff@(Sym (IL s)) as = case getBuiltInFunc s of
  Just (BuiltInFunc n f) | n > length as -> return $ Call ff as
                         | n == length as -> mapM eval as >>= f
                         | otherwise -> mapM eval af >>= f >>= \x -> return $ Call x al
                         where (af,al) = splitAt n as
  Nothing -> putError "call a nonexisting built-in function"
evalCall (Var i) as = getVal i >>= \case
  Just f  -> evalCall f as
  Nothing -> putError "can't call an undefined function"
evalCall (Fun [] cmds) [] = run cmds (putError "the function value is undefined")
evalCall (Fun (x:xs) cmds) (a:as) = let c = Assign Simple x a NONE
                                  in evalCall (Fun xs (c:cmds)) as
evalCall e as = putError "call an undefined function"


ident :: Monad m => Expr -> Expr -> Handler m Expr

ident (Alt xs) y = foldr ((<|>) . (`ident`y)) empty xs
ident x (Alt ys) = foldr ((<|>) . ident x) empty ys

ident Any y = return y
ident x Any = return x

ident (Var i) y = getVal i >>= \case
  Just x  -> ident x y
  Nothing -> setVal i y

ident x (Var i) = getVal i >>= \case
  Just y  -> ident x y
  Nothing -> setVal i x

ident (Ptr i x) y = ident x y >>= setVal i

ident x (Ptr i y) = ident x y >>= setVal i

ident (Ref i x) y = ident x y >>= setVal i

ident x (Ref i y) = ident x y >>= setVal i

ident (IfElse c t f) y = evalB c >>= \case
  True  -> ident t y
  False -> ident f y

ident x (IfElse c t f) = evalB c >>= \case
  True  -> ident x t
  False -> ident x f

ident (CaseOf e []) y = empty
ident (CaseOf e ((p,x):cmds)) y = (ident e p >> ident x y) <|> ident (CaseOf e cmds) y

ident x (CaseOf e []) = empty
ident x (CaseOf e ((p,y):cmds)) = (ident e p >> ident x y) <|> ident x (CaseOf e cmds)

ident x@(Call _ _) y = eval x >>= \e -> ident e y
ident x y@(Call _ _) = eval y >>= \e -> ident x e

ident (Sym x) (Sym y) = if x == y
  then return (Sym x)
  else putError "logical symbols are not equal"

ident (Int x) (Int y) = if x == y
  then return (Int x)
  else putError "integers are not equal"

ident (Term (T x)) (Term (T y)) = Term . T <$> ident x y
ident (Term (T x))   y@(Term _) = ident x y
ident   x@(Term _) (Term (T y)) = ident x y
ident (Term (x :> xs)) (Term (y :> ys)) =
  ident (List (x : map Term xs)) (List (y : map Term ys)) >>=
  \ ~(List (e:es)) -> return (Term (e :> map getTerm es))

ident (Tuple xs) (Tuple ys) = Tuple <$> identLists xs ys

ident (List xs) (List ys) = List <$> identLists xs ys

ident (Set xs) (Set ys) = Set <$> identLists xs ys

ident _ _ = putError "an unsupported identification"

identLists :: Monad m => [Expr] -> [Expr] -> Handler m [Expr]
identLists [] [] = return []
identLists [] _  = putError "can't identify lists with different lengths"
identLists _  [] = putError "can't identify lists with different lengths"
identLists (x:xs) (y:ys) = ident x y >>= \e -> identLists xs ys >>= \es -> return (e:es)

check :: Monad m => Expr -> Handler m ()
check e = evalB e >>= \case
  True  -> return ()
  False -> putError "a condition is false"

-- | Run a list of commands
run :: Monad m => [Command] -> Handler m Expr -> Handler m Expr

run (Assign Simple l r c : cmds) _ =
  ident l r >>= \ e -> check c >> run cmds (return e)

run (Assign Iterate l r c : cmds) _ =
  eval r >>= ident l >>= \ e -> check c >> run cmds (return e)

{- Handle an assignment command of the form 'p <- g | c', where
 p is a program expression denoting a list of patterns;
 g is a program expression denoting a list of values;
 c is a program expression denoting a condition.

The program expressions p, g, c are evaluated as follows:
'eval p' is a list of logical expressions in which some of leaves are program variables;
'eval g' is a list of logical expressions;
'eval c' is a logical constant true or false.

Note that we does not handle an assignment command of the form 'p = g | c', since it is equivalent to '[p] <- [g] | c'.

We handle a command 'p <- g | c' as follows. First, we eval p as a list of expressions [p1,...,pn] and g as a list of logical expressions [g1,...,gm], and reduce 'p <- g | c' to '[p1,...,pn] <- [g1,...,gm] | c'. If n is less than or equal to m, we try to handle '[p1,...,pn] <- [g1,...,gm] | c' in the following steps:
1. if n > 0, match p1 with g1 and handle '[p2,...,pn] <- [g2,...,gm] | c';
2. if n = 0, eval c;
3. handle '[p1,...,pn] <- [g2,...,gm] | c'.
-}
run (Assign Select (List ls) r c : cmds) _ =
  evalL r >>= f ls >>= \ es ->
  check c >> run cmds (return $ List es)
  where
    f :: Monad m => [Expr] -> [Expr] -> Handler m [Expr]
    f [] _ = empty
    f (x:xs) (y:ys) = do
      guard (length xs <= length ys)
      (ident x y >>= \ e -> (e:) <$> f xs ys) <|> f (x:xs) ys

{- Handle an assignment command of the form 'p ~= g | c', where
 p is a program expression denoting a list of patterns;
 g is a program expression denoting a list of values;
 c is a program expression denoting a condition.

The program expressions p, g, c are evaluated as follows:
'eval p' is a list of logical expressions in which some of leaves are program variables;
'eval g' is a list of logical expressions;
'eval c' is a logical constant true or false;

We handle a command 'p ~= g | c' as follows. First, we eval p as a list of expressions [p1,...,pn], g as a list of logical expressions [g1,...,gm], and reduce 'p ~= g | c' to '[p1,...,pn] ~= [g1,...,gm] | c'. If n is less than or equal to m+1 and there is at most one pi = p@__, where __ is a program sequence-variable, we try to handle '[p1,...,pn] ~= [g1,...,gm] | c' in the following steps:
1. if 'p1 != p@__', match p1 with g1 and handle '[p2,...,pn] ~= [g2,...,gm] | c';
2. if 'p1 = p@__' and n > 1, match p2 with g1 and handle '[p1,p3,...,pn] ~= [g2,...,gm] | c';
3. if 'p1 = p@__' and n = 1, assing p the list [g1,...,gm] and eval c;
4. if n = 0, eval c;
5. handle '[p1,...,pn] ~= [g2,...,gm,g1] | c'.
-}
run (Assign Unord (List ls) r c : cmds) _ =
  evalL r >>= \ rs -> let n = length rs in
  guard (length ls <= n + 1) >> f ls rs n >>= \ es ->
  check c >> run cmds (return $ List es)
  where
    f :: Monad m => [Expr] -> [Expr] -> Int -> Handler m [Expr]
    f [] _ n = empty
    f [Ref i AnySeq] ys n = setVal i (List ys) >>= \e -> return  [e]
    f (z@(Ref i AnySeq):xs) ys n = f (xs ++ [z]) ys n
    f (x:xs) (y:ys) n = guard (length xs <= length ys) >>
      ( (ident x y >>= \ e -> (e:) <$> f xs ys (length ys))
        <|> f (x:xs) (ys ++ [y]) (n-1) )

{-Handle an assignment command of the form 'p << g | c', where
 p is a program variable whose values are lists of logical expressions;
 g is a program expression denoting a list of values;
 c is a program expression denoting a condition.

The program expressions p, g, c are evaluated as follows:
if p is assigned, 'eval p' is a list of logical expressions;
'eval g' is a list of logical expressions;
'eval c' is a logical constant true or false;

We handle a command 'p << g | c' as follows. First, we eval p as a list of logical expressions [p1,...,pn], g as a list of logical expressions [g1,...,gm], and c as a logical constant C. If C is true, we assign p the new value [p1,...,pn,g1,...,gm].
-}
run (Assign Append (Var i) r@(List rs) c : cmds) _ =
  check c >> getVal i >>= f >>= (setVal i . List) >>= run cmds . return
  where
    f = \case (Just ~(List ls)) -> return (ls ++ rs); Nothing -> return rs

{- Handle a branch command of the form:
if c
do
  branch commands
next commands
-}
run (Branch c b : cmds) res =
  -- evaluate the condition
  evalB c >>= \case
    True  -> run b res   -- run the branch commands if the condition holds
    False -> run cmds res  -- run the next commands otherwise

{- Handle a switch command of the form:
case e | c
p1 | c1
  commands of the first case
...
pn | cn
  commands of the lase case
next commands
-}
run (Switch e c cs : cmds) _ =
  check c >> f cs >>= run cmds . return
  where
    f ((xp,xc,xb):xs) = (ident xp e >>= \ x -> check xc >> run xb (return x)) <|> f xs
    f [] = empty

-- Handle an acting command
run (Apply a c : cmds) _ =
  check c >> eval a >>= run cmds . return

-- Handle an empty command
run [] res = res
