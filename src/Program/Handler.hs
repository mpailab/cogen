{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleInstances         #-}
{-# LANGUAGE InstanceSigs              #-}
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
      Program.Handler.Base,
      eval,
      getState,
      initState,
      run,
      setState,
      Program.Handler.State(..)
    )
where

-- External imports
import           Control.Applicative
import           Control.Monad.State
import           Data.Foldable
import qualified Data.Map            as M

-- Internal imports
import           DebugInfo
import           Expr
import           LSymbol
import           Program
import           Program.BuiltIn
import           Term

------------------------------------------------------------------------------------------
-- Data types and clases declaration

type Subst m d = Expr d -> Handler d m ()

newtype State m d = State
  {
    vals :: M.Map Var (Expr d, Subst m d)
  }

-- | Base handler class
class (Monad m, DebugInfo d) => Base m d where
  getState :: m (Program.Handler.State m d)
  setState :: Program.Handler.State m d -> m ()

-- | Init the handler state
initState :: (Monad m, DebugInfo d) => Program.Handler.State m d
initState = Program.Handler.State M.empty

class DebugInfo d => Handle d a where
  eval :: Expr d -> a
  run  :: Command d -> a

instance (DebugInfo d, Program.Handler.Base m d) => Handle d (m (Expr d)) where
  eval :: Expr d -> m (Expr d)
  eval e = getState >>= runHandler (evalI e) >>= \ ~(a,s) -> case a of
    Value x   -> setState s >> return x
    Error err -> error $ "Handler error: " ++ err ++ ".\n"

  run :: Command d -> m (Expr d)
  run cmd = getState >>= runHandler (runI [cmd] (return NONE)) >>= \ ~(a,s) -> case a of
    Value x   -> setState s >> return x
    Error err -> error $ "Handler error: " ++ err ++ ".\n"

instance Handle d a => Handle d (Expr d -> a) where
  eval :: Expr d -> Expr d -> a
  eval (Fun (x:xs) cmds) e = let cmd = Assign Simple x e (Bool True)
                             in eval (Fun xs (cmd:cmds))

  run :: Command d -> Expr d -> a
  run cmd = error "Handler error: can't run a command with arguments.\n"

data Value a
  = Value { getValue :: a }
  | Error { getError :: String }

instance Functor Value where
  fmap f (Value a)   = Value (f a)
  fmap f (Error err) = Error err

newtype Handler d m a = Handler
  {
    runHandler :: DebugInfo d => Program.Handler.State m d ->
                                 m (Value a, Program.Handler.State m d)
  }

instance Functor m => Functor (Handler d m) where
  fmap f (Handler m) = Handler $ \ s -> fmap (\ ~(a, s') -> (f <$> a, s')) (m s)
  {-# INLINE fmap #-}

instance Monad m => Applicative (Handler d m) where
  pure a = Handler $  \s -> return (Value a, s)
  {-# INLINE pure #-}

  Handler mf <*> Handler mx = Handler $ mf >=> \case
    (Value f, s') -> mx s' >>= \case
        (Value x, s'') -> return (Value (f x), s'')
        (Error err, s'') -> return (Error err, s'')
    (Error err, s') -> return (Error err, s')
  {-# INLINE (<*>) #-}

instance Monad m => Alternative (Handler d m) where
  empty = Handler $ \ s -> return (Error "an empty alternative", s)
  {-# INLINE empty #-}

  Handler m <|> Handler n = Handler $ \ s -> m s >>= \ case
    x@(Value _, _) -> return x
    _              -> n s
  {-# INLINE (<|>) #-}

instance Monad m => Monad (Handler d m) where
  m >>= k  = Handler $ \s -> runHandler (fmap k m) s >>= \case
    (Value n, s') -> runHandler n s'
    (Error err, s') -> return (Error err, s')
  {-# INLINE (>>=) #-}

  return a = Handler $ \ s -> return (Value a, s)
  {-# INLINE return #-}

  fail str = Handler $ \ s -> return (Error str, s)
  {-# INLINE fail #-}

instance Monad m => MonadPlus (Handler d m) where
  mzero = Handler $ \ s -> return (Error "an empty alternative", s)
  {-# INLINE mzero #-}

  Handler m `mplus` Handler n = Handler $ \ s -> m s >>= \ case
    x@(Value _, _) -> return x
    _              -> n s
  {-# INLINE mplus #-}

instance MonadTrans (Handler d) where
  lift m = Handler $ \ s -> m >>= \a -> return (Value a, s)
  {-# INLINE lift #-}

instance MonadIO m => MonadIO (Handler d m) where
  liftIO = lift . liftIO
  {-# INLINE liftIO #-}

instance Monad m => MonadState (Program.Handler.State m d) (Handler d m) where
  state f = Handler $ \ s -> let ~(a,s') = f s in return (Value a, s')
  get = state $ \ s -> (s, s)
  put s = state $ const ((), s)

------------------------------------------------------------------------------------------
-- Functions

getVal :: Monad m => Var -> Handler d m (Maybe (Expr d))
getVal x = get >>= \s -> return (fst <$> M.lookup x (vals s))

setVal :: Monad m => Var -> Expr d -> Handler d m (Expr d)
setVal x e = modify (\s -> s {vals = M.insert x (e, \_ -> return ()) (vals s)})
  >> return e

getPtr :: Monad m => Var -> Handler d m (Maybe (Expr d, Subst m d))
getPtr x = fmap (M.lookup x . vals) get

setPtr :: Monad m => Int -> Expr d -> Subst m d -> Handler d m (Expr d)
setPtr x e sw = modify (\s -> s { vals = M.insert x (e,sw) (vals s) }) >> return e

putError :: Monad m => forall a . String -> Handler d m a
putError str = Handler $ \ s -> return (Error str, s)


evalI :: (DebugInfo d, Monad m) => Expr d -> Handler d m (Expr d)

evalI (Var i) = getVal i >>= \case
  Just x  -> return x
  Nothing -> putError "a variable is not initialized"

evalI (Ptr _ _) = putError "can't take a pointer in the evaluating mode"

evalI (Ref i e) = evalI e >>= setVal i

evalI x@(Sym _) = return x

evalI x@(Int _) = return x

evalI Any = putError "can't evaluate any expression"

evalI AnySeq = putError "can't evaluate any sequence of expressions"

evalI x@(Bool _) = return x

evalI (Equal x y) = Bool <$> liftM2 (==) (evalI x) (evalI y)

evalI (NEqual x y) = Bool <$> liftM2 (/=) (evalI x) (evalI y)

evalI (In x y) = evalI x >>= \ex -> evalI y >>= \case
  (Alt   ey) -> return (Bool (ex `elem` ey))
  (Tuple ey) -> return (Bool (ex `elem` ey)) -- tuples are handled as lists
  (List  ey) -> return (Bool (ex `elem` ey))
  (Set   ey) -> return (Bool (ex `elem` ey)) -- sets are handled as lists
  _          -> putError "a wrong in-expression"

evalI (Not x) = Bool . Prelude.not <$> evalB x

evalI (And xs) = Bool . and <$> mapM evalB xs

evalI (Or xs) = Bool . or <$> mapM evalB xs

evalI (IfElse c t f) = evalB c >>= \case
  True  -> evalI t
  False -> evalI f

evalI (CaseOf e []) = putError "can't evaluate an empty case-expression"
evalI (CaseOf e ((p,c):cmds)) = (ident e p >> evalI c) <|> evalI (CaseOf e cmds)

evalI (Term (T x)) = Term . T <$> evalI x
evalI (Term (x :> xs)) = Term <$> liftM2 (:>) (evalI x)
  (fmap getTerm <$> mapM (evalI . Term) xs)
evalI (Term (x :>> y)) = Term <$> liftM2 (:>) (evalI x) (map getTerm . getList <$> evalI y)

evalI (Alt xs)   = Alt   <$> mapM evalI xs

evalI (Tuple xs) = Tuple <$> mapM evalI xs

evalI (List xs)  = List  <$> mapM evalI xs

evalI (Set xs)   = Set   <$> mapM evalI xs

evalI (Call f args) = evalCall f args

evalI x@(Fun [] cs) = evalCall x []
evalI (Fun _ _) = putError "can't evaluate a function definition"

evalI NONE = putError "can't evaluate an undefined expression"

evalB :: (DebugInfo d, Monad m) => Expr d -> Handler d m Bool
evalB e = evalI e >>= \case
  Bool c -> return c
  _      -> putError "a wrong Boolean expression"

evalL :: (DebugInfo d, Monad m) => Expr d -> Handler d m [Expr d]
evalL e = evalI e >>= \case
  List l -> return l
  _      -> putError "a wrong list expression"

evalCall :: (DebugInfo d, Monad m) => Expr d -> [Expr d] -> Handler d m (Expr d)
evalCall ff@(Sym (IL s)) as = case getBuiltInFunc s of
  Just (BuiltInFunc n f) | n > length as -> return $ Call ff as
                         | n == length as -> mapM evalI as >>= f
                         | otherwise -> mapM evalI af >>= f >>= \x -> return $ Call x al
                         where (af,al) = splitAt n as
  Nothing -> putError "call a nonexisting built-in function"
evalCall (Var i) as = getVal i >>= \case
  Just f  -> evalCall f as
  Nothing -> putError "can't call an undefined function"
evalCall (Fun [] cmds) [] = get >>= \s -> runI cmds (putError "the function value is undefined") >>= \e -> put s >> return e
evalCall (Fun (x:xs) cmds) (a:as) = let c = Assign Simple x a NONE
                                  in evalCall (Fun xs (c:cmds)) as
evalCall e as = putError "call an undefined function"

ident :: (DebugInfo d, Monad m) => Expr d -> Expr d -> Handler d m (Expr d)
ident x y = identI x (\_ -> return ()) y (\_ -> return ())

identI :: (DebugInfo d, Monad m) => Expr d -> Subst m d -> Expr d -> Subst m d -> Handler d m (Expr d)

identI (Alt []) lsw y rsw = empty
identI (Alt (x:xs)) lsw y rsw =
  identI x (\ a -> lsw $ Alt (a:xs)) y rsw <|>
  identI (Alt xs) (\ ~(Alt as) -> lsw $ Alt (x:as)) y rsw
identI x lsw (Alt ys) rsw = identI (Alt ys) rsw x lsw

identI Any lsw y rsw = return y
identI x lsw Any rsw = return x

identI (Var i) lsw y rsw = getVal i >>= \case
  Just x  -> identI x (void . setVal i) y rsw
  Nothing -> setVal i y
identI x lsw (Var i) rsw = identI (Var i) rsw x lsw

identI (Ptr i x) lsw y rsw = identI x (lsw . Ptr i) y rsw >>= \e -> setPtr i e rsw
identI x lsw (Ptr i y) rsw = identI (Ptr i y) rsw x lsw

identI (Ref i x) lsw y rsw = identI x (lsw . Ref i) y rsw >>= setVal i
identI x lsw (Ref i y) rsw = identI (Ref i y) rsw x lsw

identI (IfElse c t f) lsw y rsw = evalB c >>= \case
  True  -> identI t (\a -> lsw $ IfElse c a f) y rsw
  False -> identI f (lsw . IfElse c t) y rsw
identI x lsw (IfElse c t f) rsw = identI (IfElse c t f) rsw x lsw

identI (CaseOf e []) lsw y rsw = empty
identI (CaseOf e ((p,x):cs)) lsw y rsw =
  ( ident e p >> identI x (\a -> lsw $ CaseOf e ((p,a):cs)) y rsw ) <|>
  identI (CaseOf e cs) (\ ~(CaseOf a as) -> lsw $ CaseOf a ((p,x):as)) y rsw
identI x lsw (CaseOf e cs) rsw = identI (CaseOf e cs) rsw x lsw

identI x@(Call _ _) lsw y rsw = evalI x >>= \e -> ident e y
identI x lsw y@(Call _ _) rsw = identI y rsw x lsw

identI (Sym x) _ (Sym y) _ = if x == y
  then return (Sym x)
  else putError "logical symbols are not equal"

identI (Int x) _ (Int y) _ = if x == y
  then return (Int x)
  else putError "integers are not equal"

identI (Term (T x)) lsw (Term (T y)) rsw =
  Term . T <$> identI x (lsw . Term . T) y (rsw . Term . T)
identI (Term (T x)) lsw y@(Term _) rsw = identI x (lsw . Term . T) y rsw
identI x@(Term _) lsw (Term (T y)) rsw = identI x lsw y (lsw . Term . T)
identI (Term (x :> xs)) lsw (Term (y :> ys)) rsw =
  identI (List (x : map Term xs))
        (\ ~(List (a:as)) -> lsw $ Term (a :> map getTerm as))
        (List (y : map Term ys))
        (\ ~(List (a:as)) -> rsw $ Term (a :> map getTerm as)) >>=
  \ ~(List (e:es)) -> return (Term (e :> map getTerm es))

identI (Tuple xs) lsw (Tuple ys) rsw =
  Tuple <$> identLists xs (lsw . Tuple) ys (rsw . Tuple)

identI (List xs) lsw (List ys) rsw =
  List <$> identLists xs (lsw . List) ys (rsw . List)

identI (Set xs) lsw (Set ys) rsw =
  Set <$> identLists xs (lsw . Set) ys (rsw . Set)

identI _ _ _ _ = putError "an unsupported identification"

identLists :: (DebugInfo d, Monad m) => [Expr d] -> ([Expr d] -> Handler d m ()) ->
                         [Expr d] -> ([Expr d] -> Handler d m ()) -> Handler d m [Expr d]
identLists [] _ [] _ = return []
identLists [] _ _ _ = putError "can't identify lists with different lengths"
identLists _ _ [] _ = putError "can't identify lists with different lengths"
identLists (x:xs) lsw (y:ys) rsw =
  identI x (lsw . (:xs)) y (rsw . (:ys)) >>= \e ->
  identLists xs (lsw . (x:)) ys (rsw . (y:)) >>= \es -> return (e:es)

check :: (DebugInfo d, Monad m) => Expr d -> Handler d m ()
check e = evalB e >>= \case
  True  -> return ()
  False -> putError "a condition is false"

-- | Run a list of commands
runI :: (DebugInfo d, Monad m) => [Command d] -> Handler d m (Expr d) -> Handler d m (Expr d)

runI (Assign Simple l r c : cmds) _ =
  ident l r >>= \ e -> check c >> runI cmds (return e)

runI (Assign Iterate l r c : cmds) _ =
  evalI r >>= ident l >>= \ e -> check c >> runI cmds (return e)

{- Handle an assignment command of the form 'p <- g | c', where
 p is a program expression denoting a list of patterns;
 g is a program expression denoting a list of values;
 c is a program expression denoting a condition.

The program expressions p, g, c are evaluated as follows:
'evalI p' is a list of logical expressions in which some of leaves are program variables;
'evalI g' is a list of logical expressions;
'evalI c' is a logical constant true or false.

Note that we does not handle an assignment command of the form 'p = g | c', since it is equivalent to '[p] <- [g] | c'.

We handle a command 'p <- g | c' as follows. First, we evalI p as a list of expressions [p1,...,pn] and g as a list of logical expressions [g1,...,gm], and reduce 'p <- g | c' to '[p1,...,pn] <- [g1,...,gm] | c'. If n is less than or equal to m, we try to handle '[p1,...,pn] <- [g1,...,gm] | c' in the following steps:
1. if n > 0, match p1 with g1 and handle '[p2,...,pn] <- [g2,...,gm] | c';
2. if n = 0, evalI c;
3. handle '[p1,...,pn] <- [g2,...,gm] | c'.
-}
runI (Assign Select (List ls) r c : cmds) _ =
  evalL r >>= f ls >>= \ es ->
  check c >> runI cmds (return $ List es)
  where
    f :: (DebugInfo d, Monad m) => [Expr d] -> [Expr d] -> Handler d m [Expr d]
    f [] _ = empty
    f (x:xs) (y:ys) = do
      guard (length xs <= length ys)
      (ident x y >>= \ e -> (e:) <$> f xs ys) <|> f (x:xs) ys

{- Handle an assignment command of the form 'p ~= g | c', where
 p is a program expression denoting a list of patterns;
 g is a program expression denoting a list of values;
 c is a program expression denoting a condition.

The program expressions p, g, c are evaluated as follows:
'evalI p' is a list of logical expressions in which some of leaves are program variables;
'evalI g' is a list of logical expressions;
'evalI c' is a logical constant true or false;

We handle a command 'p ~= g | c' as follows. First, we evalI p as a list of expressions [p1,...,pn], g as a list of logical expressions [g1,...,gm], and reduce 'p ~= g | c' to '[p1,...,pn] ~= [g1,...,gm] | c'. If n is less than or equal to m+1 and there is at most one pi = p@__, where __ is a program sequence-variable, we try to handle '[p1,...,pn] ~= [g1,...,gm] | c' in the following steps:
1. if 'p1 != p@__', match p1 with g1 and handle '[p2,...,pn] ~= [g2,...,gm] | c';
2. if 'p1 = p@__' and n > 1, match p2 with g1 and handle '[p1,p3,...,pn] ~= [g2,...,gm] | c';
3. if 'p1 = p@__' and n = 1, assing p the list [g1,...,gm] and evalI c;
4. if n = 0, evalI c;
5. handle '[p1,...,pn] ~= [g2,...,gm,g1] | c'.
-}
runI (Assign Unord (List ls) r c : cmds) _ =
  evalL r >>= \ rs -> let n = length rs in
  guard (length ls <= n + 1) >> f ls rs n >>= \ es ->
  check c >> runI cmds (return $ List es)
  where
    f :: (DebugInfo d, Monad m) => [Expr d] -> [Expr d] -> Int -> Handler d m [Expr d]
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
if p is assigned, 'evalI p' is a list of logical expressions;
'evalI g' is a list of logical expressions;
'evalI c' is a logical constant true or false;

We handle a command 'p << g | c' as follows. First, we evalI p as a list of logical expressions [p1,...,pn], g as a list of logical expressions [g1,...,gm], and c as a logical constant C. If C is true, we assign p the new value [p1,...,pn,g1,...,gm].
-}
runI (Assign Append (Var i) r@(List rs) c : cmds) _ =
  check c >> getVal i >>= f >>= (setVal i . List) >>= runI cmds . return
  where
    f = \case (Just ~(List ls)) -> return (ls ++ rs); Nothing -> return rs

{- Handle a branch command of the form:
if c
do
  branch commands
next commands
-}
runI (Branch c b : cmds) res =
  -- evaluate the condition
  evalB c >>= \case
    True  -> runI b res   -- runI the branch commands if the condition holds
    False -> runI cmds res  -- runI the next commands otherwise

{- Handle a switch command of the form:
case e | c
p1 | c1
  commands of the first case
...
pn | cn
  commands of the lase case
next commands
-}
runI (Switch e c cs : cmds) _ =
  check c >> f cs >>= runI cmds . return
  where
    f ((xp,xc,xb):xs) = (ident xp e >>= \ x -> check xc >> runI xb (return x)) <|> f xs
    f [] = empty

-- Handle an acting command
runI (Apply a c : cmds) _ =
  check c >> evalI a >>= runI cmds . return

-- Handle an empty command
runI [] res = res
