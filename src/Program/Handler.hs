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
      Base,
      eval,
      getState,
      initState,
      run,
      setState,
      State(..)
    )
where

-- External imports
import           Control.Applicative
import           Control.Monad.State hiding (State)
import           Data.Foldable
import qualified Data.Map            as M

-- Internal imports
import           DebugInfo
import           Expr
import           LSymbol             hiding (Base)
import           Program             hiding (Base)
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
  getState :: m (State m d)
  setState :: State m d -> m ()

-- | Init the handler state
initState :: (Monad m, DebugInfo d) => State m d
initState = State M.empty

class DebugInfo d => Handle d a where
  eval :: Expr d -> a
  run  :: [Command d] -> a

instance (DebugInfo d, Base m d) => Handle d (m (Expr d)) where
  eval :: Expr d -> m (Expr d)
  eval e = getState >>= runHandler (evalI e) >>= \ ~(a,s) -> case a of
    Value x   -> setState s >> return x
    Goto _    -> error "Handler error: unexpected goto state.\n"
    Next      -> error "Handler error: unexpected next state.\n"
    Error err -> error $ "Handler error: " ++ err ++ ".\n"

  run :: [Command d] -> m (Expr d)
  run cmds = getState >>= runHandler (runI cmds) >>= \ ~(a,s) -> case a of
    Value x   -> setState s >> return x
    Goto _    -> error "Handler error: unexpected goto state.\n"
    Next      -> error "Handler error: unexpected next state.\n"
    Error err -> error $ "Handler error: " ++ err ++ ".\n"

instance Handle d a => Handle d (Expr d -> a) where
  eval :: Expr d -> Expr d -> a
  eval (Fun (x:xs) cmds) e = let cmd = Assign Simple x e
                             in eval (Fun xs (cmd:cmds))

  run :: [Command d] -> Expr d -> a
  run _ = error "Handler error: can't run commands with arguments.\n"

data Value a
  = Value a
  | Goto Label
  | Next
  | Error String

instance Functor Value where
  fmap f (Value a)   = Value (f a)
  fmap f (Goto l)    = Goto l
  fmap f Next        = Next
  fmap f (Error err) = Error err

newtype Handler d m a = Handler
  {
    runHandler :: DebugInfo d => State m d -> m (Value a, State m d)
  }

instance Functor m => Functor (Handler d m) where
  fmap f (Handler m) = Handler $ \ s -> fmap (\ ~(a, s') -> (f <$> a, s')) (m s)
  {-# INLINE fmap #-}

instance Monad m => Applicative (Handler d m) where
  pure a = Handler $  \s -> return (Value a, s)
  {-# INLINE pure #-}

  Handler mf <*> Handler mx = Handler $ mf >=> \case
    (Value f, s)   -> mx s >>= \case
      (Value x, s')   -> return (Value (f x), s')
      (Goto l, s')    -> return (Goto l, s')
      (Next, s')      -> return (Next, s')
      (Error err, s') -> return (Error err, s')
    (Goto l, s)    -> return (Goto l, s)
    (Next, s)      -> return (Next, s)
    (Error err, s) -> return (Error err, s)
  {-# INLINE (<*>) #-}

instance Monad m => Alternative (Handler d m) where
  empty = Handler $ \ s -> return (Next, s)
  {-# INLINE empty #-}

  Handler m <|> Handler n = Handler $ \ s -> m s >>= \ case
    (Next, _) -> n s
    x         -> return x
  {-# INLINE (<|>) #-}

instance Monad m => Monad (Handler d m) where
  m >>= k  = Handler $ runHandler (fmap k m) >=> \case
    (Value n, s)   -> runHandler n s
    (Goto l, s)    -> return (Goto l, s)
    (Next, s)      -> return (Next, s)
    (Error err, s) -> return (Error err, s)
  {-# INLINE (>>=) #-}

  return a = Handler $ \ s -> return (Value a, s)
  {-# INLINE return #-}

  fail str = Handler $ \ s -> return (Error str, s)
  {-# INLINE fail #-}

instance Monad m => MonadPlus (Handler d m) where
  mzero = Handler $ \ s -> return (Next, s)
  {-# INLINE mzero  #-}

  Handler m `mplus` Handler n = Handler $ \ s -> m s >>= \ case
    (Value _, _) -> n s
    (Next, _)    -> n s
    x            -> return x
  {-# INLINE mplus #-}

instance MonadTrans (Handler d) where
  lift m = Handler $ \ s -> m >>= \a -> return (Value a, s)
  {-# INLINE lift #-}

instance MonadIO m => MonadIO (Handler d m) where
  liftIO = lift . liftIO
  {-# INLINE liftIO #-}

instance Monad m => MonadState (State m d) (Handler d m) where
  state f = Handler $ \ s -> let ~(a,s') = f s in return (Value a, s')
  get = state $ \ s -> (s, s)
  put s = state $ const ((), s)

call :: Monad m => Handler d m a -> Handler d m a
call = label 0
{-# INLINE call #-}

exit :: Monad m => Handler d m a
exit = Handler $ \ s -> return (Goto 0, s)
{-# INLINE exit #-}

label l m = Handler $ runHandler m >=> \case
  x@(Goto n, s')
    | n == l    -> return (Next, s')
    | otherwise -> return x
  x -> return x
{-# INLINE label #-}

goto l = Handler $ \ s -> return (Goto l, s)
{-# INLINE goto #-}

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
evalCall (Fun [] cmds) [] = get >>= \s -> call (runI cmds) >>= \e -> put s >> return e
evalCall (Fun (x:xs) cmds) (a:as) = evalCall (Fun xs (Assign Simple x a:cmds)) as
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
  False -> empty

-- | Run a list of commands
runI :: (DebugInfo d, Monad m) => [Command d] -> Handler d m (Expr d)

runI (Apply f : cmds) = evalI f >> runI cmds

runI (Assign Simple l r : cmds) = ident l r >> runI cmds

runI (Assign Iterate l r : cmds) = evalI r >>= ident l >> runI cmds

{-
Handle an assignment command of the form 'p <- g', where
p is a program expression denoting a list of patterns;
g is a program expression denoting a list of values.
-}
runI (Assign Select (List ls) r : cmds) = evalL r >>= f ls >> runI cmds
  where
    f :: (DebugInfo d, Monad m) => [Expr d] -> [Expr d] -> Handler d m ()
    f [] _ = empty
    f (x:xs) (y:ys) = guard (length xs <= length ys) >> (ident x y >> f xs ys) <|> f (x:xs) ys

{-
Handle an assignment command of the form 'p ~= g', where
p is a program expression denoting a list of patterns;
g is a program expression denoting a list of values.
-}
runI (Assign Unord (List ls) r : cmds) = evalL r >>= \ rs -> let n = length rs in guard (length ls <= n + 1) >> f ls rs n >> runI cmds
  where
    f :: (DebugInfo d, Monad m) => [Expr d] -> [Expr d] -> Int -> Handler d m (Expr d)
    f [] _ n = empty
    f [Ref i AnySeq] ys n = setVal i (List ys)
    f (z@(Ref i AnySeq):xs) ys n = f (xs ++ [z]) ys n
    f (x:xs) (y:ys) n = guard (length xs <= length ys) >> ( (ident x y >> f xs ys (length ys)) <|> f (x:xs) (ys ++ [y]) (n-1) )

{-
Handle an assignment command of the form 'p << g', where
p is a program variable whose values are lists of logical expressions;
g is a program expression denoting a list of values.
-}
runI (Assign Append (Var i) (List rs) : cmds) = (getVal i >>= f >>= (setVal i . List)) >> runI cmds
  where
    f = \case (Just ~(List ls)) -> return (ls ++ rs); Nothing -> return rs

runI (Branch b : cmds) = get >>= \s -> runI b >> put s >> runI cmds

runI (Break l : cmds) = goto l

runI (Exit : cmds) = exit

runI (Guard c : cmds) = check c >> runI cmds

runI (IfBlock c tb fb : cmds) = evalB c >>= (\case True -> runI tb; False -> runI fb) >> runI cmds

runI (Import m : cmds) = putError "import does not supported"

runI (Label l : cmds) = label l (runI cmds)

runI (Return e : cmds) = return e >> exit

runI (Switch e cs : cmds) = asum ((\(x,ys) -> ident x e >> asum ((\(y,z) -> check y >> runI z) <$> ys)) <$> cs) >> runI cmds

runI (When c cmd : cmds) = evalB c >>= \cond -> when cond (void (runI [cmd])) >> runI cmds

runI (Yield e : cmds) = return e >> runI cmds

runI [] = return Void
