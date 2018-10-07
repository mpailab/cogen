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

type Args = M.Map Int LExpr

handle :: Monad m => Program -> Args -> m Args
handle p s = return s
-- handle p s = let info = execState (run p) (Info s M.empty)
--              in return (assignments info)


data ExprPtr

  = RPtr
    {
      getExpr :: LExpr,
      rSwap   :: forall m . Monad m => LExpr -> Handler m ()
    }

  | IPtr
    {
      getExpr :: LExpr,
      up      :: ExprPtr,
      iSwap   :: LExpr -> LExpr -> LExpr
    }

type LSwap = forall m . Monad m => LExpr -> Handler m ()

data LFun
  = BFun [LExpr] Bool
  | EFun [LExpr] LExpr

data LEntry
  = Ptr LExpr LSwap
  | Fun LFun
  deriving (Eq, Ord, Show)

-- | Type of logical term
type LTExpr = TExpr LEntry

-- | Type of logical expressions
type LExpr = Expr LEntry

-- class Expression a where
--   expr :: a -> LExpr
--   bool :: a -> Bool
--   int  :: a -> Int
--
-- instance Expression LExpr where
--
--   expr (PE x) = expr x
--   expr x      = x
--
--   bool (BE x) = x
--   bool _      = error "Non-Boolean expression"
--
--   int (IE x) = x
--   int _      = error "Non-integer expression"
--
-- instance Expression ExprPtr where
--   expr = getExpr
--   bool = bool . expr
--   int = int . expr

data Info = Info
  {
    assignments :: M.Map Int LExpr,
    buffer      :: M.Map Int LExpr,
    swap        :: LSwap
  }

type Handler = StateT Info

------------------------------------------------------------------------------------------
-- -- Functions
--
-- getAssignment :: Monad m => Int -> Handler m (Maybe LExpr)
-- getAssignment x = M.lookup x . assignments <$> get
--
-- setAssignment :: Monad m => Int -> LExpr -> Handler m ()
-- setAssignment x y = modify (\info -> info {buffer = M.insert x y (buffer info)})
--
-- swapAssignment :: Monad m => Int -> LExpr -> Handler m ()
-- swapAssignment x y = modify (\info -> info {assignments = M.insert x y (assignments info)})
--
-- saveAssignments :: Monad m => Handler m ()
-- saveAssignments = modify (\info -> info {assignments = M.union (buffer info)
--                                                                    (assignments info),
--                                              buffer = M.empty})

-- eval :: Monad m => PExpr -> Handler m LExpr
-- eval (Bool x) = Bool <$> evalB x
-- eval (Aggr x) = Aggr <$> evalE x

evalB :: Monad m => PBool -> Handler m Bool
evalB (Const c) = return c
evalB (Equal x y) = liftM2 (==) (evalT x) (evalT y)
evalB (NEqual x y) = liftM2 (!=) (evalT x) (evalT y)
evalB (In x y) = evalT x >>= \ex -> evalE y >>= \case
  (Alt   ey) -> return (elem ex ey)
  (Tuple ey) -> return (elem ex ey) -- tuples are handled as lists
  (List  ey) -> return (elem ex ey)
  (Set   ey) -> return (elem ex ey) -- sets are handled as lists
  _          -> error "Evaluating error: a wrong in-expression\n"
evalB (Not x) = Prelude.not <$> evalB x
evalB (And xs) = mapM evalB xs >>= foldrM (\y -> return . (y &&)) True
evalB (Or xs) = mapM evalB xs >>= foldrM (\y -> return . (y ||)) True
evalB (BVar x) = getAssignment x >>= \case
  Just y  -> return y
  Nothing -> error "Evaluating error: a boolean variable is not initialized\n"

evalE :: Monad m => PExpr -> Handler m LExpr
evalE (Term (T x)) = (Term . f) <$> evalT x
evalE (Term (x :> xs)) = Term <$> (x :>) <$> mapM (f <$> evalT) xs
evalE (Alt xs)   = Alt   <$> mapM evalE xs
evalE (Tuple xs) = Tuple <$> mapM evalE xs
evalE (List xs)  = List  <$> mapM evalE xs
evalE (Set xs)   = Set   <$> mapM evalE xs
where
  f :: LTerm -> (Term LSymbol LTExpr)
  f (T y) = T y
  f ((Sym y) :> ys) = y :> (map f ys)
  f y = T y

evalT :: Monad m => PTerm -> Handler m LTerm
evalT (T x) = evalTE x
evalT (x :> xs) = evalTE x >>= \case
  (T (Sym s)) -> (s :>) <$> (mapM evalT xs)
  _ -> error "Evaluating error: a term header is not a logical symbol\n"

evalTE :: Monad m => PTExpr -> Handler m LTerm
evalTE (Var x) = getAssignment x >>= \case
  Just y  -> return y
  Nothing -> error "Evaluating error: a program variable is not initialized\n"
evalTE (Ptr x e) =
evalTE (S x) = return (T (LSym x))
evalTE (I x) = return (T (LSym (IL x)))

eval (X i) = getAssignment i >>= \case
  Just x  -> return x
  Nothing -> error "Evaluating error: a variable is not initialized\n"

eval (I i) = return (IE i)

eval (B c) = return (BE c)

eval (S x) = return (SE x)

eval (Program.Term x y) = do
  SE s <- eval x
  LE es <- eval y
  return (TE (s :> map (\(TE t) -> t) es))

eval (List x) = LE <$> mapM eval x

eval (Tuple x) = LE <$> mapM eval x

eval (Not x) = BE . Prelude.not . bool <$> eval x

eval (And x) = BE <$> (mapM eval x >>= foldrM (\y -> return . (bool y &&)) True)

eval (Or x) = BE <$> (mapM eval x >>= foldrM (\y -> return . (bool y ||)) False)

eval (Equal x y) = BE <$> liftM2 (==) (eval x) (eval y)

eval (NEqual x y) = BE <$> liftM2 (/=) (eval x) (eval y)

eval (In x y) = do
  xx <- eval x
  LE yy <- eval y
  return( BE (xx `elem` yy))

eval (Args x) = eval x >>= \case
  (TE t) -> (return . LE . fmap TE . Term.args) t
  (PE p) -> (return . LE . fmap getTermRef . zip [0..] . Term.args) t
    where
      TE t = expr p
      getTermRef (i,x) = PE (IPtr (TE x) p (\(TE u) (TE v) -> TE (Term.change u i v)))

-- replace :: Monad m => LExpr -> LExpr -> Handler m ()
-- replace (PE (RPtr _ sw)) = sw
-- replace (PE (IPtr _ par sw)) = let re = expr par in replace (PE par) . sw re
-- replace _ = error "Ambiguous first argument of the replace operator"
--
-- make :: Monad m => PTerm -> Handler m ()
-- make (Replace x y) = join $ liftM2 replace (eval x) (eval y)
--
--
-- ident :: Monad m => PTerm -> PTerm -> Handler m Bool
--
-- ident x (X i) = getAssignment i >>= \case
--   Just y  -> identPtr x y (RPtr y (swapAssignment i))
--   Nothing -> error "Evaluating error: a variable is not initialized\n"
--
-- ident (X i) y = do
--   e <- eval y
--   getAssignment i >>= \case
--     Just x
--       | x == e    -> return True
--       | otherwise -> return False
--     Nothing       -> setAssignment i e >> return True
--
-- ident (Ref i x) y = do
--   c <- ident x y
--   if c
--     then eval y >>= setAssignment i >> return True
--     else return False
--
-- ident (I x) t = eval t >>= \case
--   (IE y)
--     | x == y    -> return True
--     | otherwise -> return False
--
-- ident (B x) t = eval t >>= \case
--   (BE y)
--     | x == y    -> return True
--     | otherwise -> return False
--
-- ident (S x) t = eval t >>= \case
--   (SE y)
--     | x == y    -> return True
--     | otherwise -> return False
--
-- ident (Term (S x) p) (Term (S y) q)
--   | x /= y    = return False
--   | otherwise = ident p q
--
-- ident (List x) (List y)
--   | length x == length y = fmap and (zipWithM ident x y)
--   | otherwise            = return False
--
-- ident (Tuple x) (Tuple y)
--   | length x == length y = fmap and (zipWithM ident x y)
--   | otherwise            = return False
--
-- ident _ _ = return False
--
--
-- -- Identify a program term with a given expression
-- -- Returns True if there is an assignment of program variables for which the evaluating
-- -- of program term coincides with the expression, and False otherwise.
-- -- State of Handler stores assignments of program variables.
-- identExpr :: Monad m => PTerm -> LExpr -> Handler m Bool
--
-- identExpr (X i) e = getAssignment i >>= \case
--   Just x
--     | x == e    -> return True
--     | otherwise -> return False
--   Nothing       -> setAssignment i e >> return True
--
-- identExpr (Ref i x) e = do
--   c <- identExpr x e
--   if c
--     then setAssignment i e >> return True
--     else return False
--
-- identExpr (I x) (IE y)
--   | x == y    = return True
--   | otherwise = return False
--
-- identExpr (B x) (BE y)
--   | x == y    = return True
--   | otherwise = return False
--
-- identExpr (S x) (SE y)
--   | x == y    = return True
--   | otherwise = return False
--
-- identExpr (Term (S x) s) (TE (y :> ts))
--   | x /= y    = return False
--   | otherwise = identExpr s (LE (fmap TE ts))
--
-- identExpr (List x) (LE y)
--   | length x == length y = fmap and (zipWithM identExpr x y)
--   | otherwise            = return False
--
-- identExpr (Tuple x) y = identExpr (List x) y
--
-- identExpr _ _ = return False
--
--
-- identPtr :: Monad m => PTerm -> LExpr -> ExprPtr -> Handler m Bool
--
-- identPtr (X i) e _ = getAssignment i >>= \case
--   Just x
--     | x == e    -> return True
--     | otherwise -> return False
--   Nothing       -> setAssignment i e >> return True
--
-- identPtr (Ref i x) e ptr = do
--   c <- identPtr x e ptr
--   if c
--     then setAssignment i e >> return True
--     else return False
--
-- identPtr (Ptr i x) e ptr = do
--   c <- identPtr x e ptr
--   if c
--     then setAssignment i (PE ptr) >> return True
--     else return False
--
-- identPtr (I x) (IE y) _
--   | x == y    = return True
--   | otherwise = return False
--
-- identPtr (B x) (BE y) _
--   | x == y    = return True
--   | otherwise = return False
--
-- identPtr (S x) (SE y) _
--   | x == y    = return True
--   | otherwise = return False
--
-- identPtr (Term x s) (TE (y :> ts)) ptr = do
--     cond <- identPtr x (SE y) (IPtr (SE y) ptr (\(TE (_ :> zs)) (SE z) -> TE (z :> zs)))
--     if cond
--       then let e = LE (fmap TE ts)
--                f (TE (z :> _)) (LE zs) = TE (z :> fmap (\(TE t) -> t) zs)
--            in identPtr s e (IPtr e ptr f)
--       else return False
--
-- identPtr (List x) (LE y) ptr
--   | length x == length y = f x y (\(LE z) -> ([], z)) (\[] z -> LE z)
--   | otherwise            = return False
--     where
--       f :: Monad m => [PTerm] -> [LExpr] -> (LExpr -> ([LExpr],[LExpr])) -> ([LExpr] -> [LExpr] -> LExpr) -> Handler m Bool
--       f [] [] _ _ = return True
--       f (x:xs) (y:ys) to from = liftM2 (&&)
--         (identPtr x y (IPtr y ptr (\u v -> let (h, _:ts) = to u in from h (v:ts))))
--         (f xs ys (\z -> let (h, t:ts) = to z in (t:h, ts))
--                  (\(t:h) ts -> from h (t:ts)))
--
-- identPtr (Tuple x) y ptr = identPtr (List x) y ptr
--
-- identPtr _ _ _ = return False
--
--
-- -- Identify a program term with a given expression by a condition
-- -- Returns True if there is an assignment of program's variables for which the evaluating
-- -- of program term coincides with expression and the condition holds, and False otherwise.
-- -- Warnings: State of Handler can be changed only if there is such assignment.
-- identExprC :: Monad m => PTerm -> LExpr -> PTerm -> Handler m Bool
-- identExprC p e c = do
--   state <- get                -- save current state of Handler
--   is_match <- liftM2 (\x (BE y) -> x && y) (identExpr p e <* saveAssignments) (eval c) -- identify term with expression by condition
--   unless is_match (put state) -- restore current state of Handler if necessary
--   return is_match
--
-- identC :: Monad m => PTerm -> PTerm -> PTerm -> Handler m Bool
-- identC p e c = do
--   state <- get                -- save current state of Handler
--   is_match <- liftM2 (\x (BE y) -> x && y) (ident p e <* saveAssignments) (eval c) -- identify term with expression by condition
--   unless is_match (put state) -- restore current state of Handler if necessary
--   return is_match

class Ident a b where
  ident :: (Expr a) -> (Expr b) -> Handler m Bool
  ident (Term (x :> xs)) (Term (y :> ys)) = if x == y then f xs ys else return False
  ident (Any xs) y = g (\x -> ident x y) xs
  ident x (Any ys) = g (ident x) ys
  ident (Tuple xs) (Tuple ys) = f xs ys
  ident (List xs) (List ys) = f xs ys
  ident (Set xs) (Set ys) = f xs ys
  where
    f xs ys = zipWithM ident xs ys >>= foldrM (\z -> return . (z &&)) True
    g x ys  = mapM x ys >>= foldrM (\z -> return . (z ||)) False

instance Ident PTExpr b where
  ident (Term (T (Var i))) y = getAssignment i >>= \case
    Just x  -> ident x y
    Nothing -> eval y >>= setAssignment i >> return True
  ident (Term (T (Ptr i x))) y = ident x y >>= \case
    True  -> eval y >>= \e -> getSwap >>= \sw -> setAssignment i (Ptr e sw) >> return True
    False -> return False
  ident (Term (T (Ref i x))) y = ident x y >>= \case
    True  -> eval y >>= setAssignment i >> return True
    False -> return False
  ident (Term (T (Sym x))) y = eval y >>= \case
    (Term (T (Sym s))) -> return (x == s)
    otherwise          -> return False
  ident (Term (T (Fun name args))) y = error "Error: a function call is not supported"
  ident (Term (T (IfElse c te fe))) y = eval c >>= \case
    True  -> ident te y
    False -> ident fe y
  ident (Term (T (CaseOf e []))) y = return False
  ident (Term (T (CaseOf e ((pe,ce):cs)))) y = ident e pe >>= \case
    True  -> ident ce y
    False -> ident (Term (T (CaseOf e cs))) y

instance Ident PTExpr PTExpr where
  ident x (Term (T (Var i))) = getAssignment i >>= \case
    Just y  -> setSwap (swapAssignment i) >> ident x y
    Nothing -> error "Error: a program variable is not initialized\n"

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
    f :: [PExpr] -> [Expr a] -> Handler m LExpr
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
run ((Assign PMUnord p g c) : cs) =
  eval p >>= \case
    (List ps) -> (eval g >>= \case
      (List gs) -> when (length ps <= length gs + 1) (f ps gs (length gs))
      _         -> error "Unsupported value in PMUnord assignment command")
    _         -> error "Unsupported pattern in PMUnord assignment command"
  where
    f [] [] 0 = do
      -- check the condition
      Bool cond <- eval c
      -- run the next commands if necessary
      when cond (run cs)

    f _ _ 0 = return

    f [Ref i $ Aggr $ Sym AnySequence] ys n = do
      -- assign a program variable with the number i the list ys
      ident (Aggr $ Sym $ X i) (List ys)
      -- check the condition
      Bool cond <- eval c
      -- run the next commands if necessary
      when cond (run cs)

    f (z@(Ref i $ Aggr $ Sym AnySequence):x:xs) (y:ys) n = do
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
run ((Assign PMAppend p g c) : cs) = do
  Bool cond <- eval c
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
  Bool cond <- eval c  -- evaluate the condition
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
  Bool cond <- eval c
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
      when is_match (saveAssignments *> eval ic >>= \(Bool cond) -> when cond (run ib))
      -- restore current state of Handler
      put state
      -- run the next case
      f is

-- Handle an acting command
run ((Action a c) : cs) = do
  Bool cond <- eval c  -- run the condition
  when cond (make a)   -- make the action if the condition holds
  run cs                -- run the next commands

-- Handle an empty command
run Empty = return Expr.NONE
