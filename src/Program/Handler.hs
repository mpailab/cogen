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

instance Eq ExprPtr where
  (==) _ _ = False
  (/=) _ _ = True

data LTerminal
  = LSym LSymbol
  | LFun ([LExpr] -> LExpr)

type LExpr = Expr LSymbol ExprPtr Bool

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
    buffer      :: M.Map Int LExpr
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
--
-- eval :: Monad m => PTerm -> Handler m LExpr
--
-- eval (X i) = getAssignment i >>= \case
--   Just x  -> return x
--   Nothing -> error "Evaluating error: a variable is not initialized\n"
--
-- eval (I i) = return (IE i)
--
-- eval (B c) = return (BE c)
--
-- eval (S x) = return (SE x)
--
-- eval (Program.Term x y) = do
--   SE s <- eval x
--   LE es <- eval y
--   return (TE (s :> map (\(TE t) -> t) es))
--
-- eval (List x) = LE <$> mapM eval x
--
-- eval (Tuple x) = LE <$> mapM eval x
--
-- eval (Not x) = BE . Prelude.not . bool <$> eval x
--
-- eval (And x) = BE <$> (mapM eval x >>= foldrM (\y -> return . (bool y &&)) True)
--
-- eval (Or x) = BE <$> (mapM eval x >>= foldrM (\y -> return . (bool y ||)) False)
--
-- eval (Equal x y) = BE <$> liftM2 (==) (eval x) (eval y)
--
-- eval (NEqual x y) = BE <$> liftM2 (/=) (eval x) (eval y)
--
-- eval (In x y) = do
--   xx <- eval x
--   LE yy <- eval y
--   return( BE (xx `elem` yy))
--
-- eval (Args x) = eval x >>= \case
--   (TE t) -> (return . LE . fmap TE . Term.args) t
--   (PE p) -> (return . LE . fmap getTermRef . zip [0..] . Term.args) t
--     where
--       TE t = expr p
--       getTermRef (i,x) = PE (IPtr (TE x) p (\(TE u) (TE v) -> TE (Term.change u i v)))
--
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


-- | Run a list of commands
run :: Monad m => [Command] -> Handler m LExpr

run ((Assign PMSelect p g c) : cs) =
  eval p >>= \case
    (List ps) -> (eval g >>= \case
      (List gs) -> when (length ps <= length gs) (f ps gs)
      _         -> error "Unsupported value in PMSelect assignment command")
    _         -> error "Unsupported pattern in PMSelect assignment command"
  where
    f [] _ = do
      -- check the condition
      is_match <- eval c
      -- run the next command if necessary
      when is_match (run cs)

    f (x:xs) (y:ys) = do
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

run ((Assign PMUnord p g c) : cs) =
  eval g >>= \case
  (List gs) -> (eval p >>= \case
    (List ps)     -> when (length ps == length gs) (f ps gs)
    (List_ ps pt) -> when (length ps <= length gs) (h ps pt gs)
    _             -> error "Unsupported pattern in PMUnord assignment command")
  _         -> error "Unsupported value in PMUnord assignment command")
  where
    f [] [] = do
      -- check the condition
      is_match <- eval c
      -- run the next command if necessary
      when is_match (run cs)

    f (x:xs) (y:ys) = do
      -- save current state of Handler
      state <- get
      -- identify the first pattern with the first value
      is_match <- ident x y
      -- continue the identification of remaining patterns
      when is_match (saveAssignments *> f xs ys)
      -- restore current state of Handler
      put state
      -- identify the list of patterns with the shifted values
      f (x:xs) (ys ++ [y])

    h [] pt ys = do
      ident pt (List ys)
      -- check the condition
      is_match <- eval c
      -- run the next command if necessary
      when is_match (run cs)

    h (x:xs) pt (y:ys) = do
      -- save current state of Handler
      state <- get
      -- identify the first pattern with the first value
      is_match <- ident x y
      -- continue the identification of remaining patterns
      when is_match (saveAssignments *> h xs pt ys)
      -- restore current state of Handler
      put state
      -- identify the list of patterns with the shifted values
      f (x:xs) pt (ys ++ [y])

-- run ((Assign t p g c) : cs) = case t of
--   PMSelect -> case p
--     List
--     List [e] -> f e
--     List es  -> mapM_ f es
--     _        -> eval g >>= \(LE es) -> mapM_ h es
--     where
--       f e = identC p e c >>= \jump -> when jump (run j)
--       h e = identExprC p e c >>= \jump -> when jump (run j)
--
-- run (Branch c b j) = do
--   s <- get             -- save current state of Handler
--   BE cond <- eval c  -- evaluate the condition
--   when cond (run b)    -- run the branch program fragment if the condition holds
--   put s                -- restore current state of Handler
--   run j                -- run the next program fragment
--
-- run (Switch g c cs j) = do
--   s <- get             -- save current state of Handler
--   runCase g c cs       -- handle a case corresponing to the expression and the condition
--   put s                -- restore current state of Handler
--   run j                -- run the next program fragment
--
-- run (Action a c j) = do
--   BE cond <- eval c  -- run the condition
--   when cond (make a)   -- make the action if the condition holds
--   run j                -- run the next program fragment
--
-- run Empty = return ()
--
-- -- Run a case corresponing to a given expression
-- runCase :: Monad m => PTerm -> PTerm -> [(PTerm, Program)] -> Handler m ()
-- runCase e c [] = return ()
-- runCase e c ((p,b):s) = identC p e c >>= \x -> if x then run b else runCase e c s
