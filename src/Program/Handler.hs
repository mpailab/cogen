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
import           Utils

------------------------------------------------------------------------------------------
-- Data types and clases declaration

type Args = M.Map Int Expr

handle :: MonadPlus m => Program -> Args -> m ()
handle p s = evalStateT (run p) (Info s M.empty)

data ExprPtr

  = RPtr
    {
      getExpr :: Expr,
      rSwap   :: forall m . Monad m => Expr -> Handler m ()
    }

  | IPtr
    {
      getExpr :: Expr,
      up      :: ExprPtr,
      iSwap   :: Expr -> Expr -> Expr
    }

instance Eq ExprPtr where
  (==) _ _ = False
  (/=) _ _ = True

data Expr = BE Bool
          | IE Int
          | SE LSymbol
          | TE LTerm
          | PE ExprPtr
          | PFE [ProgStmt]
          | LE [Expr]
          deriving (Eq)

class Expression a where
  expr :: a -> Expr
  bool :: a -> Bool
  int  :: a -> Int
  list :: a -> [Expr]
  prog :: a -> [ProgStmt]

instance Expression Expr where

  expr (PE x) = expr x
  expr x      = x

  bool (BE x) = x
  bool _      = error "Non-Boolean expression"

  int (IE x) = x
  int _      = error "Non-integer expression"

  list (LE x) = x
  list _      = error "Non-list expression"

  prog (PFE x) = x
  prog (LE l) = mconcat $ map prog l
  prog _       = error "Non-program expression"

instance Expression ExprPtr where
  expr = getExpr
  bool = bool . expr
  int  = int  . expr
  prog = prog . expr
  list = list . expr

data Info = Info
  {
    assignments :: M.Map Int Expr,
    buffer      :: M.Map Int Expr
  }

type Handler = StateT Info

------------------------------------------------------------------------------------------
-- Functions

getAssignment :: Monad m => Int -> Handler m (Maybe Expr)
getAssignment x = M.lookup x . assignments <$> get

setAssignment :: Monad m => Int -> Expr -> Handler m ()
setAssignment x y = modify (\info -> info {buffer = M.insert x y (buffer info)})

swapAssignment :: Monad m => Int -> Expr -> Handler m ()
swapAssignment x y = modify (\info -> info {assignments = M.insert x y (assignments info)})

saveAssignments :: Monad m => Handler m ()
saveAssignments = modify (\info -> info {assignments = M.union (buffer info)
                                                                   (assignments info),
                                             buffer = M.empty})

eval :: Monad m => PTerm -> Handler m Expr

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

eval (Prog p) = return (PFE p)

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

eval Underscore = error "Evaluating error: cannot evaluate '_' symbol\n"

replace :: Monad m => Expr -> Expr -> Handler m ()
replace (PE (RPtr _ sw)) = sw
replace (PE (IPtr _ par sw)) = let re = expr par in replace (PE par) . sw re
replace _ = error "Ambiguous first argument of the replace operator"

make :: Monad m => PTerm -> Handler m ()
make (Replace x y) = join $ liftM2 replace (eval x) (eval y)


ident :: Monad m => PTerm -> PTerm -> Handler m Bool

ident x (X i) = getAssignment i >>= \case
  Just y  -> identPtr x y (RPtr y (swapAssignment i))
  Nothing -> error "Evaluating error: a variable is not initialized\n"

ident (X i) y = do
  e <- eval y
  getAssignment i >>= \case
    Just x
      | x == e    -> return True
      | otherwise -> return False
    Nothing       -> setAssignment i e >> return True

ident (Ref i x) y = do
  c <- ident x y
  if c
    then eval y >>= setAssignment i >> return True
    else return False

ident (I x) t = eval t >>= \case
  (IE y)
    | x == y    -> return True
    | otherwise -> return False

ident (B x) t = eval t >>= \case
  (BE y)
    | x == y    -> return True
    | otherwise -> return False

ident (S x) t = eval t >>= \case
  (SE y)
    | x == y    -> return True
    | otherwise -> return False

ident Underscore _ = return True

ident (Term (S x) p) (Term (S y) q)
  | x /= y    = return False
  | otherwise = ident p q

ident (List x) (List y)
  | length x == length y = fmap and (zipWithM ident x y)
  | otherwise            = return False

ident (Tuple x) (Tuple y)
  | length x == length y = fmap and (zipWithM ident x y)
  | otherwise            = return False

ident _ _ = return False


-- Identify a program term with a given expression
-- Returns True if there is an assignment of program variables for which the evaluating
-- of program term coincides with the expression, and False otherwise.
-- State of Handler stores assignments of program variables.
identExpr :: Monad m => PTerm -> Expr -> Handler m Bool

identExpr (X i) e = getAssignment i >>= \case
  Just x
    | x == e    -> return True
    | otherwise -> return False
  Nothing       -> setAssignment i e >> return True

identExpr (Ref i x) e = do
  c <- identExpr x e
  if c
    then setAssignment i e >> return True
    else return False

identExpr (I x) (IE y)
  | x == y    = return True
  | otherwise = return False

identExpr (B x) (BE y)
  | x == y    = return True
  | otherwise = return False

identExpr (S x) (SE y)
  | x == y    = return True
  | otherwise = return False

identExpr (Term (S x) s) (TE (y :> ts))
  | x /= y    = return False
  | otherwise = identExpr s (LE (fmap TE ts))

identExpr (List x) (LE y)
  | length x == length y = fmap and (zipWithM identExpr x y)
  | otherwise            = return False

identExpr (Tuple x) y = identExpr (List x) y

identExpr _ _ = return False

iftrue :: Applicative f => f () -> Bool -> f ()
iftrue f b = when b f

identPtr :: Monad m => PTerm -> Expr -> ExprPtr -> Handler m Bool

identPtr (X i) e _ = getAssignment i >>= \case
  Just x
    | x == e    -> return True
    | otherwise -> return False
  Nothing       -> setAssignment i e >> return True

identPtr (Ref i x) e ptr = do
  c <- identPtr x e ptr
  if c
    then setAssignment i e >> return True
    else return False

identPtr (Ptr i x) e ptr = do
  c <- identPtr x e ptr
  if c
    then setAssignment i (PE ptr) >> return True
    else return False

identPtr (I x) (IE y) _
  | x == y    = return True
  | otherwise = return False

identPtr (B x) (BE y) _
  | x == y    = return True
  | otherwise = return False

identPtr Underscore _ _ = return True

identPtr (S x) (SE y) _
  | x == y    = return True
  | otherwise = return False

identPtr (Term x s) (TE (y :> ts)) ptr = do
    cond <- identPtr x (SE y) (IPtr (SE y) ptr (\(TE (_ :> zs)) (SE z) -> TE (z :> zs)))
    if cond
      then let e = LE (fmap TE ts)
               f (TE (z :> _)) (LE zs) = TE (z :> fmap (\(TE t) -> t) zs)
           in identPtr s e (IPtr e ptr f)
      else return False

identPtr (List x) (LE y) ptr
  | length x == length y = f x y (\(LE z) -> ([], z)) (\[] z -> LE z)
  | otherwise            = return False
    where
      f :: Monad m => [PTerm] -> [Expr] -> (Expr -> ([Expr],[Expr])) -> ([Expr] -> [Expr] -> Expr) -> Handler m Bool
      f [] [] _ _ = return True
      f (x:xs) (y:ys) to from = liftM2 (&&)
        (identPtr x y (IPtr y ptr (\u v -> let (h, _:ts) = to u in from h (v:ts))))
        (f xs ys (\z -> let (h, t:ts) = to z in (t:h, ts))
                 (\(t:h) ts -> from h (t:ts)))

identPtr (Tuple x) y ptr = identPtr (List x) y ptr

identPtr _ _ _ = return False


-- Identify a program term with a given expression by a condition
-- Returns True if there is an assignment of program's variables for which the evaluating
-- of program term coincides with expression and the condition holds, and False otherwise.
-- Warnings: State of Handler can be changed only if there is such assignment.
identExprC :: Monad m => PTerm -> Expr -> PTerm -> Handler m Bool
identExprC p e c = do
  state <- get                -- save current state of Handler
  is_match <- liftM2 (\x (BE y) -> x && y) (identExpr p e <* saveAssignments) (eval c) -- identify term with expression by condition
  unless is_match (put state) -- restore current state of Handler if necessary
  return is_match

identC :: Monad m => PTerm -> PTerm -> PTerm -> Handler m Bool
identC p e c = do
  state <- get                -- save current state of Handler
  is_match <- liftM2 (\x (BE y) -> x && y) (ident p e <* saveAssignments) (eval c) -- identify term with expression by condition
  unless is_match (put state) -- restore current state of Handler if necessary
  return is_match

thVarOffset = 2^30

l2PTerm :: LTerm -> PTerm
l2PTerm (T (XL i)) = X (thVarOffset+i)
l2PTerm (T (IL i)) = I i
l2PTerm (T s@(SL _)) = S s
l2PTerm (h :>> arg) = Program.Term (S h) (List [l2PTerm arg])
l2PTerm (h :> args) = Program.Term (S h) (List (l2PTerm /@ args))

e2PTerm :: Expr -> PTerm
e2PTerm (BE b) = B b
e2PTerm (IE i) = I i
e2PTerm (SE s) = S s
e2PTerm (LE l) = List $ map e2PTerm l
e2PTerm (PE p) = e2PTerm $ expr p
e2PTerm (PFE p) = Prog p
e2PTerm (TE t) = l2PTerm t

replaceVRef :: Monad m => PTerm -> Handler m PTerm

replVR1 cns x = cns <$> replaceVRef x
replVR2 cns x y = liftM2 cns (replaceVRef x) (replaceVRef y)
replVRN cns args = cns <$> sequence (replaceVRef /@ args)

replaceVRef (RX i)   = fmap (e2PTerm . (<|. error "variable undefined")) (getAssignment i)
replaceVRef (List l) = replVRN List l
replaceVRef (Tuple l) = replVRN Tuple l
replaceVRef (Term h args) = replVR2 Term h args
replaceVRef (And args) = replVRN And args
replaceVRef (Or args) = replVRN Or args
replaceVRef (Not x) = replVR1 Not x
replaceVRef (Equal x y) = replVR2 Equal x y
replaceVRef (NEqual x y) = replVR2 NEqual x y
replaceVRef (In x y) = replVR2 In x y
replaceVRef (Args args) = replVR1 Args args
replaceVRef (Replace x y) = replVR2 Replace x y
replaceVRef (Ref r t) = replVR1 (Ref r) t
replaceVRef (Ptr p t) = replVR1 (Ptr p) t
replaceVRef y = return y

replaceVRefProg :: Monad m => Program -> Handler m Program
replaceVRefProg (Stmts ss) = Stmts <$> sequence (replaceVRefSt /@ ss)

replaceVRefSt :: Monad m => ProgStmt -> Handler m ProgStmt
replaceVRefSt (Assign tp p g c) = liftM3 (Assign tp) (replaceVRef p) (replaceVRef g) (replaceVRef c)
replaceVRefSt (Branch c b) = liftM2 Branch (replaceVRef c) (replaceVRefProg b)
replaceVRefSt (Switch g c cs) = liftM3 Switch (replaceVRef g) (replaceVRef c) (sequence $ (\(x,y) -> liftM2 (,) (replaceVRef x) (replaceVRefProg y)) /@ cs)
replaceVRefSt (Action a c) = liftM2 Action (replaceVRef a) (replaceVRef c)

-- | Run program of one statement
runStmt :: MonadPlus m => ProgStmt -> Handler m () -> Handler m ()
runStmt (Assign PMSelect p g c) runj = case g of
  List [e] -> f e
  List es  -> mapM_ f es
  _        -> eval g >>= \(LE es) -> mapM_ h es
  where
    f e = identC p e c >>= iftrue runj
    h e = identExprC p e c >>= iftrue runj

runStmt (Assign PMAppend p g c) runj = case p of
  X i -> do
    BE cc <- eval c
    guard cc
    var <- getAssignment i
    gg <- eval g
    newvar <- case (var <|. PFE [], gg) of
          (PFE x, LE y) -> PFE . (x ++ ) <$> sequence (replaceVRefSt /@ mconcat (map prog y)) -- if not defined, assign right part
          _ -> error "Evaluating error: invalid operands for '<<' operator\n"
    setAssignment i newvar
    runj
  _ -> error "Evaluating error: left of '<<' must be a variable\n"

runStmt (Branch c b) runj = do
  s <- get             -- save current state of Handler
  BE cond <- eval c    -- evaluate the condition
  when cond (run b)    -- run the branch program fragment if the condition holds
  put s                -- restore current state of Handler
  runj                 -- run the next program fragment

runStmt (Switch g c cs) runj = do
  s <- get             -- save current state of Handler
  runCase g c cs       -- handle a case corresponing to the expression and the condition
  put s                -- restore current state of Handler
  runj                 -- run the next program fragment

runStmt (Action a c) runj = do
  BE cond <- eval c    -- run the condition
  when cond (make a)   -- make the action if the condition holds
  runj                 -- run the next program fragment

-- | Run a program fragment
run :: MonadPlus m => Program -> Handler m ()
run (Stmts l) = foldr runStmt (return ()) l

-- Run a case corresponing to a given expression
runCase :: MonadPlus m => PTerm -> PTerm -> [(PTerm, Program)] -> Handler m ()
runCase e c [] = return ()
runCase e c ((p,b):s) = identC p e c >>= \x -> if x then run b else runCase e c s
