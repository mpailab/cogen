{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleInstances         #-}
{-# LANGUAGE MultiParamTypeClasses     #-}
-- {-# LANGUAGE RankNTypes                #-}
{-# LANGUAGE TypeSynonymInstances      #-}

{-|
Module      : Program.Handle
Description : Program interpreter
Copyright   : (c) Grigoriy Bokov, 2017
License     : GPL-3
Maintainer  : bokov@intsys.msu.ru
Stability   : experimental
Portability : POSIX
-}
module Program.Handle
    (
      -- exports
      Handle(..), Handler
    )
where

-- External imports
import           Control.Monad.State
import qualified Data.Map            as Map

-- Internal imports
import           LSymbol
import           Program
import           Program.PSymbol
import           Term

type Args = Map.Map Int LTerm

data Expr = EB Bool
          | ES LSymbol
          | ET LTerm
          | ETS [LTerm]
          | ETT (LTerm, LTerm)
          | ETTT (LTerm, LTerm, LTerm)

class Pack a where
  pack :: a -> Expr
  unpack :: Expr -> a

instance Pack Bool where
  pack x = EB x
  unpack (EB x) = x
  unpack _ = error "Can't unpack Bool expression.\n"

instance Pack LSymbol where
  pack x = ES x
  unpack (ES x) = x
  unpack _ = error "Can't unpack LSymbol expression.\n"

type Handler a = State Args a

class Handle a b where
  handle :: a -> Handler b

apply :: (a -> b) -> Expr -> Expr
apply f = pack . f . unpack

convert :: [Expt] -> Expr
convert [] = ETS []
convert (x:xs)

applyL :: ([a] -> b) -> [Expr] -> Expr
applyL f [] = pack (f [])
applyL f (x:xs) = pack

eval :: PTerm -> Handler Expr

eval (T (B x)) = return (EB x)

eval (Not :> [x]) = fmap (pack . Prelude.not . unpack) (eval x)

eval (And :> x) = fmap (pack . Prelude.and . unpack) (mapM eval x)
  expr <- eval x
  case expr of
    EB c  -> return $ EB (Prelude.not c)
    otherwise -> error "Evaluating error: Not has been applied to a non-boolean type.\n"

--   fmap Prelude.not (eval x)
--   handle (And :> x)        = fmap Prelude.and (mapM handle x)
--   handle (Or :> x)         = fmap Prelude.or (mapM handle x)
--   handle (Equal :> [x,y])  = liftM2 (==) (handle x) (handle y)
--   handle x@(NEqual :> _)   = fmap Prelude.not (handle x)
--
-- instance Handle PTerm LSymbol where
--   handle (T (S x)) = return x
--
-- instance Handle PTerm LTerm where
--   handle (T (X i)) = do
--     table <- get
--     case Map.lookup i table of
--       Just x  -> return x
--       Nothing -> error "Handler error: a variable is not initialized\n"
--
-- instance Handle PTerm [LTerm] where
--   handle (List :> x)   = mapM handle x
--   handle (Args :> [x]) = do
--     t <- handle x :: Handler LTerm
--     return (Term.args t)
--
-- instance Handle PTerm (LTerm, LTerm) where
--   handle (Tuple :> [x1,x2]) = do
--     y1 <- handle x1 :: Handler LTerm
--     y2 <- handle x2 :: Handler LTerm
--     return (y1,y2)
--
-- instance Handle PTerm (LTerm, LTerm, LTerm) where
--   handle (Tuple :> [x1,x2,x3]) = do
--     y1 <- handle x1 :: Handler LTerm
--     y2 <- handle x2 :: Handler LTerm
--     y3 <- handle x3 :: Handler LTerm
--     return (y1,y2,y3)
--
-- instance Handle PTerm Bool where
--   handle (T (B x))        = return x
--   handle (Not :> [x])     = fmap Prelude.not (handle x)
--   handle (And :> x)       = fmap Prelude.and (mapM handle x)
--   handle (Or :> x)        = fmap Prelude.or (mapM handle x)
--   handle (Equal :> [x,y]) = liftM2 (==) (handle x) (handle y)
--   handle x@(NEqual :> _)  = fmap Prelude.not (handle x)
--
-- instance Handle PTerm LSymbol where
--   handle (T (S x)) = return x
--
-- instance Handle PTerm LTerm where
--   handle (T (X i)) = do
--     table <- get
--     case Map.lookup i table of
--       Just x  -> return x
--       Nothing -> error "Handler error: a variable is not initialized\n"
--
-- instance Handle PTerm [LTerm] where
--   handle (List :> x)   = mapM handle x
--   handle (Args :> [x]) = do
--     t <- handle x :: Handler LTerm
--     return (Term.args t)
--
-- instance Handle PTerm (LTerm, LTerm) where
--   handle (Tuple :> [x1,x2]) = do
--     y1 <- handle x1 :: Handler LTerm
--     y2 <- handle x2 :: Handler LTerm
--     return (y1,y2)
--
-- instance Handle PTerm (LTerm, LTerm, LTerm) where
--   handle (Tuple :> [x1,x2,x3]) = do
--     y1 <- handle x1 :: Handler LTerm
--     y2 <- handle x2 :: Handler LTerm
--     y3 <- handle x3 :: Handler LTerm
--     return (y1,y2,y3)
--
-- class Identify a where
--   identify :: PTerm -> a -> Handler Bool
--
-- -- Identify a program term with a given logical term
-- -- Returns True if there is an assignment of program variables for which the program term
-- -- coincides with the logical term, and False otherwise.
-- -- State monad stores assignments of program variables.
-- instance Identify LTerm where
--   identify (T (X i)) t = do
--     s <- get
--     case Map.lookup i s of
--       Just x
--         | x == t    -> return True
--         | otherwise -> return False
--       Nothing       -> modify (Map.insert i t) >> return True
--
--   identify (T x) (T y)
--     | x == S y  = return True
--     | otherwise = return False
--
--   identify (x :> ps) (y :> ts)
--     | length ps /= length ts = return False
--     | x == S y               = fmap Prelude.and (zipWithM identify ps ts)
--     | otherwise              = return False
--
--   identify _ _ = return False
--
-- instance Identify [LTerm] where
--   identify (List :> x) y
--     | length x == length y = fmap Prelude.and (zipWithM identify x y)
--     | otherwise            = return False
--
-- instance Identify (LTerm, LTerm) where
--   identify (Tuple :> [x1,x2]) (y1,y2) = do
--     z1 <- identify x1 y1
--     z2 <- identify x2 y2
--     return (z1 && z2)
--
-- instance Identify (LTerm, LTerm, LTerm) where
--   identify (Tuple :> [x1,x2,x3]) (y1,y2,y3) = do
--     z1 <- identify x1 y1
--     z2 <- identify x2 y2
--     z3 <- identify x3 y3
--     return (z1 && z2 && z3)
--
-- ------------------------------------------------------------------------------------------
-- -- Handle instances
--
-- -- instance Handle PTerm Bool where
-- -- handle (T sym)     = return $ eval sym
-- -- handle (sym :> ts) = fmap (eval sym) (mapM handle ts)
-- --
-- -- instance Handle PTerm LSymbol where
-- -- handle (T sym) = return $ eval sym
-- --
-- -- instance Handle PTerm LTerm where
-- -- handle (T sym) = get >>= \table -> eval sym table
-- --
-- -- instance Handle PTerm [a] where
-- -- handle (sym :> ts) = fmap (eval sym) (mapM handle ts)
--
-- -- | Handle a program fragment
-- instance Handle Program () where
--   handle (Assign p g c j) = do
--     e <- handle g        -- handle an expression corresponding to the generator
--     jump <- ident p e c  -- identify the term with the pattern by the condition
--     when jump (handle j) -- handle the next program fragment if term matches pattern
--
--   handle (Branch c b j) = do
--     s <- get             -- save current state of Handler
--     cond <- handle c     -- handle the condition
--     when cond (handle b) -- handle the branch program fragment if the condition holds
--     put s                -- restore current state of Handler
--     handle j             -- handle the next program fragment
--
--   handle (Switch e c cs j) = do
--     s <- get             -- save current state of Handler
--     e <- handle e        -- handle an expression corresponing to the expression
--     handleCase e c cs    -- handle a case corresponing to the expression and the condition
--     put s                -- restore current state of Handler
--     handle j             -- handle the next program fragment
--
--   handle (Action a c j) = do
--     cond <- handle c     -- handle the condition
--     when cond (handle a) -- handle the action if the condition holds
--     handle j             -- handle the next program fragment
--
--   handle Empty = return ()
--
--
-- -- Identify a term with a given pattern by a condition
-- -- Returns True if there is an assignment of pattern's variables for which the term
-- -- matches the pattern and the condition holds, and False otherwise.
-- -- Warnings: State of Handler can be changed only if the term matches the pattern.
-- -- ident :: LTerm -- a term
-- --       -> PTerm -- a pattern
-- --       -> PTerm -- a condition
-- --       -> Handler Bool
-- ident p e c = do
--   s <- get                -- save current state of Handler
--   is_match <- liftM2 (&&) (identify p e) (handle c) -- identify term with pattern by condition
--   unless is_match (put s) -- restore current state of Handler if necessary
--   return is_match
--
-- -- Handle a case corresponing to a given term
-- -- handleCase :: LTerm -> PTerm -> [(PTerm, Program)] -> Handler ()
-- handleCase e c [] = return ()
-- handleCase e c ((p,b):s) = ident p e c >>= \x -> if x then handle b else handleCase e c s
