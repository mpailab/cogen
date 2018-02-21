{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}

{-|
Module      : Program.PSymbol
Description : Program symbols and terms
Copyright   : (c) Grigoriy Bokov, 2017
License     : GPL-3
Maintainer  : bokov@intsys.msu.ru
Stability   : experimental
Portability : POSIX

Program symbols are special symbol used for composing of program terms which are base elements of programs.

In order to add new program symbol need:
  1. add new constructor in data PSymbol,
  2. update Show/Read instances for PSymbol and PTerm,
  3. add new composing functions,
  4. add new evaluating functions.

Warning: Constructors of program symbols are private, do not exporte it.
         Try use composing and evaluating functions for this.
-}
module Program.PSymbol
    (
      -- exports
      PSymbol(..), PTerm,
      -- parsePTerm,
      var, cons,
      Program.PSymbol.list, list',
      Program.PSymbol.not,
      Program.PSymbol.and, and',
      Program.PSymbol.or, or',
      Program.PSymbol.eq,
      Program.PSymbol.neq,
      Program.PSymbol.args,
      isAction, isBool
    )
where

-- External imports
import           Control.Monad.State
import           Data.Char
import           Data.List
import           Data.Map            ((!))
import qualified Data.Map            as Map
import           Data.Maybe

-- Internal imports
import           LSymbol
import           Term
import Program.Data

------------------------------------------------------------------------------------------
-- Data and type declaration

-- | Type of program symbols
data PSymbol = X Int                    -- ^ program variable
             | I Int                    -- ^ integer constant
             | B Bool                   -- ^ boolean constant
             | S LSymbol                -- ^ user-defined logical symbol
             | List                     -- ^ list of terms
             | Tuple                    -- ^ tuple of terms
             | Not                      -- ^ logical negation
             | And                      -- ^ logical and
             | Or                       -- ^ logical or
             | Equal                    -- ^ equality of objects
             | NEqual                   -- ^ negation of equality of objects
             | In                       -- ^ including for elements of set
             | Args                     -- ^ arguments of term
             | Replace                  -- !
             deriving (Eq, Ord)

-- | Type of program terms
type PTerm = Term PSymbol

------------------------------------------------------------------------------------------
-- Write instances

instance Write PSymbol where
  write = writePSymbol

-- | Show instance for program symbols
instance Show PSymbol where
  show (X i)     = 't' : show i
  show (I i)     = show i
  show (B True)  = "True"
  show (B False) = "False"
  show Not       = "no"
  show And       = "and"
  show Or        = "or"
  show Equal     = "eq"
  show NEqual    = "ne"
  show In        = "in"
  show Args      = "args"
  show Replace   = "replace"

-- | Write a program symbol
writePSymbol :: LSymbolsBase m => PSymbol -> m String
writePSymbol (S s) = (name s) <$> lsymbols
writePSymbol s     = return (show s)

instance Write PTerm where
  write = writePTerm False

-- | Write sequence of program terms
writeSequence :: LSymbolsBase m => [PTerm] -> m String
writeSequence [t]    = writePTerm False t
writeSequence (t:ts) = writePTerm False t +>+ pure ", " +>+ writeSequence ts

-- | Write prefix expression
writePrefx :: LSymbolsBase m => PSymbol -> [PTerm] -> m String
writePrefx x [t] = write x +>+ writePTerm True t
writePrefx x ts =  write x +>+ (unwords <$> (mapM (\t -> writePTerm True t) ts))

-- | Write infix expression
writeInfx :: LSymbolsBase m => PSymbol -> [PTerm] -> m String
writeInfx x ts = liftM2 intercalate (pure " " +>+ write x +>+ pure " ")
                                    (mapM (\t -> writePTerm True t) ts)

-- | Write a program term
writePTerm :: LSymbolsBase m => Bool -> PTerm -> m String
writePTerm par t = let (x,y) = f t in if par && y then pure "(" +>+ x +>+ pure ")" else x
  where
    f (T x)            = (write x, False)
    f (x@(X _) :> y)   = (write x +>+ pure " [" +>+ writeSequence y +>+ pure "]", True)
    f (x@(X _) :>> y)  = (write x +>+ pure " " +>+ writePTerm True y, True)
    f (x@(S _) :> y)   = (write x +>+ pure " [" +>+ writeSequence y +>+ pure "]", True)
    f (x@(S _) :>> y)  = (write x +>+ pure " " +>+ writePTerm True y, True)
    f (List :> x)      = (pure "[" +>+ writeSequence x +>+ pure "]", False)
    f (Tuple :> x)     = (pure "(" +>+ writeSequence x +>+ pure ")", False)
    f (Not :> x)       = (writePrefx Not x, True)
    f (And :> x)       = (writeInfx And x, True)
    f (Or :> x)        = (writeInfx Or x, True)
    f (Equal :> x)     = (writeInfx Equal x, True)
    f (NEqual :> x)    = (writeInfx NEqual x, True)
    f (In :> x)        = (writeInfx In x, True)
    f (Args :> x)      = (writePrefx Args x, True)
    f (Replace :> x)   = (writePrefx Replace x, True)

------------------------------------------------------------------------------------------
-- Composing functions

-- | Get a program variable with a given number
class Var a where
  var :: Int -> a
instance Var PSymbol where var = X
instance Var PTerm  where var i = T (X i)

-- | Convert a given constant to a program symbol
class Cons a b where
  cons :: a -> b
instance Cons Int  PSymbol where cons = I
instance Cons Int  PTerm  where cons i = T (I i)
instance Cons Bool PSymbol where cons = B
instance Cons Bool PTerm  where cons c = T (B c)
instance Cons LSymbol PSymbol where cons = S
instance Cons LSymbol PTerm  where cons c = T (S c)

-- | Allows to define function with variable number of arguments
--   For example: if func :: [a] -> a, then (func <> []) :: a -> ... -> a -> a
--   and (func <> []) a1 ... an == func [a1, ..., an] for all n >= 0
class VarArgs a where
  (<>) :: ([PTerm] -> PTerm) -> [PTerm] -> a
instance VarArgs PTerm where (<>) f ts = f $ reverse ts
instance VarArgs a => VarArgs (PTerm -> a) where (<>) f ts t = f <> (t:ts)

-- | Tuple of program terms
tuple' :: [PTerm] -> PTerm
tuple' ts = Tuple :> ts
tuple :: (VarArgs a) => a
tuple = tuple' <> []

-- | List of program terms
list' :: [PTerm] -> PTerm
list' ts = List :> ts
list :: (VarArgs a) => a
list = list' <> []

-- | Negate a given program term
not :: PTerm -> PTerm
not (Equal  :> ts) = NEqual :> ts
not (NEqual :> ts) = Equal :> ts
not t              = Not :> [t]

-- | Take the logical and of a given program terms
and' :: [PTerm] -> PTerm
and' [] = T (B True)
and' [t] = t
and' (t:s) = And :> case (t, and' s) of
  (T (B True), y)        -> [y]
  (x@(T (B False)), y)   -> [x]
  (x, T (B True))        -> [x]
  (x, y@(T (B False)))   -> [y]
  (And :> xs, And :> ys) -> xs ++ ys
  (And :> xs, y)         -> xs ++ [y]
  (x, And :> ys)         -> x : ys
  (x, y)                 -> [x, y]

and :: (VarArgs a) => a
and = and' <> []

-- | Take the logical or of a given program terms
or' :: [PTerm] -> PTerm
or' [t] = t
or' (t:s) = Or :> case (t, or' s) of
  (x@(T (B True)), y)  -> [x]
  (T (B False), y)     -> [y]
  (x, y@(T (B True)))  -> [y]
  (x, T (B False))     -> [x]
  (Or :> xs, Or :> ys) -> xs ++ ys
  (Or :> xs, y)        -> xs ++ [y]
  (x, Or :> ys)        -> x : ys
  (x, y)               -> [x, y]

or :: (VarArgs a) => a
or = or' <> []

-- | Return a program term which is the equality of a given program terms
eq :: PTerm -> PTerm -> PTerm
eq x y = Equal :> [x,y]

-- | Return a program term which is the negation of equality of a given program terms
neq :: PTerm -> PTerm -> PTerm
neq x y = NEqual :> [x,y]

-- | Return a program term which is the list of arguments of a given program term
args :: PTerm -> PTerm
args t = Args :> [t]

------------------------------------------------------------------------------------------
-- Functions

keywordsBool :: [PSymbol]
keywordsBool = [Not, And, Or, Equal, NEqual]

keywordsAction :: [PSymbol]
keywordsAction = [Replace]

-- | Does a program term correspond to an action
isAction :: PTerm -> Bool
isAction (x :> _) = x `elem` keywordsAction

isBool :: PTerm -> Bool
isBool (x :> _) = x `elem` keywordsBool
