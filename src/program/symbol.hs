{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}

{-|
Module      : Program.Symbol
Description : Program symbols and terms
Copyright   : (c) Grigoriy Bokov, 2017
License     : GPL-3
Maintainer  : bokov@intsys.msu.ru
Stability   : experimental
Portability : POSIX

Program symbols are special symbol used for composing of program terms which are base elements of programs.

In order to add new program symbol need:
  1. add new constructor in data Symbol,
  2. update Show/Read instances for Symbol and PTerm,
  3. add new composing functions,
  4. add new evaluating functions.

Warning: Constructors of program symbols are private, do not exporte it.
         Try use composing and evaluating functions for this.
-}
module Program.Symbol
    (
      -- exports
      Symbol, PTerm,
      var, cons,
      Program.Symbol.not,
      Program.Symbol.and,
      Program.Symbol.or,
      Program.Symbol.eq,
      Program.Symbol.neq,
      Program.Symbol.header,
      Program.Symbol.args,
      eval
    )
where

-- External imports
import           Data.Char
import           Data.List

-- Internal imports
import qualified Symbol    as L
import           Term

------------------------------------------------------------------------------------------
-- Data and type declaration

-- | Type of program symbols
data Symbol = X Int                    -- ^ program variable
            | C Int                    -- ^ integer constants
            | B Bool                   -- ^ boolean constants
            | S L.Symbol               -- ^ user-defined logical symbols
            | Not                      -- ^ logical negation
            | And                      -- ^ logical and
            | Or                       -- ^ logical or
            | Equal                    -- ^ equality of objects
            | NEqual                   -- ^ negation of equality of objects
            | Header                   -- ^ function symbol of term
            | Args                     -- ^ arguments of term
            | Plus                     -- ! symbol +
            | Replacing                -- !
            deriving (Eq, Ord)

-- | Type of program terms
type PTerm = Term Symbol

------------------------------------------------------------------------------------------
-- Show instances

-- | Show instance for program symbols
instance Show Symbol where
  show (X i)            = 'p' : show i
  show (C i)            = show i
  show (B True)         = "T"
  show (B False)        = "F"
  show (S (L.Symbol i)) = 's': show i
  show Not              = "!"
  show And              = "&&"
  show Or               = "||"
  show Equal            = "=="
  show NEqual           = "!="
  show Header           = "header"
  show Args             = "args"
  show Plus             = "Plus"
  show Replacing        = "Replacing"

-- | Show instance for program terms
instance Show PTerm where
  show (Var x)          = "$" ++ show x
  show (Const x)        = show x
  show (Not :> y)       = showPrefx Not y
  show (And :> y)       = showInfx And y
  show (Or :> y)        = showInfx Or y
  show (Equal :> y)     = showInfx Equal y
  show (NEqual :> y)    = showInfx NEqual y
  show (Header :> y)    = showPrefx Header y
  show (Args :> y)      = showPrefx Args y
  show (Replacing :> y) = showPrefx Replacing y

-- | Show prefix expression
showPrefx :: Symbol -> [PTerm] -> String
showPrefx x [t] = case show t of
  y@('(':_) -> show x ++ y
  y         -> show x ++ "(" ++ y ++ ")"
showPrefx x ts = show x ++ "(" ++ intercalate ", " (map show ts) ++ ")"

-- | Show infix expression
showInfx :: Symbol -> [PTerm] -> String
showInfx x y = "(" ++ intercalate (" " ++ show x ++ " ") (map show y) ++ ")"

------------------------------------------------------------------------------------------
-- Read instances

-- | Read instance for program symbols
instance Read Symbol where
  readsPrec p r = [ (X (read x), "") | ('p':x,"") <- lex r ]
               ++ [ (C (read x), "") | (x,"") <- lex r, isDigit (head x) ]
               ++ [ (S $ L.Symbol (read x), "") | ('s':x,"") <- lex r ]
               ++ [ (B True, "") | ("T","") <- lex r ]
               ++ [ (B False, "") | ("F","") <- lex r ]
               ++ [ (Plus, "") | ("Plus","") <- lex r ]

-- | Read instance for program terms
instance Read PTerm where
  readsPrec p r = [ (Var (read x), t) | ("$",s@('p':_)) <- lex r, (x,t) <- lex s ]
               ++ [ (Const (read x), s) | (x,s) <- lex r, isDigit (head x) ]
               ++ [ (Const (read x), s) | ("$",s@('s':_)) <- lex r, (x,t) <- lex s ]
               ++ [ (Not :> [x],s) | ([x],s) <- readPrefx p (show Not) r ]
               ++ [ (And :> x,s) | (x,s) <- readInfx p (show And) r ]
               ++ [ (Or :> x,s) | (x,s) <- readInfx p (show Or) r ]
               ++ [ (Equal :> x,s) | (x,s) <- readInfx p (show Equal) r ]
               ++ [ (NEqual :> x,s) | (x,s) <- readInfx p (show NEqual) r ]
               ++ [ (Header :> [x],s) | ([x],s) <- readPrefx p (show Header) r ]
               ++ [ (Args :> [x],s) | ([x],s) <- readPrefx p (show Args) r ]
               ++ [ (Replacing :> [x],s) | ([x],s) <- readPrefx p (show Replacing) r ]

-- | Read prefix expression
readPrefx :: Int -> String -> ReadS [PTerm]
readPrefx p str r = [ (x,t) | (str,s) <- lex r,
                              (x,t) <- readParen True (readsPrec p) s ]

-- | Read infix expression
readInfx :: Int -> String -> ReadS [PTerm]
readInfx p str r = [ (x:xs,t) | (x,s) <- readParen False (readsPrec p) r,
                                (xs,t) <- f p str s ]
  where
    f :: Int -> String -> ReadS [PTerm]
    f p str s = case lex s of
      [(str,u)] -> [ (x:xs,t) | (x,w) <- readParen False (readsPrec p) u,
                              (xs,t) <- f p str w ]
      _         -> [([],s)]

------------------------------------------------------------------------------------------
-- Composing functions

-- | Get a program variable with a given number
class Var a where
  var :: Int -> a
instance Var Symbol where var = X
instance Var PTerm  where var i = Var (X i)

-- | Convert a given constant to a program symbol
class Cons a b where
  cons :: a -> b
instance Cons Int  Symbol where cons = C
instance Cons Int  PTerm  where cons i = Const (C i)
instance Cons Bool Symbol where cons = B
instance Cons Bool PTerm  where cons c = Const (B c)
instance Cons L.Symbol Symbol where cons = S
instance Cons L.Symbol PTerm  where cons c = Const (S c)

-- | Negate a given program term
not :: PTerm -> PTerm
not (Equal  :> ts) = NEqual :> ts
not (NEqual :> ts) = Equal :> ts
not t              = Not :> [t]

-- | Take the logical and of a given program terms
and :: [PTerm] -> PTerm
and [t] = t
and (t:s) = And :> case (t, Program.Symbol.and s) of
  (Const (B True), y)      -> [y]
  (x@(Const (B False)), y) -> [x]
  (x, Const (B True))      -> [x]
  (x, y@(Const (B False))) -> [y]
  (And :> xs, And :> ys)   -> xs ++ ys
  (And :> xs, y)           -> xs ++ [y]
  (x, And :> ys)           -> x : ys
  (x, y)                   -> [x, y]

-- | Take the logical or of a given program terms
or :: [PTerm] -> PTerm
or [t] = t
or (t:s) = Or :> case (t, Program.Symbol.or s) of
  (x@(Const (B True)), y) -> [x]
  (Const (B False), y)    -> [y]
  (x, y@(Const (B True))) -> [y]
  (x, Const (B False))    -> [x]
  (Or :> xs, Or :> ys)    -> xs ++ ys
  (Or :> xs, y)           -> xs ++ [y]
  (x, Or :> ys)           -> x : ys
  (x, y)                  -> [x, y]

-- | Return a program term which is the equality of a given program terms
eq :: PTerm -> PTerm -> PTerm
eq x y = Equal :> [x,y]

-- | Return a program term which is the negation of equality of a given program terms
neq :: PTerm -> PTerm -> PTerm
neq x y = NEqual :> [x,y]

-- | Return a program term which is the header of a given program term
header :: PTerm -> PTerm
header t = Header :> [t]

-- | Return a program term which is the list of arguments of a given program term
args :: PTerm -> PTerm
args t = Args :> [t]

------------------------------------------------------------------------------------------
-- Evaluating functions

-- | Evaluate a program symbol
class Eval a where
  eval :: Symbol -> a

instance Eval ((Int -> Term a) -> Term a) where
  eval (X i) f = f i

instance Eval Int where
  eval (C i) = i

instance Eval Bool where
  eval (B c) = c

instance Eval L.Symbol where
  eval (S s) = s

instance Eval (Bool -> Bool) where
  eval Not = Prelude.not

instance Eval (Bool -> Bool -> Bool) where
  eval And = (&&)
  eval Or  = (||)

instance Eq a => Eval (a -> a -> Bool) where
  eval Equal  = (==)
  eval NEqual = (/=)

instance Eval (Term a -> a) where
  eval Header = Term.header

instance Eval (Term a -> [Term a]) where
  eval Args = Term.args
