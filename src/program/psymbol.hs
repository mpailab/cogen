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
      PSymbol, PTerm,
      readPSymbol, readPTerm,
      var, cons,
      Program.PSymbol.not,
      Program.PSymbol.and,
      Program.PSymbol.or,
      Program.PSymbol.eq,
      Program.PSymbol.neq,
      Program.PSymbol.header,
      Program.PSymbol.args,
      eval,
      isAction
    )
where

-- External imports
import           Data.Char
import           Data.List

-- Internal imports
import           LSymbol
import           Term

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
             | Args                     -- ^ arguments of term
             | Replace                  -- !
             deriving (Eq, Ord)

-- | Type of program terms
type PTerm = Term PSymbol

------------------------------------------------------------------------------------------
-- Parser instances

-- | Parser instance for program symbols
instance Parser PSymbol where
  parse_ = parsePSymbol
  write  = writePSymbol

-- | Parse a program symbol
parsePSymbol :: ParserS PSymbol
parsePSymbol str db
  =  [ (X (read x), "") | ('x':x,"") <- lex str, all isDigit x ]
  ++ [ (I (read x), "") | (x,"") <- lex str, all isDigit x ]
  ++ [ (B True, "") | ("True","") <- lex str ]
  ++ [ (B False, "") | ("False","") <- lex str ]
  ++ [ (S (lsymbol x db), "") | (x,"") <- lex str, isLSymbol x db ]

-- | Show instance for program symbols
instance Show PSymbol where
  show (X i)     = 'x' : show i
  show (I i)     = show i
  show (B True)  = "True"
  show (B False) = "False"
  show Not       = "no"
  show And       = "and"
  show Or        = "or"
  show Equal     = "eq"
  show NEqual    = "ne"
  show Args      = "args"
  show Replace   = "replace"

-- | Write a program symbol
writePSymbol :: PSymbol -> LSymbols -> String
writePSymbol (S s) db = name s db
writePSymbol s _ = show s

-- | Parser instance for program terms
instance Parser PTerm where
  parse_ = parsePTerm
  write  = writePTerm

-- | List of keyword of program symbols
keywords :: [String]
keywords = map show [Not, And, Or, Equal, NEqual, Args, Replace]

isKeyword :: String -> Bool
isKeyword x = elem x keywords

isVariable :: String -> Bool
isVariable ('p':x) = all isDigit x
isVariable ('_':x) = all isAlphaNum x

isConstant :: String -> Bool
isConstant "True"  = True
isConstant "False" = True
isConstant x       = all isDigit x

-- | Does a string correspond to a program symbol
isPSymbol :: String -> LSymbols -> Bool
isPSymbol x db = isKeyword x or isVariable x or isConstant x or isLSymbol x db

parseBlock :: ParserS String
parseBlock s0 db
  =  [ ("", s0) | s1 <- skip " *\n" s0 ]
  ++ [ ("", s0) | (x,s1) <- lex s0, x == ")" or x == "]" or x == "," or isKeyword x ]

parseSequence :: String -> ParserS [PTerm]
parseSequence sep s0 db
  =  [ ([t], s1) | (t, s1) <- parsesPTerm (skip "\\s*" s0) db,
                   (x, s2) <- lex s1, x \= sep ]
  ++ [ (t:ts, s3) | (t, s1) <- parsePTerm (skip "\\s*" s0) db,
                    (x, s2) <- lex s1, x == sep,
                    (ts, s3) <- parseSequence sep (skip "\\s*" s2) db ]

parsePSymbol :: ParserS PTerm
parsePSymbol s0 db
  =  [ (T x, s1) | (x,s1) <- parsePSymbol s0 db,
                   ("",_) <- parseBlock s1 db ]

parseVariable :: ParserS PTerm
parseVariable so db
  =  [ (x :> ts, s1) | (x@(X _),s1) <- parsePSymbol s0 db,
       (ts,s2) <- parseList s1 db,
       ("",_) <- parseBlock s2 db ]
  ++ [ (x :>> t, s1) | (x@(X _),s1) <- parsePSymbol s0 db,
       (t,s2) <- parsePTerm s1 db,
       ("",_) <- parseBlock s2 db ]

parseLSymbol :: ParserS PTerm
parseLSymbol so db
  =  [ (x :> ts, s1) | (x@(S _),s1) <- parsePSymbol s0 db,
                       (ts,s2) <- parseList s1 db,
                       ("",_) <- parseBlock s2 db ]
  ++ [ (x :>> t, s1) | (x@(S _),s1) <- parsePSymbol s0 db,
                       (t,s2) <- parsePTerm s1 db,
                       ("",_) <- parseBlock s2 db ]

parseList :: ParserS [PTerm]
parseList s0 db
  =  [ (ts, s3) | ("[", s1) <- lex s0,
                  (ts, s2) <- parseSequence "," s1 db,
                  ("]", s3) <- lex s2 ]

parseTuple :: ParserS PTerm
parseTuple s0 db
  =  [ (t, s3) | ("(", s1) <- lex s0,
                 ([t], s2) <- parseSequence "," s1 db,
                 (")", s3) <- lex s2 ]
  ++ [ (Tuple :> ts, s3) | ("(", s1) <- lex s0,
                           (ts, s2) <- parseSequence "," s1 db,
                           (")", s3) <- lex s2 ]

parseNot :: ParserS PTerm
parseNot s0 db
  = [ (Not :> [t], s2) | ("no", s1) <- lex s0,
                         (t,s2) <- parsePTerm s1 db ]

parseAnd :: Bool -> Bool -> ParserS PTerm
parseAnd s0 db
  = [ (And :> ts, s1) | (ts, s1) <- parseSequence "and" s0 db ]

parseOr :: Bool -> Bool -> ParserS PTerm
parseOr s0 db
  = [ (Or :> ts, s1) | (ts, s1) <- parseSequence "or" s0 db ]

parseEqual :: Bool -> Bool -> ParserS PTerm
parseEqual s0 db
  = [ (Equal :> ts, s1) | (ts@[_,_], s1) <- parseSequence "eq" s0 db ]

parseNEqual :: Bool -> Bool -> ParserS PTerm
parseNEqual s0 db
  = [ (NEqual :> ts, s1) | (ts@[_,_], s1) <- parseSequence "ne" s0 db ]

parseArgs :: Bool -> Bool -> ParserS PTerm
parseArgs s0 db
  = [ (Args :> [t], s2) | ("args", s1) <- lex s0,
                          (t,s2) <- parsePTerm s1 db ]

parseReplace :: Bool -> Bool -> ParserS PTerm
parseReplace s0 db
  = [ (Replace :> [t1, t2], s3) | ("replace", s1) <- lex s0,
                                  (t1,s2) <- parsePTerm s1 db,
                                  (t2,s3) <- parsePTerm s2 db ]

-- | Parse a program term
parsePTerm :: Bool -> ParserS PTerm
parsePTerm par s0 db
  =  parsePSymbol s0 db
  ++ parseVariable s0 db
  ++ parseLSymbol s0 db
  ++ parseList s0 db
  ++ parseTuple s0 db
  ++ parseNot s0 db
  ++ parseAnd s0 db
  ++ parseOr s0 db
  ++ parseEqual s0 db
  ++ parseNEqual s0 db
  ++ parseArgs s0 db
  ++ parseReplace s0 db

-- | Write sequence of program terms
writeSequence :: [PTerm] -> LSymbols -> String
writeSequence [t] db = writePTerm False t db
writeSequence (t:ts) db = writePTerm False t db ++ ", " ++ writeSequence ts db

-- | Write prefix expression
writePrefx :: PSymbol -> [PTerm] -> LSymbols -> String
writePrefx x [t] db = writePSymbol x db ++ writePTerm True t db
writePrefx x ts db =
  writePSymbol x db ++ intercalate " " (map (\t -> writePTerm True t db) ts)

-- | Write infix expression
writeInfx :: PSymbol -> [PTerm] -> LSymbols -> String
writeInfx x ts db =
  intercalate (" " ++ writePSymbol x db ++ " ") (map (\t -> writePTerm True t db) ts)

-- | Write a program term
writePTerm :: Bool -> PTerm -> LSymbols -> String
writePTerm par t db = if par and snd s@(f t) then "(" ++ s ++ ")" else s
  where
    f (T x)            = (writePSymbol x db, False)
    f (x@(X _) :> y)   = (writePSymbol x db ++ " [" ++ writeSequence y db ++ "]", True)
    f (x@(X _) :>> y)  = (writePSymbol x db ++ " " ++ writePTerm True y db, True)
    f (x@(S _) :> y)   = (writePSymbol x db ++ " [" ++ writeSequence y db ++ "]", True)
    f (x@(S _) :>> y)  = (writePSymbol x db ++ " " ++ writePTerm True y db, True)
    f (List :> x)      = ("[" ++ writeSequence x db ++ "]", False)
    f (Tuple :> x)     = ("(" ++ writeSequence x db ++ ")", False)
    f (Not :> x)       = (writePrefx Not x db, True)
    f (And :> x)       = (writeInfx And x db, True)
    f (Or :> x)        = (writeInfx Or x db, True)
    f (Equal :> x)     = (writeInfx Equal x db, True)
    f (NEqual :> x)    = (writeInfx NEqual x db, True)
    f (Args :> x)      = (writePrefx Args x db, True)
    f (Replace :> x)   = (writePrefx Replace x db, True)

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
instance VarArgs (PTerm -> a) where (<>) f ts t = group f t:ts

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
and :: [PTerm] -> PTerm
and [] = T (B True)
and [t] = t
and (t:s) = And :> case (t, Program.PSymbol.and s) of
  (T (B True), y)        -> [y]
  (x@(T (B False)), y)   -> [x]
  (x, T (B True))        -> [x]
  (x, y@(T (B False)))   -> [y]
  (And :> xs, And :> ys) -> xs ++ ys
  (And :> xs, y)         -> xs ++ [y]
  (x, And :> ys)         -> x : ys
  (x, y)                 -> [x, y]

-- | Take the logical or of a given program terms
or :: [PTerm] -> PTerm
or [t] = t
or (t:s) = Or :> case (t, Program.PSymbol.or s) of
  (x@(T (B True)), y)  -> [x]
  (T (B False), y)     -> [y]
  (x, y@(T (B True)))  -> [y]
  (x, T (B False))     -> [x]
  (Or :> xs, Or :> ys) -> xs ++ ys
  (Or :> xs, y)        -> xs ++ [y]
  (x, Or :> ys)        -> x : ys
  (x, y)               -> [x, y]

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
-- Evaluating functions

-- | Evaluate a program symbol
class Eval a where
  eval :: PSymbol -> a

instance Eval ((Int -> Term a) -> Term a) where
  eval (X i) f = f i

instance Eval Int where
  eval (X i) = i
  eval (I i) = i

instance Eval Bool where
  eval (B c) = c

instance Eval LSymbol where
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

------------------------------------------------------------------------------------------
-- Functions

-- | Does a program term correspond to an action
isAction :: PTerm -> Bool
isAction (Replacing :> _) = True
isAction t                = False
