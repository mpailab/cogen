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
      parsePTerm,
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
import           Program.Parser
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
             | In                       -- ^ including for elements of set
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

parseVar :: ParserS PSymbol Char
parseVar = "parse a program variable" >>> \s0 db ->
  [ (X (read x), s1) | ('t':x,s1) <- lex s0, all isDigit x ]

parseInt :: ParserS PSymbol Char
parseInt = "parse an integer constant" >>> \s0 db ->
  [ (I (read x), s1) | (x@(_:_),s1) <- lex s0, all isDigit x ]

parseBool :: ParserS PSymbol Char
parseBool = "parse a bool constant" >>> \s0 db ->
  [ (B True, s1) | ("True",s1) <- lex s0 ] ++
  [ (B False, s1) | ("False",s1) <- lex s0 ]

parseLSymbol :: ParserS PSymbol Char
parseLSymbol = "parse a logical symbol" >>> \s0 db ->
  [ (S (lsymbol x db), s1) | (x@(_:_),s1) <- lex s0, isLSymbol x db ]

-- | Parse a program symbol
parsePSymbol :: ParserS PSymbol Char
parsePSymbol = parseVar +>+ parseInt +>+ parseBool +>+ parseLSymbol

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
writePSymbol :: PSymbol -> LSymbols -> String
writePSymbol (S s) db = name s db
writePSymbol s _      = show s

-- | Parser instance for program terms
instance Parser PTerm where
  parse_ = parsePTerm
  write  = writePTerm False

-- | List of keyword of program symbols
keywords :: [PSymbol]
keywords = [Not, And, Or, Equal, NEqual, In, Args, Replace]

keywordsBool :: [PSymbol]
keywordsBool = [Not, And, Or, Equal, NEqual]

keywordsAction :: [PSymbol]
keywordsAction = [Replace]

parseSequence :: String -> ParserS [PTerm] Char
parseSequence sep = "parse a sequence" >>>
  if sep == "," then f parsePTerm else f parseSingleton
  where
    f g s0 db = [ ([t], s1)  | (t, s1) <- g s0 db, (x, s2) <- lex s1, x /= sep ] ++
                [ (t:ts, s3) | (t, s1) <- g s0 db, (x, s2) <- lex s1, x == sep,
                               (ts, s3) <- parseSequence sep s2 db ]

parseSymbol :: ParserS PTerm Char
parseSymbol = "parse a symbol statement" >>> \s0 db ->
  [ (T x, s1) | (x, s1) <- parsePSymbol s0 db ]

parseTerm :: ParserS PTerm Char
parseTerm = "parse a term statement" >>> \s0 db ->
  [ (f x t, s2) | (x, s1) <- (parseVar +>+ parseLSymbol) s0 db,
                  (t, s2) <- parseSingleton s1 db ]
  where
    f x (List :> ts) = x :> ts
    f x t            = x :>> t

parseList :: ParserS PTerm Char
parseList = "parse a list statement" >>> \s0 db ->
  [ (List :> ts, s3) | ("[", s1) <- lex s0,
                       (ts@(_:_), s2) <- parseSequence "," s1 db,
                       ("]", s3) <- lex s2 ]

parseTuple :: ParserS PTerm Char
parseTuple = "parse a tuple statement" >>> \s0 db ->
  [ (t, s3) | ("(", s1) <- lex s0,
              ([t], s2) <- parseSequence "," s1 db,
              (")", s3) <- lex s2 ] ++
  [ (Tuple :> ts, s3) | ("(", s1) <- lex s0,
                        (ts@(_:_:_), s2) <- parseSequence "," s1 db,
                        (")", s3) <- lex s2 ]

parseNot :: ParserS PTerm Char
parseNot = "parse 'no' statement" >>> \s0 db ->
  [ (Not :> [t], s2) | ("no", s1) <- lex s0, (t, s2) <- parseSingleton s1 db ]

parseAnd :: ParserS PTerm Char
parseAnd = "parse 'and' statement" >>> \s0 db ->
  [ (And :> ts, s1) | (ts@(_:_:_), s1) <- parseSequence "and" s0 db ]

parseOr :: ParserS PTerm Char
parseOr = "parse 'or' statement" >>> \s0 db ->
  [ (Or :> ts, s1) | (ts@(_:_:_), s1) <- parseSequence "or" s0 db ]

parseEqual :: ParserS PTerm Char
parseEqual = "parse 'eq' statement" >>> \s0 db ->
  [ (Equal :> ts, s1) | (ts@[_,_], s1) <- parseSequence "eq" s0 db ]

parseNEqual :: ParserS PTerm Char
parseNEqual = "parse 'ne' statement" >>> \s0 db ->
  [ (NEqual :> ts, s1) | (ts@[_,_], s1) <- parseSequence "ne" s0 db ]

parseIn :: ParserS PTerm Char
parseIn = "parse 'in' statement" >>> \s0 db ->
  [ (In :> ts, s1) | (ts@[_,_], s1) <- parseSequence "in" s0 db ]

parseArgs :: ParserS PTerm Char
parseArgs = "parse 'args' statement" >>> \s0 db ->
  [ (Args :> [t], s2) | ("args", s1) <- lex s0, (t, s2) <- parseSingleton s1 db ]

parseReplace :: ParserS PTerm Char
parseReplace = "parse 'replace' statement" >>> \s0 db ->
  [ (Replace :> [t1, t2], s3) | ("replace", s1) <- lex s0,
                                (t1, s2) <- parseSingleton s1 db,
                                (t2, s3) <- parseSingleton s2 db ]

parseSingleton :: ParserS PTerm Char
parseSingleton = parseSymbol +>+ parseList +>+ parseTuple

parsePrefix :: ParserS PTerm Char
parsePrefix = parseNot +>+ parseIn +>+ parseArgs +>+ parseReplace

parseInfix :: ParserS PTerm Char
parseInfix = parseAnd +>+ parseOr +>+ parseEqual +>+ parseNEqual

parseKeyword :: ParserS PTerm Char
parseKeyword = parsePrefix +>+ parseInfix

-- | Parse a program term
parsePTerm :: ParserS PTerm Char
parsePTerm "" db = []
parsePTerm x db  = (parseKeyword +>+ parseTerm +>+ parseSingleton) x db

-- | Write sequence of program terms
writeSequence :: [PTerm] -> LSymbols -> String
writeSequence [t] db    = writePTerm False t db
writeSequence (t:ts) db = writePTerm False t db ++ ", " ++ writeSequence ts db

-- | Write prefix expression
writePrefx :: PSymbol -> [PTerm] -> LSymbols -> String
writePrefx x [t] db = writePSymbol x db ++ writePTerm True t db
writePrefx x ts db =
  writePSymbol x db ++ unwords (map (\t -> writePTerm True t db) ts)

-- | Write infix expression
writeInfx :: PSymbol -> [PTerm] -> LSymbols -> String
writeInfx x ts db =
  intercalate (" " ++ writePSymbol x db ++ " ") (map (\t -> writePTerm True t db) ts)

-- | Write a program term
writePTerm :: Bool -> PTerm -> LSymbols -> String
writePTerm par t db = let (x,y) = f t in if par && y then "(" ++ x ++ ")" else x
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
    f (In :> x)        = (writeInfx In x db, True)
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
-- Evaluating functions

------------------------------------------------------------------------------------------
-- Functions

-- | Does a program term correspond to an action
isAction :: PTerm -> Bool
isAction (x :> _) = x `elem` keywordsAction

isBool :: PTerm -> Bool
isBool (x :> _) = x `elem` keywordsBool
