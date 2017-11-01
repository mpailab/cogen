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
      PSymbol (B, List, And), PTerm,
      parsePTerm,
      var, cons,
      Program.PSymbol.list, list',
      Program.PSymbol.not,
      Program.PSymbol.and, and',
      Program.PSymbol.or, or',
      Program.PSymbol.eq,
      Program.PSymbol.neq,
      Program.PSymbol.args,
      eval,
      isAction, isBool
    )
where

-- External imports
import           Data.Char
import           Data.List
import           Data.Map       ((!))
import qualified Data.Map       as M
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
parseVar s0 db = [ (X (read x), s1) | ('t':x,s1) <- lex s0, all isDigit x ]

parseInt :: ParserS PSymbol Char
parseInt s0 db = [ (I (read x), s1) | (x@(_:_),s1) <- lex s0, all isDigit x ]

parseConstPSymbol :: ParserS PSymbol Char
parseConstPSymbol s0 db
  =  [ (I (read x), s1) | (x@(_:_),s1) <- lex s0, all isDigit x ]
  ++ [ (B True, s1) | ("True",s1) <- lex s0 ]
  ++ [ (B False, s1) | ("False",s1) <- lex s0 ]

parseVarPSymbol :: ParserS PSymbol Char
parseVarPSymbol s0 db
  =  [ (X (read x), s1) | ('t':x,s1) <- lex s0, all isDigit x ]
  ++ [ (S (lsymbol x db), s1) | (x@(_:_),s1) <- lex s0, isLSymbol x db ]

-- | Parse a program symbol
parsePSymbol :: ParserS PSymbol Char
parsePSymbol = parseConstPSymbol +++ parseVarPSymbol

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

keywordsInfix :: [PSymbol]
keywordsInfix = [And, Or, Equal, NEqual, In]

keywordsPrefix :: [PSymbol]
keywordsPrefix = [Not, Args, Replace]

keywordsM :: M.Map String PSymbol
keywordsM = M.fromList $ map (\x -> (show x, x)) keywords

keywordsInfixM :: M.Map String PSymbol
keywordsInfixM = M.fromList $ map (\x -> (show x, x)) keywordsInfix

toKeyword :: String -> PSymbol
toKeyword x = keywordsM ! x

isInfixKeyword :: String -> Bool
isInfixKeyword x = isJust $ M.lookup x keywordsInfixM

isKeyword :: String -> Bool
isKeyword x = isJust $ M.lookup x keywordsM

isVariable :: String -> Bool
isVariable ('t':x) = all isDigit x
isVariable ('_':x) = all isAlphaNum x

isConstant :: String -> Bool
isConstant "True"  = True
isConstant "False" = True
isConstant x       = all isDigit x

-- | Does a string correspond to a program symbol
isPSymbol :: String -> LSymbols -> Bool
isPSymbol x db = isKeyword x || isVariable x || isConstant x || isLSymbol x db

parseBlock :: ParserS String Char
parseBlock s0 db
  =  [ ("", s0) | s1 <- skip "[[:space:]]*" s0, null s1 ]
  ++ [ ("", s0) | s1@(_:_) <- skip "[[:space:]]*" s0,
                  (x,s2) <- lex s1,
                  (x == ")" || x == "]" || x == "," || isInfixKeyword x) ]

parseSequence :: Bool -> String -> ParserS [PTerm] Char
parseSequence par sep s0 db
  =  [ ([t], s2) | s1 <- skip "[[:space:]]*" s0,
                   (t, s2) <- parsePTerm_ par s1 db,
                   (x, s3) <- lex s2, x /= sep ]
  ++ [ (t:ts, s4) | s1 <- skip "[[:space:]]*" s0,
                    (t, s2) <- parsePTerm_ par s1 db,
                    (x, s3) <- lex s2, x == sep,
                    (ts, s4) <- parseSequence par sep s3 db ]

parseSymbol :: ParserS PTerm Char
parseSymbol s0 db
  = [ (T x, s1) | (x, s1) <- parsePSymbol s0 db, ("", _) <- parseBlock s1 db ]

parseVariable :: Bool -> ParserS PTerm Char
parseVariable par s0 db
  = ([ (f x t, s3) | (x@(X _), s1) <- parsePSymbol s0 db,
                     (t, s2) <- parsePTerm_ True s1 db,
                     ("", s3) <- parseBlock s2 db ])
  where
    f x (List :> ts) = x :> ts
    f x t            = x :>> t

parseLSymbol :: Bool -> ParserS PTerm Char
parseLSymbol par s0 db = if par
  then []
  else if length q > 1 then error ("(3) " ++ s0) else q
    where
      q = ([ (f x t, s3) | (x@(S _), s1) <- parsePSymbol s0 db,
                         (t, s2) <- parsePTerm_ True s1 db,
                         ("", s3) <- parseBlock s2 db ])
      f x (List :> ts) = x :> ts
      f x t            = x :>> t

parseList :: ParserS PTerm Char
parseList s0 db
  =  if length q > 1 then error ("(4) " ++ s0) else q
    where
      q = [ (List :> ts, s3) | ("[", s1) <- lex s0,
                          (ts, s2) <- parseSequence False "," s1 db,
                          ("]", s3) <- lex s2 ]

parseTuple :: ParserS PTerm Char
parseTuple s0 db
  =  if length q > 1 then error ("(5) " ++ s0) else q
    where
      q = ([ (t, s3) | ("(", s1) <- lex s0,
                 ([t], s2) <- parseSequence False "," s1 db,
                 (")", s3) <- lex s2 ] ++ [ (Tuple :> ts, s3) | ("(", s1) <- lex s0,
                           (ts, s2) <- parseSequence False "," s1 db,
                           (")", s3) <- lex s2 ])

parseNot :: Bool -> ParserS PTerm Char
parseNot par s0 db = if par
  then []
  else if length q > 1 then error ("(6) " ++ s0) else q
    where
      q = [ (Not :> [t], s2) | ("no", s1) <- lex s0,
                            (t, s2) <- parsePTerm_ True s1 db ]

parseAnd :: Bool -> ParserS PTerm Char
parseAnd par s0 db = if par
  then []
  else if length q > 1 then error ("(7) " ++ s0) else q
    where
      q = [ (And :> ts, s1) | (ts@(_:_:_), s1) <- parseSequence True "and" s0 db ]

parseOr :: Bool -> ParserS PTerm Char
parseOr par s0 db = if par
  then []
  else if length q > 1 then error ("(8) " ++ s0) else q
    where
      q = [ (Or :> ts, s1) | (ts@(_:_:_), s1) <- parseSequence True "or" s0 db ]

parseEqual :: Bool -> ParserS PTerm Char
parseEqual par s0 db = if par
  then []
  else if length q > 1 then error ("(9) " ++ s0) else q
    where
      q = [ (Equal :> ts, s1) | (ts@[_,_], s1) <- parseSequence True "eq" s0 db ]

parseNEqual :: Bool -> ParserS PTerm Char
parseNEqual par s0 db = if par
  then []
  else if length q > 1 then error ("(10) " ++ s0) else q
    where
      q = [ (NEqual :> ts, s1) | (ts@[_,_], s1) <- parseSequence True "ne" s0 db ]

parseIn :: Bool -> ParserS PTerm Char
parseIn par s0 db = if par
  then []
  else if length q > 1 then error ("(11) " ++ s0) else q
    where
      q = [ (In :> ts, s1) | (ts@[_,_], s1) <- parseSequence True "in" s0 db ]

parseArgs :: Bool -> ParserS PTerm Char
parseArgs par s0 db = if par
  then []
  else if length q > 1 then error ("(12) " ++ s0) else q
    where
      q = [ (Args :> [t], s2) | ("args", s1) <- lex s0,
                             (t, s2) <- parsePTerm_ True s1 db ]

parseReplace :: Bool -> ParserS PTerm Char
parseReplace par s0 db = if par
  then []
  else if length q > 1 then error ("(13) " ++ s0) else q
    where
      q = [ (Replace :> [t1, t2], s3) | ("replace", s1) <- lex s0,
                                     (t1, s2) <- parsePTerm_ True s1 db,
                                     (t2, s3) <- parsePTerm_ True s2 db ]

-- | Parse a program term
parsePTerm_ :: Bool -> ParserS PTerm Char
parsePTerm_ par "" db = []
parsePTerm_ par s0 db = if length q1 + length q2 + length q3 + length q4 + length q5 + length q6 + length q7 + length q8 + length q9 + length q10 + length q11 + length q12 + length q13 > 1 then error (show s0 ++ "\n1: " ++ show (length q1) ++ "\n2: " ++ show (length q2) ++ "\n3: " ++ show (length q3) ++ "\n4: " ++ show (length q4) ++ "\n5: " ++ show (length q5) ++ "\n6: " ++ show (length q6) ++ "\n7: " ++ show (length q7) ++ "\n8: " ++ show (length q8) ++ "\n9: " ++ show (length q9) ++ "\n10: " ++ show (length q10) ++ "\n11: " ++ show (length q11) ++ "\n12: " ++ show (length q12) ++ "\n13: " ++ show (length q13)) else q1 ++ q2 ++ q3 ++ q4 ++ q5 ++ q6 ++ q7 ++ q8 ++ q9 ++ q10 ++ q11 ++ q12 ++ q13
  where
    q1 = parseSymbol s0 db
    q2 = parseVariable par s0 db
    q3 = parseLSymbol par s0 db
    q4 = parseList s0 db
    q5 = parseTuple s0 db
    q6 = parseNot par s0 db
    q7 = parseAnd par s0 db
    q8 = parseOr par s0 db
    q9 = parseEqual par s0 db
    q10 = parseNEqual par s0 db
    q11 = parseIn par s0 db
    q12 = parseArgs par s0 db
    q13 = parseReplace par s0 db

-- | Parse a program term
parsePTerm :: ParserS PTerm Char
parsePTerm = parsePTerm_ False

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

instance Eval ([Term a] -> Term a) where
  eval Tuple [x] = x

instance Eval ([Term a] -> (Term a, Term a)) where
  eval Tuple [x1,x2] = (x1,x2)

instance Eval ([Term a] -> (Term a, Term a, Term a)) where
  eval Tuple [x1,x2,x3] = (x1,x2,x3)

instance Eval ([Term a] -> (Term a, Term a, Term a, Term a)) where
  eval Tuple [x1,x2,x3,x4] = (x1,x2,x3,x4)

instance Eval ([Term a] -> (Term a, Term a, Term a, Term a, Term a)) where
  eval Tuple [x1,x2,x3,x4,x5] = (x1,x2,x3,x4,x5)

instance Eval (Bool -> Bool) where
  eval Not = Prelude.not

instance Eval (Bool -> Bool -> Bool) where
  eval And = (&&)
  eval Or  = (||)

instance Eq a => Eval (a -> a -> Bool) where
  eval Equal  = (==)
  eval NEqual = (/=)

instance Eval (Term a -> [Term a]) where
  eval Args = Term.args

------------------------------------------------------------------------------------------
-- Functions

-- | Does a program term correspond to an action
isAction :: PTerm -> Bool
isAction (x :> _) = x `elem` keywordsAction

isBool :: PTerm -> Bool
isBool (x :> _) = x `elem` keywordsBool
