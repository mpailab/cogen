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
  =  [ (X (read x), "") | ('p':x,"") <- lex str, all isDigit x ]
  ++ [ (I (read x), "") | (x,"") <- lex str, all isDigit x ]
  ++ [ (B True, "") | ("True","") <- lex str ]
  ++ [ (B False, "") | ("False","") <- lex str ]
  ++ [ (S (lsymbol x db), "") | (x,"") <- lex str, isLSymbol x db ]

-- | Write a program symbol
writePSymbol :: PSymbol -> LSymbols -> String
writePSymbol (X i) _       = 'p' : show i
writePSymbol (I i) _       = show i
writePSymbol (B True) _    = "True"
writePSymbol (B False) _   = "False"
writePSymbol (S s) db      = name s db
writePSymbol (Not) _       = "no"
writePSymbol (And) _       = "and"
writePSymbol (Or) _        = "or"
writePSymbol (Equal) _     = "eq"
writePSymbol (NEqual) _    = "ne"
writePSymbol (Args) _      = "args"
writePSymbol (Replace) _   = "replace"

-- | Parser instance for program terms
instance Parser PTerm where
  parse_ = parsePTerm
  write  = writePTerm

-- | Parse a program term
parsePTerm :: ParserS PTerm
parsePTerm s0 db
  =  [ (T (parse x), s1) | (x,s1) <- lex s0, isPSymbol x db ]
  ++ [ (Not :> [x], s1) | ([x],s1) <- parsePrefx (write Not) s0 db ]
  ++ [ (And :> x, s1) | (x,s1) <- parseInfx (write And) s0 db ]
  ++ [ (Or :> x, s1) | (x,s1) <- parseInfx (write Or) s0 db ]
  ++ [ (Equal :> x, s1) | (x,s1) <- parseInfx2 (write Equal) s0 db ]
  ++ [ (NEqual :> x, s1) | (x,s1) <- parseInfx2 (write NEqual) s0 db ]
  ++ [ (Args :> [x], s1) | ([x],s1) <- parsePrefx (write Args) s0 db ]
  ++ [ (Replace :> [x], s1) | ([x],s1) <- parsePrefx (write Replace) s0 db ]

parsePTerms :: Bool -> Bool -> ParserS [PTerm]
parsePTerms par whole s0 db
  =  [ ([t], s1) |
       (t, s1) <- parsesPTerm False True (skip "\\s*" s0) db,
       (")", s2) <- lex s1 ]
  ++ [ ([t], s1) |
       (t, s1) <- parsePTerm False True (skip "\\s*" s0) db,
       ("]", s2) <- lex s1 ]
  ++ [ (t:ts, s3) |
       (t, s1) <- parsePTerm False True (skip "\\s*" s0) db,
       (",", s2) <- lex s1,
       (ts, s3) <- parsePTerms par whole (skip "\\s*" s2) db ]

parseList :: Bool -> Bool -> ParserS [PTerm]
parseList par whole s0 db
  =  [ (ts, s3) |
       ("[", s1) <- lex s0,
       (ts, s2) <- parsePTerms par whole s1 db,
       ("]", s3) <- lex s2 ]

parseTuple :: Bool -> Bool -> ParserS PTerm
parseTuple par whole s0 db
  =  [ (Tuple :> ts, s3) |
       ("(", s1) <- lex s0,
       (ts, s2) <- parsePTerms par whole s1 db,
       (")", s3) <- lex s2 ]

-- | Parse prefix expression
parsePrefx :: Bool -> Bool -> ParserS PTerm
parsePrefx par whole s0 db
  =  [ (Not :> [t], s2) | ("no", s1) <- lex s0,
                          (t,s2) <- parsePTerm True True s1 db ]
  ++ [ (Args :> [t], s2) | ("args", s1) <- lex s0,
                           (t,s2) <- parsePTerm True True s1 db ]
  ++ [ (Replace :> [t1, t2], s3) | ("replace", s1) <- lex s0,
                                   (t1,s2) <- parsePTerm True True s1 db,
                                   (t2,s3) <- parsePTerm True True s2 db ]

parseInfxName :: String -> Bool -> Bool -> ParserS [PTerm]
parseInfxName name par whole s0 db
  =  [ (t:ts, s3) | (x, s1) <- lex s0, x == name,
                    (t, s2) <- parsePTerm True True s1 db,
                    (ts, s3) <- parseInfxName name True True s2 db ]
  ++ [ ([], s0) | (x, s1) <- lex s0, x \= name ]

-- | Parse infix expression
parseInfx :: Bool -> Bool -> ParserS PTerm
parseInfx par whole s0 db
  =  [ (And :> t:ts, s2) | (t, s1) <- parsePTerm True True s0 db,
                           ("and", _) <- lex s1,
                           (ts, s2) <- parseInfxName "and" True True s1 db ]
  ++ [ (Or :> t:ts, s2) | (t, s1) <- parsePTerm True True s0 db,
                          ("or", _) <- lex s1,
                          (ts, s2) <- parseInfxName "or" True True s1 db ]
  ++ [ (Equal :> [t1,t2], s2) | (t1, s1) <- parsePTerm True True s0 db,
                                ("eq", _) <- lex s1,
                                ([t2], s2) <- parseInfxName "eq" True True s1 db ]
  ++ [ (NEqual :> [t1,t2], s2) | (t1, s1) <- parsePTerm True True s0 db,
                                 ("ne", _) <- lex s1,
                                 ([t2], s2) <- parseInfxName "ne" True True s1 db ]

-- | Parse infix expression with two arguments
parseInfx2 :: String -> ParserS [PTerm]
parseInfx2 infx s0 = [ (x:xs,t) | (x,s) <- parseParen False (parsesPrec p) r,
      (xs,t) <- f p str s ]

-- | Write a program term
writePTerm :: PTerm -> LSymbols -> String
writePTerm (T x, db)            = write x db
writePTerm (Not :> x, db)       = writePrefx Not x db
writePTerm (And :> x, db)       = writeInfx And x db
writePTerm (Or :> x, db)        = writeInfx Or x db
writePTerm (Equal :> x, db)     = writeInfx Equal x db
writePTerm (NEqual :> x, db)    = writeInfx NEqual x db
writePTerm (Args :> x, db)      = writePrefx Args x db
writePTerm (Replace :> x, db)   = writePrefx Replace x db

-- | Show prefix expression
writePrefx :: PSymbol -> [PTerm] -> LSymbols -> String
writePrefx x [t] db = case write t db of
  y@('(':_) -> write x db ++ y
  y         -> write x db ++ "(" ++ y ++ ")"
writePrefx x ts db =
  write x db ++ "[" ++ intercalate ", " (map (\t -> write t db) ts) ++ "]"

-- | Show infix expression
writeInfx :: PSymbol -> [PTerm] -> LSymbols -> String
writeInfx x ts db =
  "(" ++ intercalate (" " ++ write x db ++ " ") (map (\t -> write t db) ts) ++ ")"

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

-- | Does a string correspond to a program symbol
isPSymbol :: String -> Bool
isPSymbol "T"     = True
isPSymbol "F"     = True
isPSymbol ('p':s) = all isDigit s
isPSymbol s       = all isDigit s || isLSymbol s

-- | Does a program term correspond to an action
isAction :: PTerm -> Bool
isAction (Replacing :> _) = True
isAction t                = False
