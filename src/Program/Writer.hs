{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE LambdaCase           #-}
{-# LANGUAGE TypeSynonymInstances #-}

{-|
Module      : Program.Writer
Description : Programs writer
Copyright   : (c) Grigoriy Bokov 2017-2018
License     : GPL-3
Maintainer  : bokov@intsys.msu.ru
Stability   : experimental
Portability : POSIX
-}
module Program.Writer
    (
      -- exports
      write
    )
where

-- External imports
import           Control.Monad
import           Data.List
import           Data.Maybe

-- Internal imports
import           LSymbol
import           Program
import           Utils

------------------------------------------------------------------------------------------
-- Data types and clases declaration

class Write a where
  write :: NameSpace m => a -> m String

instance Write PTerm where
  write = writePTerm False

instance Write Program where
  write = writeProgram 0

------------------------------------------------------------------------------------------
-- Functions

-- | Write a program variable
writePVar :: NameSpace m => Int -> m String
writePVar n = namePVar n >>= \case
  Just name -> return name
  _ -> error ("Unknown variable with number " ++ show n ++ "\n")

-- | Write sequence of program terms
writeSequence :: NameSpace m => [PTerm] -> m String
writeSequence [t]    = writePTerm False t
writeSequence (t:ts) = writePTerm False t +<>+ ", " +>+ writeSequence ts

-- | Write prefix expression
writePrefx :: NameSpace m => String -> [PTerm] -> m String
writePrefx x [t] = x +>+ writePTerm True t
writePrefx x ts  = x +>+ (unwords <$> mapM (writePTerm True) ts)

-- | Write infix expression
writeInfx :: NameSpace m => String -> [PTerm] -> m String
writeInfx x ts = intercalate (" " ++ x ++ " ") <$> mapM (writePTerm True) ts

-- | Write a program term
writePTerm :: NameSpace m => Bool -> PTerm -> m String
writePTerm par t = let (x,y) = f t in if par && y then "(" +>+ x +<+ ")" else x
  where
    f (X n)            = (writePVar n, False)
    f (I i)            = (return (show i), False)
    f (B True)         = (return "True", False)
    f (B False)        = (return "False", False)
    f (S s)            = (nameLSymbol s, False)
    f (Term x@(X _) y) = (writePTerm False x +<>+ " " +>+ writePTerm True y, True)
    f (Term x@(S _) y) = (writePTerm False x +<>+ " " +>+ writePTerm True y, True)
    f (List x)         = ("[" +>+ writeSequence x +<+ "]", False)
    f (Tuple x)        = ("(" +>+ writeSequence x +<+ ")", False)
    f (Not x)          = (writePrefx "no" [x], True)
    f (And x)          = (writeInfx "and" x, True)
    f (Or x)           = (writeInfx "or" x, True)
    f (Equal x y)      = (writeInfx "eq" [x,y], True)
    f (NEqual x y)     = (writeInfx "ne" [x,y], True)
    f (In x y)         = (writeInfx "in" [x,y], True)
    f (Args x)         = (writePrefx "args" [x], True)
    f (Replace x)      = (writePrefx "replace" [x], True)
    f (Ref n x)        = (writePVar n +<>+ "@" +>+ writePTerm True x, False)
    f (Ptr n x)        = (writePVar n +<>+ "&" +>+ writePTerm True x, False)

writeIndent :: NameSpace m => Int -> m String
writeIndent 0 = return ""
writeIndent n = writeIndent (n-1) >>= \x -> return (' ' : ' ' : x)

writeWhere :: NameSpace m => Int -> [PTerm] -> m String
writeWhere ind = foldr (\ t -> (+<>+) (writeIndent ind +<>+ write t +<+ "\n")) (return "")

-- | Write a program fragment corresponding to a given indent
writeProgram :: NameSpace m => Int -> Program -> m String

-- | Write an assigning instruction of program fragment corresponding to a given indent
writeProgram ind (Assign pat (List [val]) (B True) jump) =
  writeIndent ind +<>+ write pat +<>+ " = " +>+ write val +<>+ "\n" +>+
  writeProgram ind jump

writeProgram ind (Assign pat (List [val]) (And conds) jump) =
  writeIndent ind +<>+ write pat +<>+ " = " +>+ write val +<>+ "\n" +>+
  writeIndent ind +<>+
  "  where\n" +>+ writeWhere (ind+2) conds +<>+
  writeProgram ind jump

writeProgram ind (Assign pat (List [val]) cond jump) =
  writeIndent ind +<>+ write pat +<>+ " = " +>+ write val +<>+
  " where " +>+ write cond  +<>+ "\n" +>+
  writeProgram ind jump

writeProgram ind (Assign pat gen (B True) jump) =
  writeIndent ind +<>+ write pat +<>+ " <- " +>+ write gen +<>+ "\n" +>+
  writeProgram ind jump

writeProgram ind (Assign pat gen (And conds) jump) =
  writeIndent ind +<>+ write pat +<>+ " <- " +>+ write gen +<>+ "\n" +>+
  writeIndent ind +<>+ "  where\n" +>+
  writeWhere (ind+2) conds +<>+
  writeProgram ind jump

writeProgram ind (Assign pat gen cond jump) =
  writeIndent ind +<>+ write pat +<>+ " <- " +>+ write gen +<>+
  " where " +>+ write cond +<>+ "\n" +>+
  writeProgram ind jump

-- | Write a branching instruction of program fragment corresponding to a given indent
writeProgram ind (Branch cond br jump) =
  writeIndent ind +<>+ "if " +>+ write cond +<>+ "\n" +>+
  writeIndent ind +<>+ "do\n" +>+
  writeProgram (ind+1) br +<>+
  writeProgram ind jump

-- | Write a switching instruction of program fragment corresponding to a given indent
writeProgram ind (Switch expr (B True) cs jump) =
  writeIndent ind +<>+ "case " +>+ write expr +<>+ " of\n" +>+
  writeSwitchCases (ind+1) cs +<>+
  writeProgram ind jump

writeProgram ind (Switch expr (And conds) cs jump) =
  writeIndent ind +<>+ "case " +>+ write expr +<>+ " of\n" +>+
  writeIndent ind +<>+ "  where\n" +>+
  writeWhere (ind+2) conds +<>+
  writeSwitchCases (ind+1) cs +<>+
  writeProgram ind jump

writeProgram ind (Switch expr cond cs jump) =
  writeIndent ind +<>+ "case " +>+ write expr +<>+ " of\n" +>+
  " where " +>+ write cond +<>+ "\n" +>+
  writeSwitchCases (ind+1) cs +<>+
  writeProgram ind jump

-- | Write an acting instruction of program fragment corresponding to a given indent
writeProgram ind (Action act (B True) jump) =
  writeIndent ind +<>+ write act +<>+ "\n" +>+
  writeProgram ind jump

writeProgram ind (Action act (And cs) jump) =
  writeIndent ind +<>+ write act +<>+ "\n" +>+
  writeIndent ind +<>+ "  where\n" +>+
  writeWhere (ind+2) cs +<>+
  writeProgram ind jump

writeProgram ind (Action act cond jump) =
  writeIndent ind +<>+ write act +<>+
  " where " +>+ write cond +<>+ "\n" +>+
  writeProgram ind jump

-- | Write an empty program fragment corresponding to a given indent
writeProgram ind Empty =
  writeIndent ind +<+ "done\n"

writeSwitchCases :: NameSpace m => Int -> [(PTerm, Program)] -> m String
writeSwitchCases ind ((pat,prog):cs) =
  writeIndent ind +<>+ write pat +<>+ "\n" +>+
  writeIndent ind +<>+ "do\n" +>+
  writeProgram (ind+1) prog +<>+
  writeSwitchCases ind cs
writeSwitchCases ind [] = return ""
