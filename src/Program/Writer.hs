{-# LANGUAGE FlexibleInstances    #-}
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

-- Internal imports
import           LSymbol
import           Program
import           Term
import           Utils

------------------------------------------------------------------------------------------
-- Data types and clases declaration

class Write a where
  write :: LSymbol.Base m => a -> m String

instance Write PSymbol where
  write = writePSymbol

instance Write PTerm where
  write = writePTerm False

instance Write Program where
  write = writeProgram 0

------------------------------------------------------------------------------------------
-- Functions

-- | Write a program symbol
writePSymbol :: LSymbol.Base m => PSymbol -> m String
writePSymbol (S s) = nameLSymbol s
writePSymbol s     = return (show s)

-- | Write sequence of program terms
writeSequence :: LSymbol.Base m => [PTerm] -> m String
writeSequence [t]    = writePTerm False t
writeSequence (t:ts) = writePTerm False t +<>+ ", " +>+ writeSequence ts

-- | Write prefix expression
writePrefx :: LSymbol.Base m => PSymbol -> [PTerm] -> m String
writePrefx x [t] = write x +<>+ writePTerm True t
writePrefx x ts  =  write x +<>+ (unwords <$> mapM (writePTerm True) ts)

-- | Write infix expression
writeInfx :: LSymbol.Base m => PSymbol -> [PTerm] -> m String
writeInfx x ts = liftM2 intercalate (" " +>+ write x +<+ " ")
                                    (mapM (writePTerm True) ts)

-- | Write a program term
writePTerm :: LSymbol.Base m => Bool -> PTerm -> m String
writePTerm par t = let (x,y) = f t in if par && y then "(" +>+ x +<+ ")" else x
  where
    f (T x)           = (write x, False)
    f (x@(X _) :> y)  = (write x +<>+ " [" +>+ writeSequence y +<+ "]", True)
    f (x@(X _) :>> y) = (write x +<>+ " " +>+ writePTerm True y, True)
    f (x@(S _) :> y)  = (write x +<>+ " [" +>+ writeSequence y +<+ "]", True)
    f (x@(S _) :>> y) = (write x +<>+ " " +>+ writePTerm True y, True)
    f (List :> x)     = ("[" +>+ writeSequence x +<+ "]", False)
    f (Tuple :> x)    = ("(" +>+ writeSequence x +<+ ")", False)
    f (Not :> x)      = (writePrefx Not x, True)
    f (And :> x)      = (writeInfx And x, True)
    f (Or :> x)       = (writeInfx Or x, True)
    f (Equal :> x)    = (writeInfx Equal x, True)
    f (NEqual :> x)   = (writeInfx NEqual x, True)
    f (In :> x)       = (writeInfx In x, True)
    f (Args :> x)     = (writePrefx Args x, True)
    f (Replace :> x)  = (writePrefx Replace x, True)

writeIndent :: LSymbol.Base m => Int -> m String
writeIndent 0 = return ""
writeIndent n = writeIndent (n-1) >>= \x -> return (' ' : ' ' : x)

writeWhere :: LSymbol.Base m => Int -> [PTerm] -> m String
writeWhere ind = foldr (\ t -> (+<>+) (writeIndent ind +<>+ write t +<+ "\n")) (return "")

-- | Write a program fragment corresponding to a given indent
writeProgram :: LSymbol.Base m => Int -> Program -> m String

-- | Write an assigning instruction of program fragment corresponding to a given indent
writeProgram ind (Assign pat (List :> [val]) (T (B True)) jump) =
  writeIndent ind +<>+ write pat +<>+ " = " +>+ write val +<>+ "\n" +>+
  writeProgram ind jump

writeProgram ind (Assign pat (List :> [val]) (And :> conds) jump) =
  writeIndent ind +<>+ write pat +<>+ " = " +>+ write val +<>+ "\n" +>+
  writeIndent ind +<>+
  "  where\n" +>+ writeWhere (ind+2) conds +<>+
  writeProgram ind jump

writeProgram ind (Assign pat (List :> [val]) cond jump) =
  writeIndent ind +<>+ write pat +<>+ " = " +>+ write val +<>+
  " where " +>+ write cond  +<>+ "\n" +>+
  writeProgram ind jump

writeProgram ind (Assign pat gen (T (B True)) jump) =
  writeIndent ind +<>+ write pat +<>+ " <- " +>+ write gen +<>+ "\n" +>+
  writeProgram ind jump

writeProgram ind (Assign pat gen (And :> conds) jump) =
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
writeProgram ind (Switch expr (T (B True)) cs jump) =
  writeIndent ind +<>+ "case " +>+ write expr +<>+ " of\n" +>+
  writeSwitchCases (ind+1) cs +<>+
  writeProgram ind jump

writeProgram ind (Switch expr (And :> conds) cs jump) =
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
writeProgram ind (Action act (T (B True)) jump) =
  writeIndent ind +<>+ write act +<>+ "\n" +>+
  writeProgram ind jump

writeProgram ind (Action act (And :> cs) jump) =
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

writeSwitchCases :: LSymbol.Base m => Int -> [(PTerm, Program)] -> m String
writeSwitchCases ind ((pat,prog):cs) =
  writeIndent ind +<>+ write pat +<>+ "\n" +>+
  writeIndent ind +<>+ "do\n" +>+
  writeProgram (ind+1) prog +<>+
  writeSwitchCases ind cs
writeSwitchCases ind [] = return ""
