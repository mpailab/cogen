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
import           Data.Monoid

-- Internal imports
import           LSymbol
import           Program
import           Utils


------------------------------------------------------------------------------------------
-- Data types and clases declaration

class Write a where
  write :: NameSpace m => a -> m String

instance Write PTerm where
  write = writePTerm 0 False

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
writeSequence :: NameSpace m => Int -> [PTerm] -> m String
writeSequence ind [t]    = writePTerm ind False t
writeSequence ind (t:ts) = writePTerm ind False t +<>+ ", " +>+ writeSequence ind ts

-- | Write prefix expression
writePrefx :: NameSpace m => Int -> String -> [PTerm] -> m String
writePrefx ind x [t] = x +>+ writePTerm ind True t
writePrefx ind x ts  = x +>+ (unwords <$> mapM (writePTerm ind True) ts)

-- | Write infix expression
writeInfx :: NameSpace m => Int -> String -> [PTerm] -> m String
writeInfx ind x ts = intercalate (" " ++ x ++ " ") <$> mapM (writePTerm ind True) ts

-- | Write a program term
writePTerm :: NameSpace m => Int -> Bool -> PTerm -> m String
writePTerm ind par t = let (x,y) = f t in if par && y then "(" +>+ x +<+ ")" else x
  where
    f :: NameSpace m => PTerm -> (m String, Bool)
    f (X n)            = (writePVar n, False)
    f (I i)            = (return (show i), False)
    f (B True)         = (return "True", False)
    f (B False)        = (return "False", False)
    f (S s)            = (nameLSymbol s, False)
    f (Term x@(X _) y) = (writePTerm ind False x +<>+ " " +>+ writePTerm ind True y, True)
    f (Term x@(S _) y) = (writePTerm ind False x +<>+ " " +>+ writePTerm ind True y, True)
    f (List x)         = ("[" +>+ writeSequence ind x +<+ "]", False)
    f (Tuple x)        = ("(" +>+ writeSequence ind x +<+ ")", False)
    f (Not x)          = (writePrefx ind "no" [x], True)
    f (And x)          = (writeInfx ind "and" x, True)
    f (Or x)           = (writeInfx ind "or" x, True)
    f (Equal x y)      = (writeInfx ind "eq" [x,y], True)
    f (NEqual x y)     = (writeInfx ind "ne" [x,y], True)
    f (In x y)         = (writeInfx ind "in" [x,y], True)
    f (Args x)         = (writePrefx ind "args" [x], True)
    f (Replace x y)    = (writePrefx ind "replace" [x, y], True)
    f (Ref n x)        = (writePVar n +<>+ "@" +>+ writePTerm ind True x, False)
    f (Ptr n x)        = (writePVar n +<>+ "&" +>+ writePTerm ind True x, False)
    f (Prog p)         = ("{\n" +>+ writeStmts (ind+2) p +<>+ writeIndent ind +<+ "}", False)

writeIndent :: NameSpace m => Int -> m String
writeIndent 0 = return ""
writeIndent n = writeIndent (n-1) >>= \x -> return (' ' : ' ' : x)

writeWhere :: NameSpace m => Int -> [PTerm] -> m String
writeWhere ind = foldr (\ t -> (+<>+) (writeIndent ind +<>+ write t +<+ "\n")) (return "")

-- | Write program statement corresponding to a given indent
writeStmt :: NameSpace m => Int -> ProgStmt -> m String
-- | Write an assigning instruction of program fragment corresponding to a given indent
writeStmt ind (Assign tp pat gen cond) =
  writeIndent ind +<>+ write pat +<>+ right +<>+ scond
  where
    right = case (tp,gen) of
      (PMSelect, List [val]) -> " = " +>+ write val
      (PMSelect, _) -> " <- " +>+ write gen
      (PMUnord, _)  -> " ~= " +>+ write gen
      (PMAppend, List frs) -> foldl (+<>+) (return "") $ map (\fr -> " << " +>+ write fr) frs
    scond = case cond of
      (B True)      -> return "\n"
      (And conds)   -> "\n" +>+
        writeIndent ind +<>+ "  where\n" +>+
        writeWhere (ind+2) conds
      _             -> " where " +>+ write cond +<+ "\n"

-- | Write a branching instruction of program fragment corresponding to a given indent
writeStmt ind (Branch cond br) =
  writeIndent ind +<>+ "if " +>+ write cond +<>+ "\n" +>+
  writeIndent ind +<>+ "do\n" +>+
  writeProgram (ind+1) br -- +<>+
  --writeStmt ind jump

-- | Write a switching instruction of program fragment corresponding to a given indent
writeStmt ind (Switch expr (B True) cs) =
  writeIndent ind +<>+ "case " +>+ write expr +<>+ " of\n" +>+
  writeSwitchCases (ind+1) cs -- +<>+
  --writeStmt ind jump

writeStmt ind (Switch expr (And conds) cs) =
  writeIndent ind +<>+ "case " +>+ write expr +<>+ " of\n" +>+
  writeIndent ind +<>+ "  where\n" +>+
  writeWhere (ind+2) conds +<>+
  writeSwitchCases (ind+1) cs -- +<>+
  --writeStmt ind jump

writeStmt ind (Switch expr cond cs) =
  writeIndent ind +<>+ "case " +>+ write expr +<>+ " of\n" +>+
  " where " +>+ write cond +<>+ "\n" +>+
  writeSwitchCases (ind+1) cs -- +<>+
  --writeStmt ind jump

-- | Write an acting instruction of program fragment corresponding to a given indent
writeStmt ind (Action act (B True)) =
  writeIndent ind +<>+ write act +<+ "\n" -- +>+
  --writeStmt ind jump

writeStmt ind (Action act (And cs)) =
  writeIndent ind +<>+ write act +<>+ "\n" +>+
  writeIndent ind +<>+ "  where\n" +>+
  writeWhere (ind+2) cs -- +<>+
  --writeStmt ind jump

writeStmt ind (Action act cond) =
  writeIndent ind +<>+ write act +<>+
  " where " +>+ write cond +<+ "\n" -- +>+
  --writeStmt ind jump

-- | Write a program fragment corresponding to a given indent
writeStmts :: NameSpace m => Int -> [ProgStmt] -> m String
writeStmts ind s = foldl (+<>+) (return "") (map (writeStmt ind) s)

-- | Write a program fragment corresponding to a given indent
writeProgram :: NameSpace m => Int -> Program -> m String
writeProgram ind (Stmts s) = writeStmts ind s +<>+ writeIndent ind +<+ "done\n"

writeSwitchCases :: NameSpace m => Int -> [(PTerm, Program)] -> m String
writeSwitchCases ind ((pat,prog):cs) =
  writeIndent ind +<>+ write pat +<>+ "\n" +>+
  writeIndent ind +<>+ "do\n" +>+
  writeProgram (ind+1) prog +<>+
  writeSwitchCases ind cs
writeSwitchCases ind [] = return ""
