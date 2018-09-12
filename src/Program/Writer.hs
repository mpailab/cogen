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
import           Expr
import           LSymbol
import           Program
import           Term
import           Utils

------------------------------------------------------------------------------------------
-- Data types and clases declaration

class Write a where
  write :: NameSpace m => a -> m String

instance Write Program where
  write = writeProgram 0

instance Write PExpr where
  write NONE     = return "NONE"
  write (Val x) = write x
  write (Bool x) = write x

instance Write PVExpr where
  write = writeVExpr False

instance Write PBool where
  write = writeBool False

instance Write PTerm where
  write = writeTerm False

instance Write PTerminal where
  write = writeSymbol

------------------------------------------------------------------------------------------
-- Functions

-- | Write a program variable
writeVar :: NameSpace m => Int -> m String
writeVar n = namePVar n >>= \case
  Just name -> return name
  _ -> error ("Unknown variable with number " ++ show n ++ "\n")

-- | Write sequence
writeSequence :: NameSpace m => [a] -> (a -> m String) -> m String
writeSequence [x] wr    = wr x
writeSequence (x:xs) wr = wr x +<>+ ", " +>+ writeSequence xs wr

-- | Write prefix expression
writePrefx :: NameSpace m => String -> [a] -> (a -> m String) -> m String
writePrefx x y wr = (x ++ " ") +>+ (unwords <$> mapM wr y)

-- | Write infix expression
writeInfx :: NameSpace m => String -> [a] -> (a -> m String) -> m String
writeInfx x y wr = intercalate (" " ++ x ++ " ") <$> mapM wr y

-- | Write a program symbol
writeSymbol :: NameSpace m => PTerminal -> m String
writeSymbol (X n) = writeVar n
writeSymbol (S s) = nameLSymbol s
writeSymbol (E e) = writeEntry False e

writeEntry :: NameSpace m => Bool -> PEntry -> m String
writeEntry par (Ref n x)  = writeVar n +<>+ "@" +>+ writeVExpr True x
writeEntry par (Ptr n x)  = writeVar n +<>+ "&" +>+ writeVExpr True x
writeEntry par (Inside x) = writeVExpr par x

writeTerm :: NameSpace m => Bool -> PTerm -> m String
writeTerm par t = let (x,y) = f t in if par && y then "(" +>+ x +<+ ")" else x
  where
    f (T x)     = (writeSymbol x, False)
    f (x :> y)  = (writeSymbol x +<>+
                  " [" +>+ writeSequence y (writeTerm False) +<+ "]", True)
    f (x :>> y) = (writeSymbol x +<>+ " " +>+ writeSymbol y, True)

writeVExpr :: NameSpace m => Bool -> PVExpr -> m String
writeVExpr par (Sym x)  = writeSymbol x
writeVExpr par (Int x)  = return (show x)
writeVExpr par (Entr x) = writeEntry par x
writeVExpr par (List x)  = "[" +>+ writeSequence x (writeVExpr False) +<+ "]"
writeVExpr par (Tuple x) = "(" +>+ writeSequence x (writeVExpr False) +<+ ")"
writeVExpr par (Set x)   = "{" +>+ writeSequence x (writeVExpr False) +<+ "}"
writeVExpr par (Term x)  = writeTerm par x

writeBool :: NameSpace m => Bool -> PBool -> m String
writeBool par t = let (x,y) = f t in if par && y then "(" +>+ x +<+ ")" else x
  where
    f (Const True)     = (return "True", False)
    f (Const False)    = (return "False", False)
    f (Equal x y)      = (writeTerm True x +<>+ " eq " +>+ writeTerm True y, True)
    f (NEqual x y)     = (writeTerm True x +<>+ " ne " +>+ writeTerm True y, True)
    f (In x y)         = (writeTerm True x +<>+ " in " +>+ writeVExpr True y, True)
    f (Not x)          = ("no " +>+ writeBool True x, True)
    f (And x)          = (writeInfx "and" x (writeBool True), True)
    f (Or x)           = (writeInfx "or" x (writeBool True), True)

writeIndent :: NameSpace m => Int -> m String
writeIndent 0 = return ""
writeIndent n = writeIndent (n-1) >>= \x -> return (' ' : ' ' : x)

writeWhere :: NameSpace m => Int -> [PBool] -> m String
writeWhere ind = foldr (\ t -> (+<>+) (writeIndent ind +<>+ write t +<+ "\n")) (return "")

-- | Write a program fragment corresponding to a given indent
writeStmt :: NameSpace m => Int -> ProgStmt -> m String

writeHeader ind (Header name vars) =
  writeIndent ind +<>+ name +>+ " " +>+ (unwords <$> mapM write vars) +<+ " =\n"

writeWhereCond :: NameSpace m => Int -> PBool -> m String
writeWhereCond ind (Const True) = return "\n"
writeWhereCond ind (And conds) = "\n" +>+
  writeIndent ind +<>+
  "  where\n" +>+ writeWhere (ind+2) conds

writeWhereCond ind cond = " where " +>+ write cond  +<+ "\n"

-- | Write an assigning instruction of program fragment corresponding to a given indent
writeStmt ind (Assign PMSelect pat (List [val]) cond) =
  writeIndent ind +<>+ write pat +<>+ " = " +>+ write val +<>+ writeWhereCond ind cond

writeStmt ind (Assign tp pat gen cond) =
  writeIndent ind +<>+ write pat +<>+ show tp +>+ write gen +<>+ writeWhereCond ind cond

-- | Write a branching instruction of program fragment corresponding to a given indent
writeStmt ind (Branch cond br) =
  writeIndent ind +<>+ "if " +>+ write cond +<>+ "\n" +>+
  writeIndent ind +<>+ "do\n" +>+
  writeProgTail ind br

-- | Write a switching instruction of program fragment corresponding to a given indent
writeStmt ind (Switch expr cond cs) =
  writeIndent ind +<>+ "case " +>+ write expr +<>+ " of" +>+
  writeWhereCond ind cond +<>+
  writeSwitchCases (ind+1) cs

-- | Write an acting instruction of program fragment corresponding to a given indent
writeStmt ind (Action act cond) =
  writeIndent ind +<>+ write act +<>+ writeWhereCond ind cond

writeProgTail :: NameSpace m => Int -> [ProgStmt] -> m String
writeProgTail ind = foldr ((+<>+) . writeStmt (ind+1)) (writeIndent ind +<+ "done\n") -- +<>+ writeProgTail ind ss
--writeProgTail ind [] = writeIndent ind +<+ "done\n"

-- | Write a program fragment corresponding to a given indent
writeProgram :: NameSpace m => Int -> Program -> m String
writeProgram ind (Program h s) = writeHeader ind h +<>+ writeProgTail ind s

writeSwitchCases :: NameSpace m => Int -> [(PVExpr, PBool, [ProgStmt])] -> m String
writeSwitchCases ind ((pat,cond,prog):cs) =
  writeIndent ind +<>+ write pat +<>+ writeWhereCond (ind+1) cond +<>+
  writeIndent ind +<>+ "do\n" +>+
  writeProgTail ind prog +<>+
  writeSwitchCases ind cs
writeSwitchCases ind [] = return ""
