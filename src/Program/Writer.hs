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
      write,
      ProgWriter
    )
where

-- External imports
import           Control.Monad
import           Control.Monad.State
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

class NameSpace m => ProgWriter m where
  getIndent :: m Int
  setIndent :: Int -> m ()
  inci :: m ()
  deci :: m ()
  inci = getIndent >>= \ind -> (echo ("increase indent"++show ind) >> setIndent (ind+1))
  deci = getIndent >>= \ind -> (echo ("decrease indent"++show ind) >> setIndent (ind-1))

  indent :: m String
  indent = getIndent >>= writeIndent

  indented :: (a -> m String) -> a -> m String
  indented wr x = do { inci; str <- wr x; deci; return str }

type SimpleWriter m = StateT Int m

instance Program.Vars m => Program.Vars (SimpleWriter m) where
  getPVars = lift getPVars
  setPVars _ = return ()

instance LSymbol.Base m => LSymbol.Base (SimpleWriter m) where
    getLSymbols = lift getLSymbols
    setLSymbols _ = return ()

instance NameSpace m => NameSpace (SimpleWriter m)

instance NameSpace m => ProgWriter (SimpleWriter m) where
  getIndent = get
  setIndent = put

class Write a where
  writeI :: ProgWriter m => a -> m String

  write :: NameSpace m => a -> m String
  write x = evalStateT wrI 0 >>= return
            where wrI :: NameSpace m => SimpleWriter m String
                  wrI = writeI x

instance Write Program where
  writeI = writeProgram

instance Write PExpr where
  writeI NONE     = return "NONE"
  writeI (Val x) = write x
  writeI (Bool x) = write x

instance Write PVExpr where
  writeI = writeVExpr False

instance Write PBool where
  writeI = writeBool False

instance Write PTerm where
  writeI = writeTerm False

instance Write PTerminal where
  writeI = writeSymbol False

------------------------------------------------------------------------------------------
-- Functions

-- | Write a program variable
writeVar :: Vars m => Int -> m String
writeVar n = namePVar n >>= \case
  Just name -> return name
  _ -> error ("Unknown variable with number " ++ show n ++ "\n")

-- | Write sequence
writeSequenceS :: NameSpace m => String -> [a] -> (a -> m String) -> m String
writeSequenceS _ [] wr = return ""
writeSequenceS _ [x] wr    = wr x
writeSequenceS sep (x:xs) wr = wr x +<>+ sep +>+ writeSequenceS sep xs wr

-- | Write sequence
writeSequence :: NameSpace m => [a] -> (a -> m String) -> m String
writeSequence = writeSequenceS ", "

-- | Write prefix expression
writePrefx :: NameSpace m => String -> [a] -> (a -> m String) -> m String
writePrefx x y wr = (x ++ " ") +>+ (unwords <$> mapM wr y)

-- | Write infix expression
writeInfx :: NameSpace m => String -> [a] -> (a -> m String) -> m String
writeInfx x y wr = intercalate (" " ++ x ++ " ") <$> mapM wr y

inpars par str = if par then "(" +>+ str +<+ ")"  else str

-- | Write a program symbol
writeSymbol :: ProgWriter m => Bool -> PTerminal -> m String
writeSymbol _ (X n) = writeVar n
writeSymbol _ (ExtVar n) = "$" +>+ writeVar n
writeSymbol _ (S s) = nameLSymbol s
writeSymbol _ (E e) = writeEntry False e
writeSymbol _ AnySymbol = return "_"
writeSymbol _ AnySequence = return "__"
writeSymbol par (FunCall (Sym t) (List args)) = inpars par $
                writeSymbol True t +<>+ "` " +>+ (unwords <$> mapM (writeVExpr True) args)

writeSymbol par (IfElse cond iftrue iffalse) = inpars par $
                "if " +>+ writeBool False cond +<>+
                " then " +>+ writeVExpr False iftrue +<>+
                " else " +>+ writeVExpr True iffalse

writeSymbol par (CaseOf pat cases) = inpars par $
                "case " +>+ writeVExpr False pat +<>+ " of {" +>+
                writeSequenceS "; " cases (\(pat,res) -> writeVExpr False pat +<>+ " -> " +>+ writeVExpr False res)
                +<+ "}"

writeSymbol par (PV vars) = inpars par $ writeSequenceS " | " vars writeI

-- writeSymbol _ (Frag frag) = "{" +>+

writeEntry :: ProgWriter m => Bool -> PEntry -> m String
writeEntry par (Ref n x)  = writeVar n +<>+ "@" +>+ writeVExpr True x
writeEntry par (Ptr n x)  = writeVar n +<>+ "&" +>+ writeVExpr True x
writeEntry par (Inside x) = writeVExpr par x

writeTerm :: ProgWriter m => Bool -> PTerm -> m String
writeTerm par t = let (x,y) = f t in if par && y then "(" +>+ x +<+ ")" else x
  where
    f (T x)     = (writeSymbol par x, False)
    f (x :> y)  = (writeSymbol True x +<>+
                  " [" +>+ writeSequence y (writeTerm False) +<+ "]", True)
    f (x :>> y) = (writeSymbol True x +<>+ " " +>+ writeSymbol True y, True)

writeVExpr :: ProgWriter m => Bool -> PVExpr -> m String
writeVExpr par (Sym x) = writeSymbol par x
writeVExpr par (Int x)  = return (show x)
writeVExpr par (Entr x) = writeEntry par x
writeVExpr par (List x)  = "[" +>+ writeSequence x (writeVExpr False) +<+ "]"
writeVExpr par (Tuple x) = "(" +>+ writeSequence x (writeVExpr False) +<+ ")"
writeVExpr par (Set x)   = "{" +>+ writeSequence x (writeVExpr False) +<+ "}"
writeVExpr par (Term x)  = writeTerm par x

writeBool :: ProgWriter m => Bool -> PBool -> m String
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

writeIndent :: ProgWriter m => Int -> m String
writeIndent 0 = return ""
writeIndent n = writeIndent (n-1) >>= \x -> return (' ' : ' ' : x)

writeWhere :: ProgWriter m => [PBool] -> m String
writeWhere = foldr (\ t -> (+<>+) (indent +<>+ writeI t +<+ "\n")) (return "")

writeHeader :: ProgWriter m => Header -> m String
writeHeader (Header name vars) =
  indent  +<>+ name +>+ " " +>+ (unwords <$> mapM writeI vars) +<+ " =\n"

writeWhereCond :: ProgWriter m => PBool -> m String
writeWhereCond (Const True) = return "\n"
writeWhereCond (And conds) = "\n" +>+
  indent +<>+
  "  where\n" +>+ indented (indented writeWhere) conds

writeWhereCond cond = " where " +>+ writeI cond +<+ "\n"

-- | Write a program fragment
writeStmt :: ProgWriter m => ProgStmt -> m String

-- | Write an assigning instruction of program fragment corresponding to a given indent
writeStmt (Assign PMSelect pat (List [val]) cond) =
  indent +<>+ writeI pat +<>+ " = " +>+ writeI val +<>+ writeWhereCond cond

writeStmt (Assign tp pat gen cond) =
  indent +<>+ writeI pat +<>+ show tp +>+ writeI gen +<>+ writeWhereCond cond

-- | Write a branching instruction of program fragment corresponding to a given indent
writeStmt (Branch cond br) =
  indent +<>+ "if " +>+ writeI cond +<>+ "\n" +>+
  indent +<>+ "do\n" +>+
  writeProgTail br

-- | Write a switching instruction of program fragment corresponding to a given indent
writeStmt (Switch expr cond cs) =
  indent +<>+ "case " +>+ writeI expr +<>+ " of" +>+
  writeWhereCond cond +<>+
  indented writeSwitchCases cs

-- | Write an acting instruction of program fragment corresponding to a given indent
writeStmt (Action act cond) =
  indent +<>+ writeI act +<>+ writeWhereCond cond

writeProgTail :: ProgWriter m => [ProgStmt] -> m String
writeProgTail = foldr ((+<>+) . indented writeStmt) (indent +<+ "done\n") -- +<>+ writeProgTail ind ss
--writeProgTail ind [] = writeIndent ind +<+ "done\n"

-- | Write a program fragment corresponding to a given indent
writeProgram :: ProgWriter m => Program -> m String
writeProgram (Program h s) = writeHeader h +<>+ writeProgTail s

writeSwitchCases :: ProgWriter m => [(PVExpr, PBool, [ProgStmt])] -> m String
writeSwitchCases ((pat,cond,prog):cs) =
  indent +<>+ writeI pat +<>+ indented writeWhereCond cond +<>+
  indent +<>+ "do\n" +>+
  writeProgTail prog +<>+
  writeSwitchCases cs
writeSwitchCases [] = return ""
