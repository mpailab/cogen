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
import           Program.BuiltIn
------------------------------------------------------------------------------------------
-- Data types and clases declaration

class NameSpace m => ProgWriter m where
  getIndent :: m Int
  setIndent :: Int -> m ()
  inci :: m ()
  deci :: m ()
  inci = getIndent >>= \ind -> echo ("increase indent"++show ind) >> setIndent (ind+1)
  deci = getIndent >>= \ind -> echo ("decrease indent"++show ind) >> setIndent (ind-1)

  indent :: m String
  indent = getIndent >>= writeIndent

  indented :: (a -> m String) -> a -> m String
  indented wr x = do { inci; str <- wr x; deci; return str }
  indented0 :: m String -> m String
  indented0 wr = indented (\()->wr) ()

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
  writeI :: ProgWriter m => Bool -> a -> m String

  write :: NameSpace m => a -> m String
  write x = evalStateT wrI 0
            where wrI :: NameSpace m => SimpleWriter m String
                  wrI = writeI False x

instance Write Program where
  writeI _ = writeProgram

instance Write Expr where
  writeI _ NONE     = return "NONE"
  --writeI (Val x) = write x
  writeI _ (Ref n x)  = writeVar n +<>+ "@" +>+ writeI True x
  writeI _ (Ptr n x)  = writeVar n +<>+ "&" +>+ writeI True x

  writeI _ (Bool x) = return $ show x
  writeI par (Term t) = writeI par t
  writeI _ (Var n) = writeVar n
  writeI _ (Sym (SL s)) = nameLSymbol (SL s)
  writeI _ (Sym (IL s)) = return $ nameBuiltIn s
  writeI _ Any = return "_"
  writeI _ AnySeq = return "__"
  writeI par (Call h args) = inpars par $
                  writeI True h +<>+ "` " +>+ (unwords <$> mapM (writeI True) args)

  writeI par (IfElse cond iftrue iffalse) = inpars par $
                  "if " +>+ writeI False cond +<>+
                  " then " +>+ writeI False iftrue +<>+
                  " else " +>+ writeI True iffalse

  writeI par (CaseOf pat cases) = inpars par $
                  "case " +>+ writeI False pat +<>+ " of {" +>+
                  writeSequenceS "; " cases (\(pat,res) -> writeI False pat +<>+ " -> " +>+ writeI False res)
                  +<+ "}"

  writeI par (Alt vars) = inpars par $ writeSequenceS " | " vars write0

  writeI par (Fun args cmds) = inpars par $ "\\" +>+
                  writeSequenceS " " args (writeI True) +<>+ " -> \n" +>+ indented0 (
                    writeSequenceS "" cmds (indented write0)
                  ) +<>+ if par then indent else return ""

  writeI par (Int x)  = return (show x)
  writeI par (List x)  = "[" +>+ writeSequence x (writeI False) +<+ "]"
  writeI par (Tuple x) = "(" +>+ writeSequence x (writeI False) +<+ ")"
  writeI par (Set x)   = "{" +>+ writeSequence x (writeI False) +<+ "}"

  writeI par (Equal x y)      = inpars par $ writeI True x +<>+ " eq " +>+ writeI True y
  writeI par (NEqual x y)     = inpars par $ writeI True x +<>+ " ne " +>+ writeI True y
  writeI par (In x y)         = inpars par $ writeI True x +<>+ " in " +>+ writeI True y
  writeI par (Not x)          = inpars par $ "no " +>+ writeI True x
  writeI par (And x)          = inpars par $ writeInfx "&&" x (writeI True)
  writeI par (Or x)           = inpars par $ writeInfx "||" x (writeI True)

  -- writeI _ (Frag f) = "{<\n" +>+ indented0 (
  --                  writeSequenceS "" f (indented writeStmt)
  --                ) +<>+ indent +<+ ">}"

instance Write TExpr where
  --writeI par = writeTerm par
  writeI par (T x)     = writeI par x
  writeI par (x :> y)  = inpars par $ writeI True x +<>+
                " [" +>+ writeSequence y (writeI False) +<+ "]"
  writeI par (x :>> y) = inpars par $ writeI True x +<>+ " " +>+ writeI True y


instance Write Command where
  writeI _ (Assign Select pat (List [val]) cond) =
    indent +<>+ write0 pat +<>+ " = " +>+ write0 val +<>+ writeWhereCond cond

  writeI _ (Assign Append pat (List frags) cond) =
      indent +<>+ write0 pat +<>+ writeSequenceS "" frags (\f -> show Append +>+ write0 f) +<>+ writeWhereCond cond

  writeI _ (Assign tp pat gen cond) =
    indent +<>+ write0 pat +<>+ show tp +>+ write0 gen +<>+ writeWhereCond cond

  -- | Write a branching instruction of program fragment corresponding to a given indent
  writeI _ (Branch cond br) =
    indent +<>+ "if " +>+ write0 cond +<>+ "\n" +>+
    indent +<>+ "do\n" +>+
    writeProgTail br

  -- | Write a switching instruction of program fragment corresponding to a given indent
  writeI _ (Switch expr cond cs) =
    indent +<>+ "case " +>+ write0 expr +<>+ " of" +>+
    writeWhereCond cond +<>+
    indented writeSwitchCases cs

  -- | Write an acting instruction of program fragment corresponding to a given indent
  writeI _ (Apply act cond) =
    indent +<>+ write0 act +<>+ writeWhereCond cond

  writeI _ (Return e) =
    indent +<>+ "return " +>+ write0 e +<+ "\n"

  writeI _ (Yield e) =
    indent +<>+ "yield " +>+ write0 e +<+ "\n"

write0 :: (Write t, ProgWriter m) => t -> m String
write0 = writeI False
------------------------------------------------------------------------------------------
-- Functions

-- | Write a program variable
writeVar :: Vars m => Int -> m String
writeVar n = namePVar n >>= \case
  Just name -> return name
  _ -> error ("Unknown variable with number " ++ show n ++ "\n")

-- | Write sequence
writeSequenceS :: NameSpace m => String -> [a] -> (a -> m String) -> m String
writeSequenceS _ [] wr       = return ""
writeSequenceS _ [x] wr      = wr x
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

inpars :: Monad m => Bool -> m String -> m String
inpars par str = if par then "(" +>+ str +<+ ")"  else str


-- writeSymbol _ (Frag frag) = "{" +>+

writeIndent :: ProgWriter m => Int -> m String
writeIndent 0 = return ""
writeIndent n = writeIndent (n-1) >>= \x -> return (' ' : ' ' : x)

writeWhere :: ProgWriter m => [Expr] -> m String
writeWhere = foldr (\ t -> (+<>+) (indent +<>+ write0 t +<+ "\n")) (return "")

writeHeader :: ProgWriter m => Header -> m String
writeHeader (Header name vars) =
  indent  +<>+ writeVar name +<>+ " " +>+ (unwords <$> mapM writeVar vars) +<+ " =\n"

writeWhereCond :: ProgWriter m => Expr -> m String
writeWhereCond (Bool True) = return "\n"
writeWhereCond (And conds) = "\n" +>+
  indent +<>+
  "  where\n" +>+ indented (indented writeWhere) conds

writeWhereCond cond = " where " +>+ write0 cond +<+ "\n"

-- | Write an assigning instruction of program fragment corresponding to a given indent

writeProgTail :: ProgWriter m => [Command] -> m String
writeProgTail = foldr ((+<>+) . indented write0) (indent +<+ "done\n") -- +<>+ writeProgTail ind ss
--writeProgTail ind [] = writeIndent ind +<+ "done\n"

-- | Write a program fragment corresponding to a given indent
writeProgram :: ProgWriter m => Program -> m String
writeProgram (Program h s) = writeHeader h +<>+ writeProgTail s

writeSwitchCases :: ProgWriter m => [(Expr, Expr, [Command])] -> m String
writeSwitchCases ((pat,cond,prog):cs) =
  indent +<>+ write0 pat +<>+ indented writeWhereCond cond +<>+
  indent +<>+ "do\n" +>+
  writeProgTail prog +<>+
  writeSwitchCases cs
writeSwitchCases [] = return ""
