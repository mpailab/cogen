{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase        #-}

{-|
Module      : Program.Parser
Description : Programs parser
Copyright   : (c) Grigoriy Bokov 2017-2018
License     : GPL-3
Maintainer  : bokov@intsys.msu.ru
Stability   : experimental
Portability : POSIX
-}
module Program.Parser
    (
      -- exports
      Program.Parser.parse
    )
where

-- External imports
import           Control.Monad.State
import           Data.Char
import           Data.Functor
import           Data.List
import qualified Data.Map               as Map
import           Data.Maybe
import qualified Data.Text              as Text
import           Debug.Trace
import           Text.Parsec
import           Text.Parsec.Char
import           Text.Parsec.Combinator
import           Text.Parsec.Error
import           Text.Parsec.Expr
import           Text.Parsec.Language
import           Text.Parsec.Prim
import           Text.Parsec.Text       hiding (Parser)
import           Text.Parsec.Token

-- Internal imports
import           Expr
import           LSymbol
import           Program
import           Term
import           Utils

------------------------------------------------------------------------------------------
-- Data types and classes declaration

data PState = PSt {
    inFrag    :: Bool,
    isPattern :: Bool,
    indents   :: [IndentSt],
    extvars   :: [(Int,Int)]
  }
type IndentSt = Either (Int,Int) Int

initState :: PState
initState = PSt { inFrag=False, isPattern = False, indents=[Left (0,0)], extvars=[] }

-- modifyStateInF :: NameSpace m => (PState -> PState) -> (t -> Parser m a) -> t -> Parser m a
-- modifyStateInF ch f Var = do old <- getState
--                            modifyState $ f
--                            res <- f
--                            setState old

setPmEnabledIn :: NameSpace m => Bool -> Parser m a -> Parser m a
setPmEnabledIn en f = setPmEnabledInF en (\() -> f) ()

setPmEnabledInF :: NameSpace m => Bool -> (t -> Parser m a) -> t -> Parser m a
setPmEnabledInF en f x = do old <- isPattern <$> getState
                            modifyState (\st -> st{ isPattern = en })
                            res <- f x
                            modifyState (\st -> st{ isPattern = old })
                            return res

inpattern :: NameSpace m => Parser m a -> Parser m a
inpattern = setPmEnabledIn True

nopattern :: NameSpace m => Parser m a -> Parser m a
nopattern = setPmEnabledIn False

inpatternF :: NameSpace m => (t -> Parser m a) -> t -> Parser m a
inpatternF = setPmEnabledInF True

nopatternF :: NameSpace m => (t -> Parser m a) -> t -> Parser m a
nopatternF = setPmEnabledInF False

ispattern :: NameSpace m => Parser m ()
ispattern = isPattern <$> getState >>= guard

topIndent :: NameSpace m => Parser m IndentSt
topIndent = head . indents <$> getState

pushIndent :: NameSpace m => IndentSt -> Parser m ()
pushIndent i = echo ("pushIndent "++show i) >> modifyState (\st -> let is = indents st in st {indents = i:is})

popIndent :: NameSpace m => Parser m IndentSt
popIndent = echo "popIndent" >> getState >>= (\st -> let i:is = indents st in putState st {indents = is} >> return i)

updateIndent :: NameSpace m => IndentSt -> Parser m ()
updateIndent i = echo ("updateIndent "++show i) >> modifyState (\st -> let _:is = indents st in st {indents = i:is})

getPos :: NameSpace m => Parser m (Int,Int)
getPos = getPosition >>= \pos -> return (sourceLine pos, sourceColumn pos)

dbg :: NameSpace m => String -> Parser m ()
dbg name = getPos >>= \pos -> echo (name++" "++show pos)

beginIndent :: NameSpace m => Parser m ()
beginIndent = whiteSpaceParser >> do
  pos@(cr,cc) <- getPos
  st <- getState
  case head $ indents st of
    Left (r,c) | cr == r   -> pushIndent (Left (r,c))
               | cc > c    -> updateIndent (Right cc) >> pushIndent (Left pos)
               | otherwise -> dbg ("no indent, top = L"++show (r,c)) >> parserZero
    Right c -> if cc == c then pushIndent (Left pos)
               else dbg ("no indent, top = R"++ show c) >>parserZero

endIndent :: NameSpace m => Parser m ()
endIndent = popIndent >>= \case
  Left (r,c) -> topIndent >>= \case
    Left (r1,c1) -> when (r1>r) $ updateIndent (Right c1)
    _            -> return ()
  _          -> return ()

indentBlock :: NameSpace m => Parser m x -> Parser m x
indentBlock p = try (beginIndent *> p <* endIndent)

-- | check if correct indentation
indented :: NameSpace m => Parser m ()
indented = whiteSpaceParser >> topIndent >>= \case
  Left (r,c) -> getPos >>= \(rr,cc)-> unless (rr==r || cc > c) parserZero
  Right c -> getPos >>= \(_,cc)-> unless (cc >= c) parserZero


-- | Parser type
type Parser = ParsecT Text.Text PState

-- | Parser instance for treatment of logical symbols in underlying monad
instance NameSpace m => LSymbol.Base (Parser m) where
  getLSymbols = lift getLSymbols
  setLSymbols = lift . setLSymbols

-- | Parser instance for treatment of program variables in underlying monad
instance NameSpace m => Program.Vars (Parser m) where
  getPVars = lift getPVars
  setPVars = lift . setPVars

-- | Class of parsers in underlying monad with constrained 'NameSpace'.
class Parse a where
  parse :: NameSpace m => String -> String -> m a

------------------------------------------------------------------------------------------
-- Main functions

-- | Parse instance for programs
instance Parse Program where
  parse str source = runParserT programParser initState source (Text.pack str) >>= \case
    Right p   -> return p
    Left  err -> (error . errorToString) err

-- | Parse instance for expressions
instance Parse Expr where
  parse str source = runParserT lambdaParser initState source (Text.pack str) >>= \case
    Right e   -> return e
    Left  err -> (error . errorToString) err

errorToString :: ParseError -> String
errorToString err = "Program parser error:\n"
  ++ showPos (errorPos err) ++ "\n"
  ++ (unlines . map messageString . errorMessages) err

showPos :: SourcePos -> String
showPos pos = file ++ " (line " ++ show i ++ ", column " ++ show j ++ ")"
  where
    file = sourceName pos
    i = sourceLine pos
    j = sourceColumn pos

-- | This is a minimal token definition for Coral language.
coralDef :: NameSpace m => GenLanguageDef Text.Text PState m
coralDef = emptyDef
  { commentStart   = "{-"
  , commentEnd     = "-}"
  , commentLine    = "--"
  , nestedComments = True
  , identStart     = letter
  , identLetter    = alphaNum <|> oneOf "_'"
  , opStart        = opLetter coralDef
  , opLetter       = oneOf ":!#$%&*+./<=>?@\\^|-~"
  , reservedOpNames= ["=", "<-", "@", "&&", "||", "|", "+", "++", "-", "*", "/", "&", "$", "<<", "~=", ".."]
  , reservedNames  = ["do", "done", "if", "case", "of", "where",
                      "True", "False", "no", "eq", "ne", "in",
                      "then", "else",
                      "args", "replace", "_", "__"]
  , caseSensitive  = True
  }

-- | Lexer for Coral language - collection of lexical parsers for tokens
coralLexer :: NameSpace m => GenTokenParser Text.Text PState m
coralLexer = makeTokenParser coralDef

-- | Lexeme parser @parensParser p@ parses @p@ enclosed in parenthesis,
-- returning the value of @p@.
parensParser :: NameSpace m => Parser m a -> Parser m a
parensParser p = indented >> parens coralLexer p

-- | Lexeme parser @bracesParser p@ parses @p@ enclosed in braces (\'{\'
-- and \'}\'), returning the value of @p@.
bracesParser :: NameSpace m => Parser m a -> Parser m a
bracesParser = braces coralLexer

-- | Lexeme parser @bracketsParser p@ parses @p@ enclosed in brackets (\'[\'
-- and \']\'), returning the value of @p@.
bracketsParser :: NameSpace m => Parser m a -> Parser m a
bracketsParser p = indented >> brackets coralLexer p

-- | Lexeme parser @identifierParser@ parses a legal identifier of Coral language.
-- Returns the identifier string. This parser will fail on identifiers that are reserved
-- words. Legal identifier (start) characters and reserved words are
-- defined in the 'coralDef'.
identifierParser :: NameSpace m => Parser m String
identifierParser = indented >> identifier coralLexer

-- | Lexeme parser @naturalParser@ parses a natural number (a positive whole
-- number). Returns the value of the number. The number is parsed according to the grammar
-- rules in the Haskell report.
naturalParser :: NameSpace m => Parser m Integer
naturalParser = indented >> natural coralLexer

-- | The lexeme parser @reservedOpParser name@ parses symbol @name@ which is a reserved
-- operator of Coral language. It also checks that the @name@ is not a prefix of a valid
-- operator.
reservedOpParser :: NameSpace m => String -> Parser m ()
reservedOpParser s = indented >> reservedOp coralLexer s

-- | The lexeme parser @reservedParser name@ parses symbol @name@ which is a reserved
-- identifier of Coral language. It also checks that the @name@ is not a prefix of a
-- valid identifier.
reservedParser :: NameSpace m => String -> Parser m ()
reservedParser s = indented >> reserved coralLexer s

reservedParserU :: NameSpace m => String -> Parser m ()
reservedParserU = reserved coralLexer

-- | Lexeme parser @commaSepParser p@ parses /zero/ or more occurrences of @p@ separated
-- by comma. Returns a list of values returned by @p@.
commaSepParser :: NameSpace m => Parser m a -> Parser m [a]
commaSepParser = commaSep coralLexer

-- | Lexeme parser @semiSepParser p@ parses /zero/ or more occurrences of @p@ separated
-- by semicolon. Returns a list of values returned by @p@.
semiSepParser :: NameSpace m => Parser m a -> Parser m [a]
semiSepParser = semiSep coralLexer

-- | Parses any white space. White space consists of /zero/ or more
-- occurrences of a space character (any character which satisfies isSpace), a line
-- comment or a block (multi line) comment. Block comments may be nested. How comments are
-- started and ended is defined in the 'coralDef'.
whiteSpaceParser :: NameSpace m => Parser m ()
whiteSpaceParser = whiteSpace coralLexer

funcApp :: NameSpace m => Parser m ()
funcApp = try $ char '`' >> notFollowedBy (letter <|> oneOf "[({") >> whiteSpaceParser
--  <|> do p0 <- getPos
--         whiteSpaceParser
--         p1 <- getPos
--         guard $ p0 /= p1
ident :: NameSpace m => Parser m String
ident = liftM2 (:) (identStart coralDef) (many (identLetter coralDef))

prefixFunc :: NameSpace m => Parser m Expr -> Parser m Expr
prefixFunc p =  whiteSpaceParser >> char '`' >> ((ident >>= symbolOrVar) <|> p)

infxFunc :: NameSpace m => Parser m Expr -> Parser m Expr -> Expr -> Parser m Expr
infxFunc parg pf first = do
  dbg "try infix function"
  f <- prefixFunc pf
  dbg "infix found"
  args <- option [] (funcApp >> many parg)
  whiteSpaceParser
  dbg $ "args = "++show args
  return $ Call f $ first:args

symbolOrVar :: NameSpace m => String -> Parser m Expr
symbolOrVar name = getLSymbol name >>= \case
  Just s  -> return (Sym s)
  Nothing -> getPVar name

-- | Parser of integers
intParser :: NameSpace m => Parser m Integer
intParser = fromInteger <$> naturalParser

extVarParser :: NameSpace m => Parser m Expr
extVarParser = do
  st <- getState
  guard (inFrag st)
  char '$'
  name <- identifierParser
  Var n <- getPVar name
  Var fn <- getPVar (name ++ "__")
  putState $ st { extvars = (n,fn):(extvars st) }
  return $ Var fn -- !!!!!!!!! TODO : ExtVar n

-- | Parser of symbols (logical symbols or variables)
symbolParser :: NameSpace m => Parser m Expr
symbolParser = extVarParser <|> (identifierParser >>= symbolOrVar)

terminalParser :: NameSpace m => Parser m Expr
terminalParser = symbolParser <|> fragParser <|> entryParser True

-- | Parser of program variables
varParser :: NameSpace m => Parser m Expr
varParser = identifierParser >>= \name -> getLSymbol name >>= \case
  Just s  -> unexpected ("The identifier " ++ name ++ " is reserved as logical symbol.\n")
  Nothing -> getPVar name

data PatternType
  = NoPattern
  | ElemPattern
  | InListPattern
  deriving (Eq,Show)

pattp :: PatternType -> PatternType -> PatternType
pattp NoPattern _ = NoPattern
pattp _ pt        = pt

anySeq :: NameSpace m => Parser m Expr
anySeq = ispattern >> try (
  (reservedParser "__" >> return AnySeq) <|> do {
  ; Var n <- varParser
  ; reservedOpParser "@" >> reservedParser "__"
  ; return $ Ref n AnySeq
  })

anyElem :: NameSpace m => Parser m Expr
anyElem = ispattern >> reservedParser "_" >> return Any
-- anyElemParser NoPattern = parserZero
-- anyElemParser _         = reservedParser "_" >> return AnySymbol

varPatternParser :: NameSpace m => PatternType -> Parser m Expr
varPatternParser NoPattern = varParser
varPatternParser InListPattern
  = (reservedParser "__" >> return AnySeq)
  <|> varPatternParser ElemPattern
varPatternParser ElemPattern
  = (reservedParser "_" >> return Any)
  <|> varParser

entryTailParser :: NameSpace m => Bool -> Int -> Parser m Expr
entryTailParser trm n = do
  dbg "reading entry 1"
  refptr <- (reservedOpParser "&" >> return Ptr) <|> (reservedOpParser "@" >> return Ref)
  dbg "reading entry 2"
  expr <- if trm then Term <$> termParser else vexprParser
  return (refptr n expr)


entryParser :: NameSpace m => Bool -> Parser m Expr
entryParser trm = dbg "parse entry" >> do { Var n <- varParser
                     ; entryTailParser trm n
                     } <?> "entry"

compParser :: NameSpace m => Parser m Expr
compParser =  dbg "parse comp" >> (
              (List  <$> bracketsParser (commaSepParser $ anySeq <|> vexprParser))
          <|> (Tuple <$> parensParser (commaSepParser $ anySeq <|> vexprParser))
          <|> (Set   <$> bracesParser (commaSepParser $ anySeq <|> vexprParser))
          <|> (Term  <$> (termParser <* dbg "return Term"))
          <?> "composite")

simpleVExprParser :: NameSpace m => Parser m Expr
simpleVExprParser = dbg "parse vexpr" >> (
                      Int <$> intParser
                  <|> lambdaParser
                  <|> (compParser >>= \case
                        Term (T v@(Var n)) -> entryTailParser False n <|> return v
                        Term (T t)       -> return t
                        Tuple [expr]     -> return expr  -- expression in parentheses is parsed as one-element tuple
                        expr             -> return expr
                      )
                  <?> "vexpr")

subExprParser :: NameSpace m => Bool -> Parser m Expr
subExprParser tm = if tm then Term <$> simpleTermParser else vexprParser

-- | parses case-of with cases in each line or one-line expression case Var of {pat1->res1; pat2->res2;...}
caseExprParser :: NameSpace m => Bool -> Parser m Expr
caseExprParser tm = do { reservedParser "case"
                      ; dbg "parse case-of expression"
                      ; x <- nopattern simpleVExprParser
                      ; reservedParser "of"
                      ; cases <- many1 (indentBlock oneCase)
                              <|> bracesParser (semiSepParser oneCase)
                      ; return $ CaseOf x cases
                      }
                      where oneCase = liftM2 (,) simpleVExprParser (reservedOpParser "->" >> subExprParser tm)

ifExprParser :: NameSpace m => Bool -> Parser m Expr
ifExprParser tm = do { reservedParser "if"
                     ; dbg "parse if expression"
                     ; cond <- inpattern boolParser
                     ; reservedParser "then"
                     ; iftrue <- indentBlock $ subExprParser tm
                     ; reservedParser "else"
                     ; iffalse <- indentBlock $ subExprParser tm
                     ; return $ IfElse cond iftrue iffalse
                     }

complexExprParser :: NameSpace m => Bool -> Parser m Expr
complexExprParser tm = ifExprParser tm <|> ifExprParser tm

-- | Parse arguments of function call in expression
vexprTailParser :: NameSpace m => Expr -> Parser m Expr
vexprTailParser h = (dbg "try expr `" >> funcApp >> Call h <$> many (nopattern vexprParser))
                <|> infxFunc (dbg "try infx expr arg" >> nopattern vexprParser) (dbg "try infx expr header" >> {- nopattern ??? -} vexprParser) h
                <|> return h

-- | parse VExpr, first argument means allow or not pattern-matching
atomExprParser :: NameSpace m => Parser m Expr
atomExprParser = fragParser
          <|> (simpleVExprParser >>= vexprTailParser)
          <|> (complexExprParser False >>= vexprTailParser)
          <?> "vexpr"

lambdaParser:: NameSpace m => Parser m Expr
lambdaParser = do
  string "\\"
  args <- many varParser
  reservedOpParser "->"
  cmds <- indentBlock $ many stmtParser
  return $ Fun args cmds


-- | try parse term or function call with given header
parseTermArgs :: NameSpace m => Expr -> Parser m TExpr
parseTermArgs h =
          (funcApp >> (T . Call h) <$> many (nopattern vexprParser))
      <|> T <$> infxFunc (toExpr <$> nopattern simpleTermParser) (toExpr <$> simpleTermParser) h
      <|> (h :>) <$> (dbg "try brackets" >> bracketsParser (commaSepParser (T <$> anySeq <|> termParser) <|> return []))
      <|> (h :>>) <$> (dbg "try :>> var" >> indented >> varParser)
      <|> (dbg ("return T "++show h) >> return (T h))
      where toExpr (T t) = t
            toExpr t     = Term t

simpleTermParser :: NameSpace m => Parser m TExpr
simpleTermParser = getState >>= (\st -> dbg ("parse term or header : pm = " ++ show (isPattern st))) >> (
          (dbg "th : try if" >> T <$> ifExprParser True)
      <|> (dbg "th : try case" >> T <$> caseExprParser True)
      <|> (dbg "th : try parens" >> parensParser simpleTermParser >>= \case
              e@(T (Ref _ _)) -> return e   --  already reference to term
              e@(T (Ptr _ _)) -> return e   --  already pointer to term
              T h -> parseTermArgs h    --  only header read which is symbol, conditional expression or function call
              t  -> return t            --  otherwise it is already a term
          )
      <|> do { dbg "th : try symbol"
             ; sym <- anyElem <|> symbolParser <|> (parensParser lambdaParser <* lookAhead funcApp)
             ; dbg $ "sym = "++show sym
             ; T <$> tryEntry sym <|> parseTermArgs sym
             }
    )
    where
      tryEntry (Var n) = dbg "try entry" >> entryTailParser True n
      tryEntry _       = parserZero

-- simpleTermParser :: NameSpace m => PatternType -> Parser m TExpr
-- simpleTermParser pm =  dbg "parse one term" >>
--       (   parensParser (termParser pm)
--       <|> (symbolParser >>= \sym ->
--               (T . E) <$> tryEntry sym
--           <|> (sym :>) <$> (dbg "try brackets" >> bracketsParser (commaSepParser (termParser $ pattp pm InListPattern) <|> return []))
--           <|> (sym :>>) <$> (dbg "try :>> var" >> indented >> varParser)
--           <|> (dbg ("return T "++show sym) >> return (T sym))))
--           <?> "term"
--           where
--             tryEntry (Var n) = dbg "try entry" >> entryTailParser pm True n
--             tryEntry _ = parserZero

termParser :: NameSpace m => Parser m TExpr
termParser =  dbg "parse term variants" >> do
                  first <- simpleTermParser
                  dbg $ "term : first var = "++show first
                  other <- many $ reservedOpParser "|" >> simpleTermParser
                  dbg $ "term : other vars = "++show other
                  return $ if null other then first else T (Alt $ map Term $ first:other)


relationParser :: NameSpace m => Parser m Expr
relationParser = inpattern vexprParser >>= \left ->
                  do {
                     ; rel <- choice[reservedParser "eq" >> return Equal, reservedParser "ne" >> return NEqual]
                     ; right <- inpattern vexprParser
                     ; return (rel left right)
                     }
              <|> do {
                     ; reservedParser "in"
                     ; set <- (inpattern compParser <|> varParser) -- ??? do we allow patterns in sets or lists in the right part of `in`
                     ; return (In left set)
                     }
              <|> case left of
                    (Var n) -> return (Var n)
                    _       -> parserZero
              <?> "relation"

atomBool :: NameSpace m => Parser m Expr
atomBool =  dbg "atomBool" >> (parensParser boolParser
        <|> (reservedParser "True"  >> return (Bool True ))
        <|> (reservedParser "False" >> return (Bool False))
        <|> relationParser
        <?> "atomic bool")

boolParser :: NameSpace m => Parser m Expr
boolParser = buildExpressionParser tableBool atomBool
          <?> "boolean expression expected"

tableBool :: NameSpace m => [[Operator Text.Text PState m Expr]]
tableBool = [ [Prefix (dbg "try parse no" >> reservedParser "no"  >> return pNot)]
            , [Infix  (dbg "try parse &&" >> reservedOpParser "&&" >> return pAnd) AssocRight]
            , [Infix  (dbg "try parse ||" >> reservedOpParser "||"  >> return pOr)  AssocRight]
            ]

tableExpr ::  NameSpace m => [[Operator Text.Text PState m Expr]]
tableExpr = [ [Infix  (dbg "try parse ^" >> op False "^") AssocRight]
            , [Infix  (dbg "try parse *" >> op True "*") AssocLeft
              ,Infix  (dbg "try parse /" >> op False "/") AssocLeft]
            , [Infix  (dbg "try parse +" >> op True "+") AssocLeft
              ,Infix  (dbg "try parse -" >> op False "-") AssocLeft]
            , [Infix  (dbg "try parse :" >> op False ":") AssocRight]
            , [Infix  (dbg "try parse ++" >> op True "++")  AssocRight]
            ] where op assoc name = reservedOpParser name >> makeBinOp assoc name

makeBinOp :: NameSpace m => Bool -> String -> Parser m (Expr -> Expr -> Expr)
makeBinOp assoc name = do {
  ; s <- getLSymbol name
  ; op <- case s of {Just x -> return x; _ -> parserZero <?> "unknown operator "++name}
  ; return (\x y -> Call (Sym op) (args op x++args op y))
} where args op t@(Call (Sym o) a) = if o==op then a else [t]
        args _ t                   = [t]

vexprParser :: NameSpace m => Parser m Expr
vexprParser = buildExpressionParser tableExpr atomExprParser

-- | Parser of program expressions
exprParser :: NameSpace m => Parser m Expr
exprParser = dbg "parse expr" >> (
                  try vexprParser
              <|> try boolParser
              <?> "expr")

-- | Parser of where-statement
whereParser :: NameSpace m => Bool -> Parser m Expr
whereParser ind = dbg "parse where" >> (
  do { dbg "before reservedParser where"
     ; when ind indented
     ; reservedParser "where"
     ; dbg "after reservedParser where"
     ; ts <- many1 (indentBlock (boolParser <?> "where condition"))
     ; dbg "where finished"
     ; return (foldr1 pAnd ts)
     }
  <|> return (Bool True))

-- | Parser of do-statement
doParser :: NameSpace m => Parser m [Command]
doParser = dbg "parse do" >> indentBlock (reservedParser "do"
                          >> many stmtParser)
                          <* reservedParserU "done"

fragParser :: NameSpace m => Parser m Expr
fragParser = dbg "parse fragment" >>  do
  string "{<" >> whiteSpaceParser
  st <- getState
  putState st{ inFrag=True }
  res <- many stmtParser
  string ">}" >> whiteSpaceParser
  args <- extvars <$> getState
  modifyState (\s -> s{ inFrag = inFrag st, extvars = extvars st })
  return $ Call (Fun (Var . snd <$> args) res) (Var . fst <$> args)

nameParser:: NameSpace m => Parser m String
nameParser = identifierParser >>= \name -> getLSymbol name >>= \case
  Just s  -> unexpected ("The identifier " ++ name ++ " is reserved as logical symbol.\n")
  Nothing -> return name

caseParser :: NameSpace m => Parser m [(Expr, Expr, [Command])]
caseParser = do
  pts <- many1 . indentBlock $ liftM2 (,) (inpattern vexprParser) (option (Bool True) (indentBlock $ whereParser False))
  dbg "done cases, find do"
  common <- whereParser True
  doblock <- doParser
  return $ map (\(a,c) -> (a, pAnd c common, doblock)) pts

-- | Parser of statements
stmtParser :: NameSpace m => Parser m Command
stmtParser = (dbg "parse statement " >> try (
         -- Parse an assigning instruction

         -- Parse an branching instruction
             do { -- reservedParser "if"
                ; c <- indentBlock (reservedParser "if" >> boolParser)
                ; b <- doParser
                ; return (Branch c b)
                }

         -- Parse an switching instruction
         <|> indentBlock (
             do { reservedParser "case"
                ; dbg "case found"
                ; e <- inpattern vexprParser
                ; dbg "case expression read"
                ; reservedParser "of"
                ; c <- whereParser True
                ; dbg "start read cases"
                ; cs <- mconcat <$> many1 caseParser -- liftM2 (,) vexprParser doParser
                ; return (Switch e c cs)
                }

          <|> (inpattern vexprParser >>= \p -> do{ -- ??? now we allow patterns in right part : [A, Var [_,Var [__]]] = [f [_, g[__]], B], where A,B,f,g are defined, Var must be defined by this expression
                (tp,g) <- (,) Select <$> ((reservedOpParser "=" >> toList <$> inpattern vexprParser)
                      <|> (reservedOpParser "<-" >> inpattern vexprParser))
                      <|> (,) Unord <$> (reservedOpParser "~=" >> inpattern vexprParser)
                      <|> (,) Append . List <$> many1 (reservedOpParser "<<" >> inpattern vexprParser)
                ; c <- whereParser True
                ; return (Assign tp p g c)
                } <|>
             -- Parse an acting instruction
             do { c <- whereParser True
                ; return (Apply p c)
                })
         <?> "Statement"))) <|> (dbg "no statement" >> parserZero)

-- | Program header parser
headerParser :: NameSpace m => Parser m Header
headerParser = try ( do { Var n <- varParser
              ; vs <- many varParser
              ; reservedOpParser "="
              ; return (Header n $ map (\case { Var x -> x} ) vs)
              } )

-- | Parser of programs
programParser :: NameSpace m => Parser m Program
programParser = do
  h <- headerParser
  stmts <- many stmtParser <* reservedParserU "done"
  return $ Program h stmts

------------------------------------------------------------------------------------------
-- Composing functions

-- | Convert a program term to list of terms
toList :: Expr -> Expr
toList x = List [x]

-- | Negate a given program term
pNot :: Expr -> Expr
pNot (Bool x)     = Bool (not x)
pNot (Not x)      = x
pNot (Equal x y)  = NEqual x y
pNot (NEqual x y) = Equal x y
pNot x            = Not x

-- | Take the logical and of given program terms
pAnd :: Expr -> Expr -> Expr
pAnd (Bool True) y     = y
pAnd x (Bool True)     = x
pAnd x@(Bool False) y  = x
pAnd x y@(Bool False)  = y
pAnd (And xs) (And ys) = And (xs ++ ys)
pAnd x (And ys)        = And (x : ys)
pAnd (And xs) y        = And (xs ++ [y])
pAnd x y               = And [x,y]

-- | Take the logical or of given program terms
pOr :: Expr -> Expr -> Expr
pOr x@(Bool True) y = x
pOr x y@(Bool True) = y
pOr (Bool False) y  = y
pOr x (Bool False)  = x
pOr (Or xs) (Or ys) = Or (xs ++ ys)
pOr x (Or ys)       = Or (x : ys)
pOr (Or xs) y       = Or (xs ++ [y])
pOr x y             = Or [x,y]

------------------------------------------------------------------------------------------
-- Auxiliary functions
