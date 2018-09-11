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
import           Data.List
import qualified Data.Map               as Map
import           Data.Maybe
import qualified Data.Text              as Text
import           Data.Functor
import           Text.Parsec
import           Text.Parsec.Char
import           Text.Parsec.Combinator
import           Text.Parsec.Error
import           Text.Parsec.Expr
import           Text.Parsec.Language
import           Text.Parsec.Prim
import           Text.Parsec.Text       hiding (Parser)
import           Text.Parsec.Token
import           Debug.Trace

-- Internal imports
import           Expr
import           LSymbol
import           Program
import           Term
import           Utils

------------------------------------------------------------------------------------------
-- Data types and classes declaration

data PState = PSt {
    inFrag :: Bool,
    indents :: [IndentSt]
  }
type IndentSt = Either (Int,Int) Int

initState :: PState
initState = PSt { inFrag=False, indents=[Left (0,0)] }

topIndent :: NameSpace m => Parser m IndentSt
topIndent = head . indents <$> getState

pushIndent :: NameSpace m => IndentSt -> Parser m ()
pushIndent i = traceM ("pushIndent "++show i) >> modifyState (\st -> let is = indents st in st {indents = i:is})

popIndent :: NameSpace m => Parser m IndentSt
popIndent = traceM "popIndent" >> getState >>= (\st -> let i:is = indents st in putState st {indents = is} >> return i)

updateIndent :: NameSpace m => IndentSt -> Parser m ()
updateIndent i = traceM ("updateIndent "++show i) >> modifyState (\st -> let _:is = indents st in st {indents = i:is})

getPos :: NameSpace m => Parser m (Int,Int)
getPos = getPosition >>= \pos -> return (sourceLine pos, sourceColumn pos)

dbg :: NameSpace m => String -> Parser m ()
dbg name = getPos >>= \pos -> traceM (name++" "++show pos)

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
  , reservedOpNames= ["=", "<-", "@", "&&", "||", "|", "+", "-", "*", "/", "&", "$", "<<", "~=", ".."]
  , reservedNames  = ["do", "done", "if", "case", "of", "where",
                      "True", "False", "no", "eq", "ne", "in",
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

-- | Parses any white space. White space consists of /zero/ or more
-- occurrences of a space character (any character which satisfies isSpace), a line
-- comment or a block (multi line) comment. Block comments may be nested. How comments are
-- started and ended is defined in the 'coralDef'.
whiteSpaceParser :: NameSpace m => Parser m ()
whiteSpaceParser = whiteSpace coralLexer

-- | Parser of integers
intParser :: NameSpace m => Parser m Int
intParser = fromInteger <$> naturalParser

-- | Parser of symbols (logical symbols or variables)
symbolParser :: NameSpace m => Parser m PTerminal
symbolParser = identifierParser >>= \name -> getLSymbol name >>= \case
  Just s  -> return (S s)
  Nothing -> getPVar name

terminalParser:: NameSpace m => Parser m PTerminal
terminalParser = symbolParser <|> fragParser <|> (E <$> entryParser True)

-- | Parser of program variables
varParser :: NameSpace m => Parser m PTerminal
varParser = identifierParser >>= \name -> getLSymbol name >>= \case
  Just s  -> unexpected ("The identifier " ++ name ++ " is reserved as logical symbol.\n")
  Nothing -> getPVar name

entryTailParser :: NameSpace m => Bool -> Int -> Parser m PEntry
entryTailParser trm n = do
  refptr <- (reservedOpParser "&" >> return Ptr) <|> (reservedOpParser "@" >> return Ref)
  expr <- if trm then (Comp . Term) <$> termParser else aggrParser
  return (refptr n expr)


entryParser :: NameSpace m => Bool -> Parser m PEntry
entryParser trm = dbg "parse entry" >> do { X n <- varParser
                     ; entryTailParser trm n
                     } <?> "entry"

simpleAggrParser :: NameSpace m => Parser m PAggr
simpleAggrParser =  dbg "parse aggr" >> (
              (reservedParser "_" >> return (Sym AnySymbol))
          <|> (Int  <$> intParser)
          <|> (compParser >>= \case
              Term (T s@(S _)) -> return $ Sym s
              Term (T v@(X n)) ->
                (Entr <$> entryTailParser False n) <|> return (Sym v)
              cmp -> return $ Comp cmp
              -- <* dbg "return Comp"
              )
          <?> "aggregate")


aggrParser :: NameSpace m => Parser m PAggr
aggrParser =  (Sym <$> fragParser)
          <|> simpleAggrParser
          <|> do { reservedParser "case"
                 ; x <- simpleAggrParser
                 ; reservedParser "of"
                 ; cases <- many1 (indentBlock (liftM2 (,) simpleAggrParser (reservedOpParser "->" >> aggrParser)))
                 ; return $ CaseOf (x, cases)
                 }
          <?> "aggregate"

simpleTermParser :: NameSpace m => Parser m PTerm
simpleTermParser =  dbg "parse one term" >>
      (   parensParser termParser
      <|> (symbolParser >>= \sym ->
              (T . E) <$> tryEntry sym
          <|> (sym :>) <$> (dbg "try brackets" >> bracketsParser (commaSepParser termParser <|> return []))
          <|> (sym :>>) <$> (dbg "try :>> var" >> indented >> varParser)
          <|> (dbg ("return T "++show sym) >> return (T sym))))
          <?> "term"
          where
            tryEntry (X n) = dbg "try entry" >> entryTailParser True n
            tryEntry _ = parserZero


termParser :: NameSpace m => Parser m PTerm
termParser =  dbg "parse term variants" >> do
                first <- simpleTermParser
                other <- many (reservedOpParser "|" >> simpleTermParser)
                return $ if other == [] then first else T (PV $ first:other)

compParser :: NameSpace m => Parser m PComp
compParser =  dbg "parse comp" >> ((List  <$> bracketsParser (commaSepParser aggrParser))
          <|> (Tuple <$> parensParser   (commaSepParser aggrParser))
          <|> (Set   <$> bracesParser   (commaSepParser aggrParser))
          <|> (Term  <$> (termParser <* dbg "return Term"))
          <?> "composite")

relationParser :: NameSpace m => Parser m PBool
relationParser = termParser >>= \left ->
                  do {
                     ; rel <- choice[reservedParser "eq" >> return Equal, reservedParser "ne" >> return NEqual]
                     ; right <- termParser
                     ; return (rel left right)
                     }
              <|> do {
                     ; reservedParser "in"
                     ; set <- compParser
                     ; return (In left set)
                     }
              <|> case left of
                    T (X n) -> return (BVar n) -- ^ global variable
                    _ -> parserZero
              <?> "relation"

atomBool :: NameSpace m => Parser m PBool
atomBool =  dbg "atomBool" >> (parensParser boolParser
        <|> (reservedParser "True"  >> return (Const True ))
        <|> (reservedParser "False" >> return (Const False))
        <|> relationParser
        <?> "atomic bool")

boolParser :: NameSpace m => Parser m PBool
boolParser = buildExpressionParser tableBool atomBool
          <?> "boolean expression expected"

tableBool :: NameSpace m => [[Operator Text.Text PState m PBool]]
tableBool = [ [Prefix (dbg "try parse no" >> reservedParser "no"  >> return pNot)]
            , [Infix  (dbg "try parse and" >> reservedOpParser "&&" >> return pAnd) AssocRight]
            , [Infix  (dbg "try parse or" >> reservedOpParser "||"  >> return pOr)  AssocRight]
            ]

-- | Parser of program expressions
exprParser :: NameSpace m => Parser m PExpr
exprParser =  dbg "parse expr" >> (
              try (Aggr <$> aggrParser)
          <|> try (Bool <$> boolParser)
          <?> "expr")

-- | Parser of where-statement
whereParser :: NameSpace m => Bool -> Parser m PBool
whereParser ind = dbg "parse where" >> (
  do { dbg "before reservedParser where"
     ; when ind indented
     ; reservedParser "where"
     ; dbg "after reservedParser where"
     ; ts <- many1 (indentBlock (boolParser <?> "where condition"))
     ; dbg "where finished"
     ; return (foldr1 pAnd ts)
     }
  <|> return (Const True))

-- | Parser of do-statement
doParser :: NameSpace m => Parser m [ProgStmt]
doParser = dbg "parse do" >> indentBlock (reservedParser "do"
                          >> many stmtParser)
                          <* reservedParserU "done"

fragParser :: NameSpace m => Parser m PTerminal
fragParser = dbg "parse fragment" >>  do
  string "{<"
  st <- getState
  putState st{ inFrag=True }
  res <- many stmtParser
  string ">}"
  putState st
  return $ Frag res

nameParser:: NameSpace m => Parser m String
nameParser = identifierParser >>= \name -> getLSymbol name >>= \case
  Just s  -> unexpected ("The identifier " ++ name ++ " is reserved as logical symbol.\n")
  Nothing -> return name

caseParser :: NameSpace m => Parser m [(PAggr, PBool, [ProgStmt])]
caseParser = do
  pts <- many1 . indentBlock $ liftM2 (,) aggrParser (option (Const True) (indentBlock $ whereParser False))
  dbg "done cases, find do"
  common <- whereParser True
  doblock <- doParser
  return $ map (\(a,c) -> (a, pAnd c common, doblock)) pts

-- | Parser of statements
stmtParser :: NameSpace m => Parser m ProgStmt
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
                ; e <- aggrParser
                ; dbg "case expression read"
                ; reservedParser "of"
                ; c <- whereParser True
                ; dbg "start read cases"
                ; cs <- mconcat <$> many1 caseParser -- liftM2 (,) aggrParser doParser
                ; return (Switch e c cs)
                }

          <|> (aggrParser >>= \p -> do{
                (tp,g) <- (,) PMSelect <$> ((reservedOpParser "=" >> toList <$> aggrParser)
                      <|> (reservedOpParser "<-" >> aggrParser))
                      <|> (,) PMUnord <$> (reservedOpParser "=~" >> aggrParser)
                      <|> (,) PMAppend . Comp . List <$> many1 (reservedOpParser "<<" >> aggrParser)
                ; c <- whereParser True
                ; return (Assign tp p g c)
                } <|>
             -- Parse an acting instruction
             do { c <- whereParser True
                ; return (Action p c)
                })
         <?> "Statement"))) <|> (dbg "no statement" >> parserZero)

-- | Program header parser
headerParser :: NameSpace m => Parser m Header
headerParser = try ( do { n <- nameParser
              ; vs <- many varParser
              ; reservedOpParser "="
              ; return (Header n vs)
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
toList :: PAggr -> PAggr
toList x = Comp (List [x])

-- | Negate a given program term
pNot :: PBool -> PBool
pNot (Const x)    = Const (not x)
pNot (Not x)      = x
pNot (Equal x y)  = NEqual x y
pNot (NEqual x y) = Equal x y
pNot x            = Not x

-- | Take the logical and of given program terms
pAnd :: PBool -> PBool -> PBool
pAnd (Const True) y    = y
pAnd x (Const True)    = x
pAnd x@(Const False) y = x
pAnd x y@(Const False) = y
pAnd (And xs) (And ys) = And (xs ++ ys)
pAnd x (And ys)        = And (x : ys)
pAnd (And xs) y        = And (xs ++ [y])
pAnd x y               = And [x,y]

-- | Take the logical or of given program terms
pOr :: PBool -> PBool -> PBool
pOr x@(Const True) y = x
pOr x y@(Const True) = y
pOr (Const False) y  = y
pOr x (Const False)  = x
pOr (Or xs) (Or ys)  = Or (xs ++ ys)
pOr x (Or ys)        = Or (x : ys)
pOr (Or xs) y        = Or (xs ++ [y])
pOr x y              = Or [x,y]

------------------------------------------------------------------------------------------
-- Auxiliary functions
