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

-- Internal imports
import           LSymbol
import           Program
import           Utils

------------------------------------------------------------------------------------------
-- Data types and classes declaration

type PState = Bool
initState :: PState
initState = False

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
  , reservedOpNames= ["=", "<-", "@", "&", "$", "<<", "~=", "_", "__", ".."]
  , reservedNames  = ["do", "done", "if", "case", "of", "where",
                      "True", "False", "no", "and", "or", "eq", "ne", "in",
                      "args", "replace"]
  , caseSensitive  = True
  }

-- | Lexer for Coral language - collection of lexical parsers for tokens
coralLexer :: NameSpace m => GenTokenParser Text.Text PState m
coralLexer = makeTokenParser coralDef

-- | Lexeme parser @parensParser p@ parses @p@ enclosed in parenthesis,
-- returning the value of @p@.
parensParser :: NameSpace m => Parser m a -> Parser m a
parensParser = parens coralLexer

-- | Lexeme parser @bracketsParser p@ parses @p@ enclosed in brackets (\'[\'
-- and \']\'), returning the value of @p@.
bracketsParser :: NameSpace m => Parser m a -> Parser m a
bracketsParser = brackets coralLexer

-- | Lexeme parser @identifierParser@ parses a legal identifier of Coral language.
-- Returns the identifier string. This parser will fail on identifiers that are reserved
-- words. Legal identifier (start) characters and reserved words are
-- defined in the 'coralDef'.
identifierParser :: NameSpace m => Parser m String
identifierParser = identifier coralLexer

-- | Lexeme parser @naturalParser@ parses a natural number (a positive whole
-- number). Returns the value of the number. The number is parsed according to the grammar
-- rules in the Haskell report.
naturalParser :: NameSpace m => Parser m Integer
naturalParser = natural coralLexer

-- | The lexeme parser @reservedOpParser name@ parses symbol @name@ which is a reserved
-- operator of Coral language. It also checks that the @name@ is not a prefix of a valid
-- operator.
reservedOpParser :: NameSpace m => String -> Parser m ()
reservedOpParser = reservedOp coralLexer

-- | The lexeme parser @reservedParser name@ parses symbol @name@ which is a reserved
-- identifier of Coral language. It also checks that the @name@ is not a prefix of a
-- valid identifier.
reservedParser :: NameSpace m => String -> Parser m ()
reservedParser = reserved coralLexer

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
intParser :: NameSpace m => Parser m PTerm
intParser = (I . fromInteger) <$> naturalParser

-- | Parser of boolean values
boolParser :: NameSpace m => Parser m PTerm
boolParser =  (reservedParser "True"  >> return (B True))
          <|> (reservedParser "False" >> return (B False))

-- | Parser of symbols (logical symbols or variables)
symbolParser :: NameSpace m => Parser m PTerm
symbolParser = identifierParser >>= \name -> getLSymbol name >>= \case
  Just s  -> return (S s)
  Nothing -> getPVar name

-- | Parser of program variables
varParser :: NameSpace m => Parser m PTerm
varParser = identifierParser >>= \name -> getLSymbol name >>= \case
  Just s  -> unexpected ("The identifier " ++ name ++ " is reserved as logical symbol.\n")
  Nothing -> getPVar name

-- | Parser of variable references in program fragments
varRefParser :: NameSpace m => Parser m PTerm
varRefParser = getState >>= \case
  True ->
    reservedOpParser "$" *> identifierParser >>= \name -> getPVarIfExist name >>= \case
    Just s  -> return s
    Nothing -> unexpected ("Variable " ++ name ++ " not in scope.\n")
  False -> unexpected "Variable reference outside of program fragment.\n"

-- | Parser of tuples of program terms
tupleParser :: NameSpace m => Parser m PTerm
tupleParser = do
  ts <- parensParser (commaSepParser termParser)
  case ts of
    [t] -> return t
    _   -> return (Tuple ts)

-- | Parser of lists of program terms
listParser :: NameSpace m => Parser m PTerm
listParser = List <$> bracketsParser (commaSepParser termParser)

-- | Parser of references to program terms
refParser :: NameSpace m => Parser m PTerm
refParser = do
  X n <- try (varParser <* reservedOpParser "@") <?> "ref to PTerm"
  t <- termParser
  return (Ref n t)

-- | Parser of pointers to program terms
ptrParser :: NameSpace m => Parser m PTerm
ptrParser = do
  X n <- try (varParser <* reservedOpParser "&") <?> "ptr to PTerm"
  t <- termParser
  return (Ptr n t)

-- | Parser of atomic program terms
atomParser :: NameSpace m => Parser m PTerm
atomParser =  intParser
          <|> boolParser
          <|> varRefParser
          <|> refParser
          <|> ptrParser
          <|> symbolParser
          <|> tupleParser
          <|> listParser
          <|> (reservedOpParser "_" $> Underscore)
          <?> "atomic PTerm"

-- | Parser of program terms
termParser :: NameSpace m => Parser m PTerm
termParser =  try (liftM2 Term symbolParser atomParser)
          <|> buildExpressionParser table atomParser
          <?> "PTerm"

-- Table for parsing of composite program terms
table :: NameSpace m => [[Operator Text.Text PState m PTerm]]
table = [ [Prefix (reservedParser "no" >> return pNot)]
        , [Prefix (reservedParser "args" >> return Args)]
        , [Prefix (reservedParser "replace" >> termParser >>= \x -> return (Replace x))]
        , [Infix  (reservedParser "and" >> return pAnd) AssocRight]
        , [Infix  (reservedParser "or" >> return pOr) AssocRight]
        , [Infix  (reservedParser "eq" >> return Equal) AssocNone]
        , [Infix  (reservedParser "ne" >> return NEqual) AssocNone]
        , [Infix  (reservedParser "in" >> return In) AssocNone]
        ]

-- | Parser of where-statement
whereParser :: NameSpace m => Parser m PTerm
whereParser =
  do { reservedParser "where"
     ; ts <- many1 (try (termParser <* notFollowedBy (opStart coralDef)) <?> "where")
     ; return (foldr1 pAnd ts)
     }
  <|> return (B True)

-- | Parser of do-statement
doParser :: NameSpace m => Parser m Program
doParser = reservedParser "do" >> programParser

fragParser :: NameSpace m => Parser m PTerm
fragParser = do
  string "{"
  st <- getState
  putState True
  res <- many stmtParser
  string "}"
  putState st
  return $ Prog res

-- | Parser of statements
stmtParser :: NameSpace m => Parser m ProgStmt
stmtParser =
         -- Parse an assigning instruction
             do { p <- termParser
                ; (tp,g) <- (,) PMSelect <$> ((reservedOpParser "=" >> toList <$> termParser)
                       <|> (reservedOpParser "<-" >> termParser))
                       <|> (,) PMUnord <$> (reservedOpParser "=~" >> termParser)
                       <|> (,) PMAppend . List <$> many1 (reservedOpParser "<<" >> termParser)
                ; c <- whereParser
                ; return (Assign tp p g c)
                }

         -- Parse an branching instruction
         <|> do { reservedParser "if"
                ; c <- termParser
                ; b <- doParser
                ; return (Branch c b)
                }

         -- Parse an switching instruction
         <|> do { reservedParser "case"
                ; e <- termParser
                ; reservedParser "of"
                ; c <- whereParser
                ; cs <- many1 $ liftM2 (,) termParser doParser
                ; return (Switch e c cs)
                }

         -- Parse an acting instruction
         <|> do { t <- termParser
                ; c <- whereParser
                ; return (Action t c)
                }
         <?> "Statement"

-- | Parser of programs
programParser :: NameSpace m => Parser m Program
programParser =
         (many stmtParser <* reservedParser "done" >>= \l -> return $ Stmts l)
         <?> "Program"

------------------------------------------------------------------------------------------
-- Composing functions

-- | Convert a program term to list of terms
toList :: PTerm -> PTerm
toList x = List [x]

-- | Negate a given program term
pNot :: PTerm -> PTerm
pNot (Not x)      = x
pNot (Equal x y)  = NEqual x y
pNot (NEqual x y) = Equal x y
pNot x            = Not x

-- | Take the logical and of given program terms
pAnd :: PTerm -> PTerm -> PTerm
pAnd (B True) y        = y
pAnd x (B True)        = x
pAnd x@(B False) y     = x
pAnd x y@(B False)     = y
pAnd (And xs) (And ys) = And (xs ++ ys)
pAnd x (And ys)        = And (x : ys)
pAnd (And xs) y        = And (xs ++ [y])
pAnd x y               = And [x,y]

-- | Take the logical or of given program terms
pOr :: PTerm -> PTerm -> PTerm
pOr x@(B True) y    = x
pOr x y@(B True)    = y
pOr (B False) y     = y
pOr x (B False)     = x
pOr (Or xs) (Or ys) = Or (xs ++ ys)
pOr x (Or ys)       = Or (x : ys)
pOr (Or xs) y       = Or (xs ++ [y])
pOr x y             = Or [x,y]

------------------------------------------------------------------------------------------
-- Auxiliary functions
