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

-- | Parser type
type Parser = ParsecT Text.Text ()

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
  parse str source = runParserT programParser () source (Text.pack str) >>= \case
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
coralDef :: NameSpace m => GenLanguageDef Text.Text () m
coralDef = emptyDef
  { commentStart   = "{-"
  , commentEnd     = "-}"
  , commentLine    = "--"
  , nestedComments = True
  , identStart     = letter
  , identLetter    = alphaNum <|> oneOf "_'"
  , opStart        = opLetter coralDef
  , opLetter       = oneOf ":!#$%&*+./<=>?@\\^|-~"
  , reservedOpNames= ["=", "<-", "@", "&"]
  , reservedNames  = ["do", "done", "if", "case", "of", "where",
                      "True", "False", "no", "and", "or", "eq", "ne", "in",
                      "args", "replace"]
  , caseSensitive  = True
  }

-- | Lexer for Coral language - collection of lexical parsers for tokens
coralLexer :: NameSpace m => GenTokenParser Text.Text () m
coralLexer = makeTokenParser coralDef

-- | Lexeme parser @parensParser p@ parses @p@ enclosed in parenthesis,
-- returning the value of @p@.
parensParser :: NameSpace m => Parser m a -> Parser m a
parensParser = parens coralLexer

-- | Lexeme parser @bracesParser p@ parses @p@ enclosed in braces (\'{\'
-- and \'}\'), returning the value of @p@.
bracesParser :: NameSpace m => Parser m a -> Parser m a
bracesParser = braces coralLexer

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
intParser :: NameSpace m => Parser m Int
intParser = fromInteger <$> naturalParser

-- | Parser of symbols (logical symbols or variables)
symbolParser :: NameSpace m => Parser m PSymbol
symbolParser = identifierParser >>= \name -> getLSymbol name >>= \case
  Just s  -> return (S s)
  Nothing -> getPVar name

-- | Parser of program variables
varParser :: NameSpace m => Parser m PSymbol
varParser = identifierParser >>= \name -> getLSymbol name >>= \case
  Just s  -> unexpected ("The identifier " ++ name ++ " is reserved as logical symbol.\n")
  Nothing -> getPVar name

entryParser :: NameSpace m => Parser m PEntry
entryParser =  try ( do { X n <- varParser
                        ; reservedOpParser "&"
                        ; expr <- aggrParser
                        ; return (Ptr n expr)
                        } )
           <|> try ( do { X n <- varParser
                        ; reservedOpParser "@"
                        ; expr <- aggrParser
                        ; return (Ref n expr)
                        } )
           <?> "entry"

aggrParser :: NameSpace m => Parser m PAggr
aggrParser =  try (Comp <$> compParser)
          <|> try (Int  <$> intParser)
          <|> try (Entr <$> entryParser)
          <|> try (Sym  <$> symbolParser)
          <?> "aggregate"

termParser :: NameSpace m => Parser m PTerm
termParser =  try ( do { sym <- symbolParser
                       ; terms <- bracketsParser (commaSepParser termParser)
                       ; return (sym :> terms)
                       } )
          <|> try ( do { sym <- symbolParser
                       ; var <- varParser
                       ; return (sym :>> var)
                       } )
          <|> try ( do { sym <- symbolParser
                       ; return (T sym)
                       } )
          <?> "term"

compParser :: NameSpace m => Parser m PComp
compParser =  try (List  <$> bracketsParser (commaSepParser aggrParser))
          <|> try (Tuple <$> parensParser   (commaSepParser aggrParser))
          <|> try (Set   <$> bracesParser   (commaSepParser aggrParser))
          <|> try (Term  <$> termParser)
          <?> "composite"

relationParser :: NameSpace m => Parser m PBool
relationParser =  try ( do {
                           ; left <- termParser
                           ; reservedParser "eq"
                           ; right <- termParser
                           ; return (Equal left right)
                           } )
              <|> try ( do {
                           ; left <- termParser
                           ; reservedParser "ne"
                           ; right <- termParser
                           ; return (NEqual left right)
                           } )
              <|> try ( do {
                           ; term <- termParser
                           ; reservedParser "in"
                           ; set <- compParser
                           ; return (In term set)
                           } )
              <?> "relation"

atomBool :: NameSpace m => Parser m PBool
atomBool =  try (parensParser boolParser)
        <|> try (reservedParser "True"  >> return (Const True ))
        <|> try (reservedParser "False" >> return (Const False))
        <|> try relationParser
        <?> "atomic bool"

boolParser :: NameSpace m => Parser m PBool
boolParser = buildExpressionParser tableBool atomBool
          <?> "bool"

tableBool :: NameSpace m => [[Operator Text.Text () m PBool]]
tableBool = [ [Prefix (reservedParser "no"  >> return pNot)]
            , [Infix  (reservedParser "and" >> return pAnd) AssocRight]
            , [Infix  (reservedParser "or"  >> return pOr)  AssocRight]
            ]

-- | Parser of program expressions
exprParser :: NameSpace m => Parser m PExpr
exprParser =  try (Aggr <$> aggrParser)
          <|> try (Bool <$> boolParser)
          <?> "expr"

-- | Parser of where-statement
whereParser :: NameSpace m => Parser m PBool
whereParser =
  do { reservedParser "where"
     ; ts <- many1 (try (boolParser <* notFollowedBy (opStart coralDef)) <?> "where")
     ; return (foldr1 pAnd ts)
     }
  <|> return (Const True)

-- | Parser of do-statement
doParser :: NameSpace m => Parser m Program
doParser = reservedParser "do" >> programParser

nameParser:: NameSpace m => Parser m String
nameParser = identifierParser >>= \name -> getLSymbol name >>= \case
  Just s  -> unexpected ("The identifier " ++ name ++ " is reserved as logical symbol.\n")
  Nothing -> return name

-- | Parser of programs
programParser :: NameSpace m => Parser m Program
programParser = (reservedParser "done" >> return Program.Empty)

         <|> try ( do { n <- nameParser
                      ; vs <- many varParser
                      ; reservedOpParser "="
                      ; p <- programParser
                      ; return (Header n vs p)
                      } )


         -- Parse an assigning instruction
         <|> try ( do { p <- aggrParser
                      ; g <- (reservedOpParser "=" >> toList <$> aggrParser)
                             <|> (reservedOpParser "<-" >> aggrParser)
                      ; c <- whereParser
                      ; j <- programParser
                      ; return (Assign p g c j)
                      } )

         -- Parse an branching instruction
         <|> try ( do { reservedParser "if"
                      ; c <- boolParser
                      ; b <- doParser
                      ; j <- programParser
                      ; return (Branch c b j)
                      } )

         -- Parse an switching instruction
         <|> try ( do { reservedParser "case"
                      ; e <- aggrParser
                      ; reservedParser "of"
                      ; c <- whereParser
                      ; cs <- many1 $ liftM2 (,) aggrParser doParser
                      ; j <- programParser
                      ; return (Switch e c cs j)
                      } )

         -- Parse an acting instruction
         <|> try ( do { t <- aggrParser
                      ; c <- whereParser
                      ; j <- programParser
                      ; return (Action t c j)
                      } )

         <?> "Program"

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
