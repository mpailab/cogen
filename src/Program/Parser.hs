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
import           LSymbol
import           Program
import           Term
import           Utils

------------------------------------------------------------------------------------------
-- Data types and classes declaration

-- data Info = Info
--   {
--     lsymbols :: LSymbols,
--     locals   :: PVars
--   }

type Parser = ParsecT Text.Text ()

instance NameSpace m => LSymbol.Base (Parser m) where
  getLSymbols = lift getLSymbols
  setLSymbols = lift . setLSymbols

instance NameSpace m => Program.Vars (Parser m) where
  getPVars = lift getPVars
  setPVars = lift . setPVars

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
  , reservedOpNames= ["=", "<-"]
  , reservedNames  = ["do", "done", "if", "case", "of", "where"] ++ Program.symbols
  , caseSensitive  = True
  }

-- | Lexer for Coral language - collection of lexical parsers for tokens
coralLexer :: NameSpace m => GenTokenParser Text.Text () m
coralLexer = makeTokenParser coralDef

parensParser :: NameSpace m => Parser m a -> Parser m a
parensParser = parens coralLexer

bracketsParser :: NameSpace m => Parser m a -> Parser m a
bracketsParser = brackets coralLexer

identifierParser :: NameSpace m => Parser m String
identifierParser = identifier coralLexer

naturalParser :: NameSpace m => Parser m Integer
naturalParser = natural coralLexer

reservedOpParser :: NameSpace m => String -> Parser m ()
reservedOpParser = reservedOp coralLexer

reservedParser :: NameSpace m => String -> Parser m ()
reservedParser = reserved coralLexer

commaSepParser :: NameSpace m => Parser m a -> Parser m [a]
commaSepParser = commaSep coralLexer

whiteSpaceParser :: NameSpace m => Parser m ()
whiteSpaceParser = whiteSpace coralLexer

-- | Parser of integers
intParser :: NameSpace m => Parser m PSymbol
intParser = (I . fromInteger) <$> naturalParser

-- | Parser of boolean values
boolParser :: NameSpace m => Parser m PSymbol
boolParser =  (reservedParser "True"  >> return (B True))
      <|> (reservedParser "False" >> return (B False))

-- | Parser of symbols (logical symbols or variables)
symbolParser :: NameSpace m => Parser m PSymbol
symbolParser = identifierParser >>= \name -> getLSymbol name >>= \case
    Just s  -> return (S s)
    Nothing -> getPVar name

-- | Parser of atomic program terms
atomParser :: NameSpace m => Parser m PTerm
atomParser =  T <$> intParser
          <|> T <$> boolParser
          <|> T <$> symbolParser
          <|> pTuple <$> parensParser (commaSepParser termParser)
          <|> pList  <$> bracketsParser (commaSepParser termParser)
          <?> "atomic PTerm"

-- | Parser of program terms
termParser :: NameSpace m => Parser m PTerm
termParser =  try (liftM2 pTerm symbolParser atomParser)
          <|> buildExpressionParser table atomParser
          <?> "PTerm"

-- Table for parsing of composite program terms
table :: NameSpace m => [[Operator Text.Text () m PTerm]]
table = [ [Prefix (reservedParser "no" >> return pNot)]
        , [Prefix (reservedParser "args" >> return pArgs)]
        , [Prefix (reservedParser "replace" >> return pReplace)]
        , [Infix  (reservedParser "and" >> return pAnd) AssocRight]
        , [Infix  (reservedParser "or" >> return pOr) AssocRight]
        , [Infix  (reservedParser "eq" >> return pEq) AssocNone]
        , [Infix  (reservedParser "ne" >> return pNeq) AssocNone]
        , [Infix  (reservedParser "in" >> return pIn) AssocNone]
        ]

-- | Parser of where-statement
whereParser :: NameSpace m => Parser m PTerm
whereParser =
  do { reservedParser "where"
     ; ts <- many1 (try (termParser <* notFollowedBy (opStart coralDef)) <?> "where")
     ; return (foldr1 pAnd ts)
     }
  <|> return (T (B True))

-- | Parser of do-statement
doParser :: NameSpace m => Parser m Program
doParser = reservedParser "do" >> programParser

-- | Parser of programs
programParser :: NameSpace m => Parser m Program
programParser = (reservedParser "done" >> return Program.Empty)

         -- Parse an assigning instruction
         <|> do { t <- termParser
                ; p <- (reservedOpParser "=" >> toList <$> termParser)
                       <|> (reservedOpParser "<-" >> termParser)
                ; c <- whereParser
                ; j <- programParser
                ; return (Assign t p c j)
                }

         -- Parse an branching instruction
         <|> do { reservedParser "if"
                ; c <- termParser
                ; b <- doParser
                ; j <- programParser
                ; return (Branch c b j)
                }

         -- Parse an switching instruction
         <|> do { reservedParser "case"
                ; e <- termParser
                ; reservedParser "of"
                ; c <- whereParser
                ; cs <- many1 $ liftM2 (,) termParser doParser
                ; j <- programParser
                ; return (Switch e c cs j)
                }

         -- Parse an acting instruction
         <|> do { t <- termParser
                ; c <- whereParser
                ; j <- programParser
                ; return (Action t c j)
                }

         <?> "Program"

------------------------------------------------------------------------------------------
-- Composing functions

-- | Compose a program term @s t@ by symbol @s@ and term @t@
pTerm :: PSymbol -> PTerm -> PTerm
pTerm s (List :> ts) = s :> ts
pTerm s t            = s :>> t

-- | Compose a program term for tuple of terms (tuple is defined as list of terms)
pTuple :: [PTerm] -> PTerm
pTuple [t] = t
pTuple ts  = List :> ts

-- | Compose a program term for list of terms
pList :: [PTerm] -> PTerm
pList ts = List :> ts

-- | Convert a program term to list of terms
toList :: PTerm -> PTerm
toList t = List :> [t]

-- | Negate a given program term
pNot :: PTerm -> PTerm
pNot (Not    :> [t]) = t
pNot (Equal  :> ts)  = NEqual :> ts
pNot (NEqual :> ts)  = Equal :> ts
pNot t               = Not :> [t]

-- | Take the logical and of given program terms
pAnd :: PTerm -> PTerm -> PTerm
pAnd (T (B True)) y          = y
pAnd x (T (B True))          = x
pAnd x@(T (B False)) y       = x
pAnd x y@(T (B False))       = y
pAnd (And :> xs) (And :> ys) = And :> (xs ++ ys)
pAnd x (And :> ys)           = And :> (x : ys)
pAnd (And :> xs) y           = And :> (xs ++ [y])
pAnd x y                     = And :> [x,y]

-- | Take the logical or of given program terms
pOr :: PTerm -> PTerm -> PTerm
pOr x@(T (B True)) y      = x
pOr x y@(T (B True))      = y
pOr (T (B False)) y       = y
pOr x (T (B False))       = x
pOr (Or :> xs) (Or :> ys) = Or :> (xs ++ ys)
pOr x (Or :> ys)          = Or :> (x : ys)
pOr (Or :> xs) y          = Or :> (xs ++ [y])
pOr x y                   = Or :> [x,y]

-- | Return a program term which is the equality of a given program terms
pEq :: PTerm -> PTerm -> PTerm
pEq x y = Equal :> [x,y]

-- | Return a program term which is the negation of equality of a given program terms
pNeq :: PTerm -> PTerm -> PTerm
pNeq x y = NEqual :> [x,y]

-- | Return a program term which is including for elements of set
pIn :: PTerm -> PTerm -> PTerm
pIn x y = In :> [x,y]

-- | Return a program term which is the list of arguments of a given program term
pArgs :: PTerm -> PTerm
pArgs t = Args :> [t]

-- | Return a program term that represents replacing of program terms
pReplace :: PTerm -> PTerm
pReplace t = Replace :> [t]

------------------------------------------------------------------------------------------
-- Auxiliary functions
