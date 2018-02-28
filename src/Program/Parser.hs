{-# LANGUAGE FlexibleInstances #-}

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
import qualified Data.Map               as M
import           Data.Maybe
import qualified Data.Text              as Text
import           Text.Parsec
import           Text.Parsec.Char
import           Text.Parsec.Combinator
import           Text.Parsec.Error
import           Text.Parsec.Prim
import           Text.Parsec.Text       hiding (Parser)

-- Internal imports
import           LSymbol
import           Program
import           Term
import           Utils

------------------------------------------------------------------------------------------
-- Data types and clases declaration

data Info = Info
  {
    lsymbols :: LSymbols,
    locals   :: M.Map String Int,
    varnum   :: Int
  }

type Parser = GenParser Info

instance LSymbol.Base Parser where
  getLSymbols = lsymbols <$> getState
  setLSymbols db = modifyState (\info -> info { lsymbols = db })

class Parse a where
  parse :: LSymbol.Base m => String -> String -> m a

-- | Parse instance for program symbols
instance Parse PSymbol where
  parse str source = getLSymbols >>= \db ->
    case runParser parsePSymbol (Info db M.empty 1) source (Text.pack str) of
      Right s   -> return s
      Left  err -> (error . errorToString) err

-- | Parse instance for program terms
instance Parse PTerm where
  parse str source = getLSymbols >>= \db ->
    case runParser parsePTerm (Info db M.empty 1) source (Text.pack str) of
      Right t   -> return t
      Left  err -> (error . errorToString) err

-- | Parse instance for programs
instance Parse Program where
  parse str source = getLSymbols >>= \db ->
    case runParser parseProgram (Info db M.empty 1) source (Text.pack str) of
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

------------------------------------------------------------------------------------------
-- Functions

word :: Parser String
word = many1 (choice [alphaNum, oneOf "_-"]) >>= \x -> echo x >> return x

addVar :: String -> Info -> Info
addVar name info =
  let n = varnum info
      l = locals info
  in info {varnum = n + 1, locals = M.insert name n l}

parseVar :: Parser PSymbol
parseVar = do
  getPosition >>= echo . (\x -> "parseVar" ++ " : " ++ x) . showPos
  w <- lookAhead word
  echo ("word \"" ++ w ++ "\" is " ++ (if isKeyword w then "" else "not ") ++ "keyword")
  if isKeyword w
    then parserZero
    else do
      name <- word
      echo "1"
      info <- getState
      case M.lookup name (locals info) of
        Just n -> return (X n)
        _      -> modifyState (addVar name) >> return (X (varnum info))

parseInt :: Parser PSymbol
parseInt = getPosition >>= echo . (\x -> "parseInt" ++ " : " ++ x) . showPos >>
  (I . read) <$> many1 digit

parseBool :: Parser PSymbol
parseBool = getPosition >>= echo . (\x -> "parseBool" ++ " : " ++ x) . showPos >>
  (I . read) <$> (string "True" <|> string "False")

parseLSymbol :: Parser PSymbol
parseLSymbol = do
  getPosition >>= echo . (\x -> "parseLSymbol" ++ " : " ++ x) . showPos
  w <- lookAhead word
  is_sym <- checkLSymbol w
  echo ("word \"" ++ w ++ "\" is " ++ (if is_sym then "" else "not ") ++ "lsymbol")
  if is_sym
    then (S . fromJust) <$> (word >>= getLSymbol)
    else parserZero

-- | Parse a program symbol
parsePSymbol :: Parser PSymbol
parsePSymbol = getPosition >>= echo . (\x -> "parsePSymbol" ++ " : " ++ x) . showPos >>
  (try parseInt <|> try parseBool <|> try parseLSymbol <|> try parseVar <?> "PSymbol")

parseSymbol :: Parser PTerm
parseSymbol = getPosition >>= echo . (\x -> "parseSymbol" ++ " : " ++ x) . showPos >>
  T <$> parsePSymbol

parseTerm :: Parser PTerm
parseTerm = getPosition >>= echo . (\x -> "parseTerm" ++ " : " ++ x) . showPos >>
  do
    s <- try parseLSymbol <|> try parseVar <?> "Term"
    t <- parseToken
    case t of
      List :> ts -> return (s :> ts)
      _          -> return (s :>> t)

parseToken :: Parser PTerm
parseToken = getPosition >>= echo . (\x -> "parseToken" ++ " : " ++ x) . showPos >>
  spaces >> (try parseSymbol <|> try parseList <|> try parseTuple <?> "Token") <* spaces

parseList :: Parser PTerm
parseList = getPosition >>= echo . (\x -> "parseList" ++ " : " ++ x) . showPos >>
  (List :>) <$> between (char '[') (char ']') (sepBy parsePTerm (char ','))

parseTuple :: Parser PTerm
parseTuple = getPosition >>= echo . (\x -> "parseTuple" ++ " : " ++ x) . showPos >>
  (List :>) <$> between (char '(') (char ')') (sepBy parsePTerm (char ','))

parseNot :: Parser PTerm
parseNot = getPosition >>= echo . (\x -> "parseNot" ++ " : " ++ x) . showPos >>
  pNot <$> (string "no" >> parseToken)

pNot :: PTerm -> PTerm
pNot (Not    :> [t]) = t
pNot (Equal  :> ts)  = NEqual :> ts
pNot (NEqual :> ts)  = Equal :> ts
pNot t               = Not :> [t]

infixOper :: String -> Parser PTerm -> Parser [PTerm]
infixOper oper p = do
  x <- p
  xs <- many1 (string oper >> p)
  return (x:xs)

parseAnd :: Parser PTerm
parseAnd = getPosition >>= echo . (\x -> "parseAnd" ++ " : " ++ x) . showPos >>
  pAnd <$> infixOper "and" parseToken

pAnd :: [PTerm] -> PTerm
pAnd [] = T (B True)
pAnd [t] = t
pAnd (t:s) = And :> case (t, pAnd s) of
  (T (B True), y)        -> [y]
  (x@(T (B False)), y)   -> [x]
  (x, T (B True))        -> [x]
  (x, y@(T (B False)))   -> [y]
  (And :> xs, And :> ys) -> xs ++ ys
  (And :> xs, y)         -> xs ++ [y]
  (x, And :> ys)         -> x : ys
  (x, y)                 -> [x, y]

parseOr :: Parser PTerm
parseOr = getPosition >>= echo . (\x -> "parseOr" ++ " : " ++ x) . showPos >>
  pOr <$> infixOper "or" parseToken

pOr :: [PTerm] -> PTerm
pOr [] = T (B False)
pOr [t] = t
pOr (t:s) = Or :> case (t, pOr s) of
  (T (B False), y)     -> [y]
  (x@(T (B True)), y)  -> [x]
  (x, T (B False))     -> [x]
  (x, y@(T (B True)))  -> [y]
  (Or :> xs, Or :> ys) -> xs ++ ys
  (Or :> xs, y)        -> xs ++ [y]
  (x, Or :> ys)        -> x : ys
  (x, y)               -> [x, y]

parseEqual :: Parser PTerm
parseEqual = getPosition >>= echo . (\x -> "parseEqual" ++ " : " ++ x) . showPos >>
  liftM2 (\x y -> Equal :> [x,y]) parseToken (string "eq" >> parseToken)

parseNEqual :: Parser PTerm
parseNEqual = getPosition >>= echo . (\x -> "parseNEqual" ++ " : " ++ x) . showPos >>
  liftM2 (\x y -> NEqual :> [x,y]) parseToken (string "ne" >> parseToken)

parseIn :: Parser PTerm
parseIn = getPosition >>= echo . (\x -> "parseIn" ++ " : " ++ x) . showPos >>
  liftM2 (\x y -> In :> [x,y]) parseToken (string "in" >> parseToken)

parseArgs :: Parser PTerm
parseArgs = getPosition >>= echo . (\x -> "parseArgs" ++ " : " ++ x) . showPos >>
  (\x -> Args :> [x]) <$> (string "args" >> parseToken)

parseReplace :: Parser PTerm
parseReplace = getPosition >>= echo . (\x -> "parseReplace" ++ " : " ++ x) . showPos >>
  (Replace :>) <$> (string "replace" >> count 2 parseToken)

parsePrefix :: Parser PTerm
parsePrefix = getPosition >>= echo . (\x -> "parsePrefix" ++ " : " ++ x) . showPos >>
  (try parseNot <|> try parseIn <|> try parseArgs <|> try parseReplace <?> "Prefix")

parseInfix :: Parser PTerm
parseInfix = getPosition >>= echo . (\x -> "parseInfix" ++ " : " ++ x) . showPos >>
  (try parseAnd <|> try parseOr <|> try parseEqual <|> try parseNEqual <?> "Infix")

parseKeyword :: Parser PTerm
parseKeyword = getPosition >>= echo . (\x -> "parseKeyword" ++ " : " ++ x) . showPos >>
  (try parsePrefix <|> try parseInfix <?> "Keyword")

-- | Parse a program term
parsePTerm :: Parser PTerm
parsePTerm = getPosition >>= echo . (\x -> "parsePTerm" ++ " : " ++ x) . showPos >>
  spaces >> (try parseKeyword <|> try parseTerm <|> try parseToken <?> "PTerm") <* spaces

parseDo :: Parser Program
parseDo = getPosition >>= echo . (\x -> "parseDo" ++ " : " ++ x) . showPos >>
  string "do" >> parseProgram

parseWhere :: Parser PTerm
parseWhere = getPosition >>= echo . (\x -> "parseWhere" ++ " : " ++ x) . showPos >>
  pAnd . reverse <$> option [] (string "where " >> f [])
  where
    f :: [PTerm] -> Parser [PTerm]
    f ts = do
      t <- parsePTerm
      (lookAhead parseAssign >> return (t:ts)) <|> f (t:ts) <|> return (t:ts)

-- | Parse an assigning instruction
parseAssign :: Parser Program
parseAssign = do
  getPosition >>= echo . (\x -> "parseAssign" ++ " : " ++ x) . showPos
  pat <- parsePTerm <* (string "= " <|> string "<- ")
  liftM3 (Assign pat) parsePTerm parseWhere parseProgram

-- | Parse a branching instruction
parseBranch :: Parser Program
parseBranch = getPosition >>= echo . (\x -> "parseBranch" ++ " : " ++ x) . showPos >>
  liftM3 Branch (string "if " >> parsePTerm) parseDo parseProgram

-- | Parse a switching instruction
parseSwitch :: Parser Program
parseSwitch = do
  getPosition >>= echo . (\x -> "parseSwitch" ++ " : " ++ x) . showPos
  expr <- string "case " >> parsePTerm <* string "of"
  cond <- parseWhere
  cases <- many1 (liftM2 (,) parsePTerm parseDo)
  Switch expr cond cases <$> parseProgram

-- | Parse an acting instruction
parseAction :: Parser Program
parseAction = do
  getPosition >>= echo . (\x -> "parseAction" ++ " : " ++ x) . showPos
  act <- parsePTerm
  liftM2 (Action act) parseWhere parseProgram

-- | Parse an empty instruction
parseEmpty :: Parser Program
parseEmpty = getPosition >>= echo . (\x -> "parseEmpty" ++ " : " ++ x) . showPos >>
  string "done" >> return Program.Empty

-- | Parse a program fragment
parseProgram :: Parser Program
parseProgram = getPosition >>= echo . (\x -> "parseProgram" ++ " : " ++ x) . showPos >>
  spaces >> (
  try parseAssign <|>
  try parseBranch <|>
  try parseSwitch <|>
  try parseAction <|>
  try parseEmpty      <?> "Program"
  ) <* spaces
