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
  parse :: LSymbol.Base m => String -> m a

-- | Parse instance for program symbols
instance Parse PSymbol where
  parse str = getLSymbols >>= \db ->
    case runParser parsePSymbol (Info db M.empty 1) "" (Text.pack str) of
      Right s   -> return s
      Left  err -> (error . unwords . map messageString . errorMessages) err

-- | Parse instance for program terms
instance Parse PTerm where
  parse str = getLSymbols >>= \db ->
    case runParser parsePTerm (Info db M.empty 1) "" (Text.pack str) of
      Right t   -> return t
      Left  err -> (error . unwords . map messageString . errorMessages) err

-- | Parse instance for programs
instance Parse Program where
  parse str = getLSymbols >>= \db ->
    case runParser parseProgram (Info db M.empty 1) "" (Text.pack str) of
      Right p   -> return p
      Left  err -> (error . unwords . map messageString . errorMessages) err

------------------------------------------------------------------------------------------
-- Functions

addVar :: String -> Info -> Info
addVar name info =
  let n = varnum info
      l = locals info
  in info {varnum = n + 1, locals = M.insert name n l}

parseVar :: Parser PSymbol
parseVar = do
  name <- many1 letter
  info <- getState
  case M.lookup name (locals info) of
    Just n -> return (X n)
    _      -> modifyState (addVar name) >> return (X (varnum info))

parseInt :: Parser PSymbol
parseInt = (I . read) <$> many1 digit

parseBool :: Parser PSymbol
parseBool = (I . read) <$> (string "True" <|> string "False")

parseLSymbol :: Parser PSymbol
parseLSymbol = S <$> (getLSymbol =<< many1 letter)

-- | Parse a program symbol
parsePSymbol :: Parser PSymbol
parsePSymbol = choice [parseInt, parseBool, parseLSymbol, parseVar]

parseSymbol :: Parser PTerm
parseSymbol = T <$> parsePSymbol

parseTerm :: Parser PTerm
parseTerm = liftM2 f (choice [parseLSymbol, parseVar]) parseToken
  where
    f x (List :> ts) = x :> ts
    f x t            = x :>> t

parseToken :: Parser PTerm
parseToken = spaces >> choice [parseSymbol, parseList, parseTuple] <* spaces

parseList :: Parser PTerm
parseList = (List :>) <$> between (char '[') (char ']') (sepBy parsePTerm (char ','))

parseTuple :: Parser PTerm
parseTuple = (List :>) <$> between (char '(') (char ')') (sepBy parsePTerm (char ','))

parseNot :: Parser PTerm
parseNot = pNot <$> parseToken

pNot :: PTerm -> PTerm
pNot (Not    :> [t]) = t
pNot (Equal  :> ts)  = NEqual :> ts
pNot (NEqual :> ts)  = Equal :> ts
pNot t               = Not :> [t]

parseAnd :: Parser PTerm
parseAnd = pAnd <$> sepBy parseToken (string "and")

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
parseOr = pOr <$> sepBy parseToken (string "or")

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
parseEqual = liftM2 (\x y -> Equal :> [x,y]) parseToken (string "eq" >> parseToken)

parseNEqual :: Parser PTerm
parseNEqual = liftM2 (\x y -> NEqual :> [x,y]) parseToken (string "ne" >> parseToken)

parseIn :: Parser PTerm
parseIn = liftM2 (\x y -> In :> [x,y]) parseToken (string "in" >> parseToken)

parseArgs :: Parser PTerm
parseArgs = (\x -> Args :> [x]) <$> parseToken

parseReplace :: Parser PTerm
parseReplace = (Replace :>) <$> (string "replace" >> count 2 parseToken)

parsePrefix :: Parser PTerm
parsePrefix = choice [parseNot, parseIn, parseArgs, parseReplace]

parseInfix :: Parser PTerm
parseInfix = choice [parseAnd, parseOr, parseEqual, parseNEqual]

parseKeyword :: Parser PTerm
parseKeyword = choice [parsePrefix, parseInfix]

-- | Parse a program term
parsePTerm :: Parser PTerm
parsePTerm = spaces >> choice [parseKeyword, parseTerm, parseToken] <* spaces

parseDo :: Parser Program
parseDo = string "do" >> parseProgram

parseWhere :: Parser PTerm
parseWhere = pAnd <$> option [] (string "where" >> many1 parsePTerm)

-- | Parse an assigning instruction
parseAssign :: Parser Program
parseAssign = do
  pat <- parsePTerm <* (string "=" <|> string "<-")
  liftM3 (Assign pat) parsePTerm parseWhere parseProgram

-- | Parse a branching instruction
parseBranch :: Parser Program
parseBranch = liftM3 Branch (string "if" >> parsePTerm) parseDo parseProgram

-- | Parse a switching instruction
parseSwitch :: Parser Program
parseSwitch = do
  expr <- string "case" >> parsePTerm <* string "of"
  cond <- parseWhere
  cases <- many1 (liftM2 (,) parsePTerm parseDo)
  Switch expr cond cases <$> parseProgram

-- | Parse an acting instruction
parseAction :: Parser Program
parseAction = do
  act <- parsePTerm
  liftM2 (Action act) parseWhere parseProgram

-- | Parse an empty instruction
parseEmpty :: Parser Program
parseEmpty = string "done" >> return Program.Empty
addWhere [] = []

-- | Parse a program fragment
parseProgram :: Parser Program
parseProgram = spaces >>
  choice [parseAssign, parseBranch, parseSwitch, parseAction, parseEmpty] <*
  spaces
