{-# LANGUAGE FlexibleInstances #-}

{-|
Module      : Program.Parser
Description :
Copyright   : (c) Grigoriy Bokov, 2017
License     : GPL-3
Maintainer  : bokov@intsys.msu.ru
Stability   : experimental
Portability : POSIX
-}
module Program.Parser
    (
      -- exports
      Parser, ParserS,
      Program.Parser.parse, parse_,
      write,
      (+>+), (>>>), skip, parsePTerm
    )
where

-- External imports
import           Control.Monad.State
import           Data.Char
import           Data.List
import qualified Data.Map               as Map
import           Data.Maybe
import           Text.Parsec
import           Text.Parsec.Char
import           Text.Parsec.Combinator
import           Text.Parsec.Prim
import qualified Text.Parsec.Text       as PT
import           Text.Regex.Posix

-- Internal imports
import           LSymbol
import           Program.PSymbol
import           Term

data Info = Info
  {
    lsymbols :: LSymbols,
    locals   :: Map.Map String Int,
    varnum   :: Int
  }

type Parser a = PT.GenParser Info a

addVar :: String -> Info -> Info
addVar name info =
  let n = varnum info
      l = locals info
  in info {varnum = n + 1, locals = Map.insert name n l}

parseVar :: Parser PSymbol
parseVar = do
  name <- many1 letter
  info <- getState
  case Map.lookup name (locals info) of
    Just n -> return (X n)
    otherwise -> modifyState (addVar name) >> return (X (varnum info))

parseInt :: Parser PSymbol
parseInt = (I . read) <$> many1 digit

parseBool :: Parser PSymbol
parseBool = (I . read) <$> (string "True" <|> string "False")

parseLSymbol :: Parser PSymbol
parseLSymbol = S <$> liftM2 lsymbol (many1 letter) (fmap lsymbols getState)

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
parseNot = (Not :>) <$> parseToken

parseAnd :: Parser PTerm
parseAnd = (And :>) <$> sepBy parseToken (string "and")

parseAnd :: Parser PTerm
parseAnd = (Or :>) <$> sepBy parseToken (string "or")

parseEqual :: Parser PTerm
parseEqual = liftM2 (\x y -> Equal :> [x,y]) parseToken (string "eq" >> parseToken)

parseNEqual :: Parser PTerm
parseNEqual = liftM2 (\x y -> NEqual :> [x,y]) parseToken (string "ne" >> parseToken)

parseIn :: Parser PTerm
parseIn = liftM2 (\x y -> In :> [x,y]) parseToken (string "in" >> parseToken)

parseArgs :: Parser PTerm
parseArgs = fmap (\x -> Args :>[x]) parseToken

parseReplace :: Parser PTerm
parseReplace = (Replace :>) <$> string "replace" >> count 2 parseToken

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
parseWhere = PSymbol.and <$> option [] (string "where" >> many1 parsePTerm)

-- | Parse an assigning instruction
parseAssign :: Parser Program
parseAssign = do
  pat <- parsePTerm <* char '=' <|> string "<-"
  liftM3 (Assign pat) parsePTerm parseWhere parseProgram

-- | Parse a branching instruction
parseBranch :: Parser Program
parseBranch = liftM3 Branch (string "if" >> parsePTerm) parseDo parseProgram

-- | Parse a switching instruction
parseSwitch :: Parser Program
parseSwitch = do
  expr <- string "case" >> parsePTerm <* string "of"
  cond <- parseWhere
  cases <- many1 (liftM2 ((,)) parsePTerm parseDo)
  (Switch expr cond cases) <$> parseProgram

-- | Parse an acting instruction
parseAction :: Parser Program
parseAction = do
  act <- parsePTerm
  when (isAction act) (liftM2 (Action act) parseWhere parseProgram)

-- | Parse an empty instruction
parseEmpty :: Parser Program
parseEmpty = string "done" >> return Empty
addWhere [] = []

-- | Parse a program fragment
parseProgram :: Parser Program
parseProgram = spaces >> choice [parseAssign, parseBranch, parseSwitch, parseAction, parseEmpty] <* spaces
