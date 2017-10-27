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
      parse, parse_,
      write
    )
where

-- External imports
import           Text.Regex.Posix

-- Internal imports
import           LSymbol

------------------------------------------------------------------------------------------
-- Data and type declaration

type ParserS a = String -> LSymbols -> [(a, String)]

class Parser a where
  parse_ :: ParserS a
  write  :: a -> LSymbols -> String

------------------------------------------------------------------------------------------
-- Function

-- | Skip a pattern in a given string
skip :: String -> String -> String
skip pat str = case (str =~ pat :: (String, String, String)) of
  ("",_,r) -> r

parseEither :: Parser a => String -> LSymbols -> Either String a
parseEither str db =
  case [ x | (x,"") <- parse_ str db ] of
    [x] -> Right x
    []  -> Left "Parser: no parse"
    _   -> Left "Parser: ambiguous parse"

parse :: Parser a => String -> LSymbols -> a
parse str db = either error id (parseEither str db)
