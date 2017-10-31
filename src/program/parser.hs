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
      write,
      skip
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
skip :: String -> String -> [String]
skip pat str = if pat == "QQQ"
  then
    error ("Error:\n>>>" ++ str ++ "\n# " ++ show (str =~ "do[[:space:]]" :: (String, String, String)))
  else
    case (str =~ pat :: (String, String, String)) of
      ("",_,r) -> [r]
      _        -> []

parseEither :: Parser a => String -> LSymbols -> Either String a
parseEither str db =
  case [ x | (x,s) <- parse_ str db, (s =~ "\\s*" :: Bool) ] of
    [x] -> Right x
    []  -> Left "Parser: no parse"
    _   -> Left "Parser: ambiguous parse"

parse :: Parser a => String -> LSymbols -> a
parse str db = either error id (parseEither str db)
