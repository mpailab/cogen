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
      (+>+), (>>>), skip
    )
where

-- External imports
import           Text.Regex.Posix

-- Internal imports
import           LSymbol

------------------------------------------------------------------------------------------
-- Data and type declaration

type ParserS a b = [b] -> LSymbols -> [(a, [b])]

class Parser a where
  parse_ :: ParserS a Char
  write  :: a -> LSymbols -> String

------------------------------------------------------------------------------------------
-- Function

infixl 5 +>+
(+>+) :: ParserS a b -> ParserS a b -> ParserS a b
f +>+ g = (\x db -> let s = f x db in if null s then g x db else s)

infixr 6 >>>
(>>>) :: Show b => String -> ParserS a b -> ParserS a b
(>>>) s f x db
  | (_:_:_) <- l = error $ "Parser error: Can't " ++ s ++ " in " ++ show x
  | otherwise = l
  where
    l = f x db

-- | Skip a pattern in a given string
skip :: String -> String -> [String]
skip pat str = case (str =~ pat :: (String, String, String)) of
  ("",_,r) -> [r]
  _        -> []

parseEither :: Parser a => String -> LSymbols -> Either String a
parseEither str db =
  case [ x | (x,"") <- parse_ str db ] of
    [x] -> Right x
    []  -> Left "Parser: no parse"
    _   -> Left "Parser: ambiguous parse"

parse :: Parser a => String -> LSymbols -> a
parse str db = either error id (parseEither str db)
