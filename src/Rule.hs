{-|
Module      : Rule
Description : Internal representarion of inference rule
Copyright   : (c) Grigoriy Bokov, 2017
                  Gleb Kalachev, 2017
License     : GPL-3
Maintainer  : sample@email.com
Stability   : experimental
Portability : POSIX

Модуль содержит описание структуры приема во внутреннем представлении, а также
интерфесы для работы с ней.
-}
module Rule
    (
      -- exports
        Rule(..)
    ) where

-- Internal imports
import           LSymbol

-- Inference rule
data Rule = Rule
  {
    symbol      :: LSymbol,
    theorem     :: LTerm,
    header      :: LTerm,
    filters     :: [LTerm],
    specifiers  :: [LTerm],
    normalizers :: [LTerm]
  }

-- -- Show instance for Rule
-- instance Show Rule where
--   show (Rule sym th h f s n) =
--        "Rule {\n"
--     ++ "  symbol      = " ++ show sym ++ ",\n"
--     ++ "  theorem     = " ++ show th ++ ",\n"
--     ++ "  header      = " ++ show h ++ ",\n"
--     ++ "  filters     = " ++ pr f ++ ",\n"
--     ++ "  specifiers  = " ++ pr s ++ ",\n"
--     ++ "  normalizers = " ++ pr n ++ "\n}"
--     where
--       pr l = case l of
--           [] -> "[]"
--           [x] -> "[" ++ show x ++ "]"
--           x:xs -> "[\n    " ++ show x ++ concatMap (\x -> ",\n    "++show x) xs ++ "\n  ]"
