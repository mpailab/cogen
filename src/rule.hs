module Rule
    (
        Rule(..)
    ) where

import Terms

data Rule = Rule
    { symbol      :: TermSym,
      theorem     :: Term,
      header      :: Term,
      filters     :: [Term],
      specifiers  :: [Term],
      normalizers :: [Term]
    }
    deriving (Read)

instance Show Rule where
    show Rule {symbol=sym, theorem=th, Rule.header=h, filters=f, specifiers=s, normalizers=n} =
         "Rule {\n"
      ++ "  symbol      = " ++ show sym ++ ",\n"
      ++ "  theorem     = " ++ show th ++ ",\n"
      ++ "  header      = " ++ show h ++ ",\n"
      ++ "  filters     = " ++ pr f ++ ",\n"
      ++ "  specifiers  = " ++ pr s ++ ",\n"
      ++ "  normalizers = " ++ pr n ++ "\n}"
      where
        pr l = case l of
            [] -> "[]"
            [x] -> "[" ++ show x ++ "]"
            x:xs -> "[\n    " ++ show x ++ concatMap (\x -> ",\n    "++show x) xs ++ "\n  ]"
