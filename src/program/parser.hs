{-|
Module      : Program.Parser
Description :
Copyright   : (c) Grigoriy Bokov, 2017
License     : GPL-3
Maintainer  : bokov@intsys.msu.ru
Stability   : experimental
Portability : POSIX
-}
module Program.PSymbol
    (
      -- exports
    )
where

-- External imports

-- Internal imports

------------------------------------------------------------------------------------------
-- Data and type declaration

------------------------------------------------------------------------------------------
-- Function

-- | Parse a program fragment corresponding to a given indent
parseProgram :: Int -> ReadS Program
parseProgram ind str =  parseAssign ind str
                     ++ parseBranch ind str
                     ++ parseSwitch ind str
                     ++ parseAction ind str
                     ++ parseEmpty  ind str

-- | Skip in a string a given number of spaces
skipSpaces :: Int -> String -> String
skipSpaces 0 str = str
skipSpaces n (' ':str) = skipSpaces (n-1) str

parseIndent :: Int -> String -> [String]
parseIndent ind str = [a:_ <- [skipSpaces ind str], not isSeparator a]

skip :: String -> String -> String
skip str pat = case (str =~ pat :: (String,String,String)) of
  ("",_,r) -> r

parseWhere :: Int -> ReadS [PTerm]
parseWhere ind str =  [ ([t],r) | (t,r) <- parsePTerm $ skip str " +where +"]
                   ++ [ (ts,s) | r <- parseIndent ind $ skip str " *\n",
                                 (ts,s) <- parseWhere (ind+2) $ skip r "where"]
                   ++ [ (t:ts,u) | r <- parseIndent ind $ skip str " *\n",
                                   (t,s)  <- parsePTerm r
                                   (ts,u) <- parseWhere ind s]
                   ++ [ ([],str) | a:_ <- parseIndent (ind-2) $ skip str " *\n",
                                   not isSeparator a]
                   ++ [ ([],str) | a:_ <- parseIndent (ind-4) $ skip str " *\n",
                                   not isSeparator a]

-- | Parse an assigning instruction of program fragment corresponding to a given indent
parseAssign :: Int -> ReadS Program
parseAssign ind str =
  [ (Assign p v (P.and ts) j, u) | (p,r)  <- parsePTerm $ parseIndent ind str,
                                   (v,s)  <- parsePTerm $ skip r " += +",
                                   (ts,t) <- parseWhere s,
                                   (j,u)  <- parseProgram ind $ skip t " *\\n"]

-- | Parse a branching instruction of program fragment corresponding to a given indent
readBranch :: Int -> Int -> ReadS Program
readBranch i ind r =
[ (Branch [] c b p, v) | s@(a:_) <- [skipSpaces r ind], a /= ' ',
                         (c, '\n':t) <- readPTerm i s,
                         (b, u) <- readProgram i (ind+2) t,
                         (p, v) <- readProgram i ind u ]

-- | Read a case of switching instruction corresponding to a given indent
readSwitchCases :: Int -> Int -> ReadS [Program]
readSwitchCases i ind r =  [ (p:cl, v) | (' ':s@(a:_)) <- [skipSpaces r ind], isDigit a,
                                       (x, ':':'\n':t) <- lex s, all isDigit x,
                                       (p, u) <- readProgram i (ind+2) t,
                                       (cl, v) <- readSwitchCases i ind u ]
                      ++ [ ([], r) | (a:s) <- [skipSpaces r ind],  a /= ' ' ]

-- | Read a switching instruction of program fragment corresponding to a given indent
readSwitch :: Int -> Int -> ReadS Program
readSwitch i ind r =
[ (Switch [] e cl p, v) | ('c':'a':'s':'e':' ':s) <- [skipSpaces r ind],
                          (e, ' ':'o':'f':'\n':t) <- readPTerm i s,
                          (cl, u) <- readSwitchCases i ind t,
                          (p, v) <- readProgram i ind u  ]

-- | Read an acting instruction of program fragment corresponding to a given indent
readAction :: Int -> Int -> ReadS Program
readAction i ind r =
[ (Action [] t, u) | s@(a:_) <- [skipSpaces r ind], a /= ' ',
                     (t, '\n':u) <- readPTerm i s, isAction t ]

-- | Read an empty program fragment corresponding to a given indent
readEmpty :: Int -> Int -> ReadS Program
readEmpty i ind "" = [(Empty, "done")]
