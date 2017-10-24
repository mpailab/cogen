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

-- | Skip in a string a given pattern
skip :: String -> String -> String
skip str pat = case (str =~ pat :: (String, String, String)) of
  ("",_,r) -> r

-- | Skip in a string a given number of indents
skipIndents :: Int -> String -> String
skipIndents 0 str = str
skipIndents n (' ':' ':str) = skipIndents (n-1) str

-- | Parse a given indent
parseIndent :: Int -> String -> [String]
parseIndent ind str =  [a:_ <- [skipIndents ind], not isSeparator a]
                    ++ [a:_ <- parseIndent ind $ skip str " *\\n", not isSeparator a]

-- | Parse a where statement of program fragment corresponding to a given indent
parseWhere :: Int -> ReadS [PTerm]
parseWhere ind s0 =  [ ([t],s1)  | (t,s1) <- parsePTerm $ skip s0 " +where +", isBool t]
                  ++ [ (ts,s2)   | s1 <- parseIndent ind s0,
                                   (ts,s2) <- parseWhere (ind+1) $ skip s1 "where"]
                  ++ [ (t:ts,s3) | s1 <- parseIndent ind s0,
                                   (t,s2)  <- parsePTerm s1, isBool t,
                                   (ts,s3) <- parseWhere ind s2]
                  ++ [ ([],s0)   | a:_ <- parseIndent (ind-2) s0, not isSeparator a]

-- | Parse an assigning instruction of program fragment corresponding to a given indent
parseAssign :: Int -> ReadS Program
parseAssign ind s0
  =  [ (Assign p (P.list v) (P.and ts) j, s5) | s1 <- parseIndent ind s0,
                                                (p,s2)  <- parsePTerm s1,
                                                (v,s3)  <- parsePTerm $ skip s2 " += +",
                                                (ts,s4) <- parseWhere (ind+1) s3,
                                                (j,s5)  <- parseProgram ind s4]
  ++ [ (Assign p g (P.and ts) j, s5) | s1 <- parseIndent ind s0,
                                       (p,s2)  <- parsePTerm s1,
                                       (g,s3)  <- parsePTerm $ skip s2 " +<- +",
                                       (ts,s4) <- parseWhere (ind+1) s3,
                                       (j,s5)  <- parseProgram ind s4]

-- | Parse a branching instruction of program fragment corresponding to a given indent
readBranch :: Int -> ReadS Program
readBranch ind s0 =
  [ (Branch c b j, s5) | s1 <- parseIndent ind s0,
                        (c,s2) <- parsePTerm $ skip s1 "if ", isBool c,
                        s3 <- parseIndent ind s2,
                        (b,s4) <- parseProgram (ind+1) $ skip s3 "do",
                        (j,s5) <- parseProgram ind s4 ]

-- | Parse a case of switching instruction corresponding to a given indent
parseSwitchCases :: Int -> ReadS [Program]
parseSwitchCases ind s0
  =  [ ((t,p):cs,s5) | s1 <- parseIndent ind s0,
                       (t,s2) <- parsePTerm s1,
                       s3 <- parseIndent ind s2,
                       (p,s4) <- parseProgram (ind+1) $ skip s3 "do",
                       (cs,s5) <- parseSwitchCases ind s4 ]
  ++ [ ([],s0) | a:_ <- parseIndent (ind-2) s0, not isSeparator a]

-- | Parse a switching instruction of program fragment corresponding to a given indent
parseSwitch :: Int -> ReadS Program
parseSwitch ind s0 =
  [ (Switch e c cs j, s4) | s1 <- parseIndent ind s0,
                            (e,s2) <- parsePTerm $ skip s1 "case ",
                            (cs,s3) <- parseSwitchCases (ind+1) $ skip s2 " *of",
                            (j,s4) <- parseProgram ind s3 ]

-- | Parse an acting instruction of program fragment corresponding to a given indent
parseAction :: Int -> ReadS Program
parseAction ind s0 =
  [ (Action a (P.and ts) j, s4) | s1 <- parseIndent ind s0,
                                  (a,s2) <- parsePTerm s1, isAction a,
                                  (ts,s3) <- parseWhere (ind+1) s2,
                                  (j,s4) <- parseProgram ind s3 ]

-- | Parse an empty program fragment corresponding to a given indent
parseEmpty :: Int -> ReadS Program
parseEmpty ind s0 = [(Empty, s2) | s1 <- parseIndent ind s0,
                                   s2 <- [skip s1 "done"] ]
