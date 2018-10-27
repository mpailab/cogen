{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE TupleSections     #-}

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
import           Control.Monad.Identity
import           Data.Char
import           Data.Functor
import           Data.List
import qualified Data.Map               as Map
import           Data.Maybe
import qualified Data.Text              as Text
import           Debug.Trace
import           Text.Parsec
import           Text.Parsec.Char
import           Text.Parsec.Combinator
import           Text.Parsec.Error
import           Text.Parsec.Expr
import           Text.Parsec.Language
import           Text.Parsec.Prim
import           Text.Parsec.Text       hiding (Parser)
import           Text.Parsec.Token

-- Internal imports
import           Expr
import           LSymbol
import           Program
import           Term
import           Utils
import           Program.BuiltIn
import           Structs.Trie


------------------------------------------------------------------------------------------
-- Data types and classes declaration

data OpType
  = Infx    -- ^ infix operator   : arg1 S1 arg2 S2 ... S(N-1) argN
  | Prfx    -- ^ prefix operator  : B arg1 S1 arg2 S2 ... S(N-1) argN
  | Pstx    -- ^ postfix operator : arg1 S1 arg2 S2 ... S(N-1) argN E
  | PrPsfx  -- ^ B arg1 S1 arg2 S2 ... argN E

data OpArg
  = OpInfx String           -- ^ internal operand
  | OpInfxSeq String String -- ^ operand sequence
  | OpLastSeq String Int    -- ^ operand sequence at the end
  | OpLast Int              -- ^ last operand
  deriving (Eq,Ord,Show)

instance Show (a->b) where
  show _ = "<fun>"

data OpHeader
  = OpFunH     Expr             -- ^ coral function
  | OpCompileH ([Expr] -> Expr) -- ^ compile-time action
  deriving (Show)

minPriority :: Int
minPriority = -2^30

maxPriority :: Int
maxPriority = 2^30

termPriority :: Int
termPriority = 100 -- ^ term composition has next priority below function apply

applyPriority :: Int
applyPriority = 102 -- ^ function application has highest priority

data OpInfo = OpInfo {
    lpriority :: Int,    -- ^ priority of first argument of infix or postfix operators
    opargs :: [OpArg],   -- ^ arguments except first
  --  assoct :: Assoc,     -- ^ type of assotiativity (left, right of none)
    opheader :: OpHeader -- ^ header of function called by
  }
  deriving (Show)

data PStateM = PStateM {
  infxopsM   :: Map.Map String OpInfo,
  prfxopsM   :: Map.Map String OpInfo,
  oppartsM   :: Trie Char Int -- 1 if keyword, 0 if sequence of symbols, 2 if reserved
}

data PState = PSt {
    inFrag    :: Bool,
    isPattern :: Bool,
    indents   :: [IndentSt],
    extvars   :: [(Int,Int)],
    statem    :: PStateM
  }
infxops = infxopsM . statem
prfxops = prfxopsM . statem
opparts = oppartsM . statem

type IndentSt = Either (Int,Int) Int

initState :: PState
initState = PSt {
  inFrag=False,
  isPattern = False,
  indents=[Left (0,0)],
  extvars=[],
  statem = initStateM
}

-- modifyStateInF :: NameSpace m => (PState -> PState) -> (t -> Parser m a) -> t -> Parser m a
-- modifyStateInF ch f Var = do old <- getState
--                            modifyState $ f
--                            res <- f
--                            setState old

setPmEnabledIn :: NameSpace m => Bool -> Parser m a -> Parser m a
setPmEnabledIn en f = setPmEnabledInF en (\() -> f) ()

setPmEnabledInF :: NameSpace m => Bool -> (t -> Parser m a) -> t -> Parser m a
setPmEnabledInF en f x = do old <- isPattern <$> getState
                            modifyState (\st -> st{ isPattern = en })
                            res <- f x
                            modifyState (\st -> st{ isPattern = old })
                            return res

inpattern :: NameSpace m => Parser m a -> Parser m a
inpattern = setPmEnabledIn True

nopattern :: NameSpace m => Parser m a -> Parser m a
nopattern = setPmEnabledIn False

inpatternF :: NameSpace m => (t -> Parser m a) -> t -> Parser m a
inpatternF = setPmEnabledInF True

nopatternF :: NameSpace m => (t -> Parser m a) -> t -> Parser m a
nopatternF = setPmEnabledInF False

ispattern :: NameSpace m => Parser m ()
ispattern = isPattern <$> getState >>= guard

topIndent :: NameSpace m => Parser m IndentSt
topIndent = head . indents <$> getState

pushIndent :: NameSpace m => IndentSt -> Parser m ()
pushIndent i = echo ("pushIndent "++show i) >> modifyState (\st -> let is = indents st in st {indents = i:is})

popIndent :: NameSpace m => Parser m IndentSt
popIndent = echo "popIndent" >> getState >>= (\st -> let i:is = indents st in putState st {indents = is} >> return i)

updateIndent :: NameSpace m => IndentSt -> Parser m ()
updateIndent i = echo ("updateIndent "++show i) >> modifyState (\st -> let _:is = indents st in st {indents = i:is})

getPos :: NameSpace m => Parser m (Int,Int)
getPos = getPosition >>= \pos -> return (sourceLine pos, sourceColumn pos)

dbg :: NameSpace m => String -> Parser m ()
dbg name = getPos >>= \pos -> echo (name++" "++show pos)

beginIndent :: NameSpace m => Parser m ()
beginIndent = whiteSpaceParser >> do
  pos@(cr,cc) <- getPos
  st <- getState
  case head $ indents st of
    Left (r,c) | cr == r   -> pushIndent (Left (r,c))
               | cc > c    -> updateIndent (Right cc) >> pushIndent (Left pos)
               | otherwise -> dbg ("no indent, top = L"++show (r,c)) >> parserZero
    Right c -> if cc == c then pushIndent (Left pos)
               else dbg ("no indent, top = R"++ show c) >>parserZero

endIndent :: NameSpace m => Parser m ()
endIndent = popIndent >>= \case
  Left (r,c) -> topIndent >>= \case
    Left (r1,c1) -> when (r1>r) $ updateIndent (Right c1)
    _            -> return ()
  _          -> return ()

indentBlock :: NameSpace m => Parser m x -> Parser m x
indentBlock p = try (beginIndent *> p <* endIndent)

-- | check if correct indentation
indented :: NameSpace m => Parser m ()
indented = whiteSpaceParser >> topIndent >>= \case
  Left (r,c) -> getPos >>= \(rr,cc)-> unless (rr==r || cc > c) parserZero
  Right c -> getPos >>= \(_,cc)-> unless (cc >= c) parserZero


-- | Parser type
type Parser = ParsecT Text.Text PState

-- | Parser instance for treatment of logical symbols in underlying monad
instance NameSpace m => LSymbol.Base (Parser m) where
  getLSymbols = lift getLSymbols
  setLSymbols = lift . setLSymbols

-- | Parser instance for treatment of program variables in underlying monad
instance NameSpace m => Program.Vars (Parser m) where
  getPVars = lift getPVars
  setPVars = lift . setPVars

-- | Class of parsers in underlying monad with constrained 'NameSpace'.
class Parse a where
  parse :: NameSpace m => String -> String -> m a

------------------------------------------------------------------------------------------
-- Main functions

-- | Parse instance for programs
instance Parse Program where
  parse str source = runParserT programParser initState source (Text.pack str) >>= \case
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

rsvOpNm = ["=", "<-", "@", "&&", "||", "|", "+",
           "++", "->", "*", "/", "&",  "<<", "~=", ".."]

-- | This is a minimal token definition for Coral language.
coralDef :: NameSpace m => GenLanguageDef Text.Text PState m
coralDef = emptyDef
  { commentStart   = "{-"
  , commentEnd     = "-}"
  , commentLine    = "--"
  , nestedComments = True
  , identStart     = letter
  , identLetter    = alphaNum <|> oneOf "_'"
  , opStart        = opLetter coralDef
  , opLetter       = oneOf ":!#$%&*+./<=>?@\\^|-~"
  , reservedOpNames= rsvOpNm ++ ["$"]++(fname . fst <$> concat getBuiltInOps)
  , reservedNames  = ["do", "done", "if", "case", "of", "where",
                      "true", "false", "no", "eq", "ne", "in",
                      "then", "else",
                      "args", "replace", "_", "__"]
  , caseSensitive  = True
  }

-- | Lexer for Coral language - collection of lexical parsers for tokens
coralLexer :: NameSpace m => GenTokenParser Text.Text PState m
coralLexer = makeTokenParser coralDef

-- | Lexeme parser @parensParser p@ parses @p@ enclosed in parenthesis,
-- returning the value of @p@.
parensParser :: NameSpace m => Parser m a -> Parser m a
parensParser p = indented >> parens coralLexer p

-- | Lexeme parser @bracesParser p@ parses @p@ enclosed in braces (\'{\'
-- and \'}\'), returning the value of @p@.
bracesParser :: NameSpace m => Parser m a -> Parser m a
bracesParser = braces coralLexer

-- | Lexeme parser @bracketsParser p@ parses @p@ enclosed in brackets (\'[\'
-- and \']\'), returning the value of @p@.
bracketsParser :: NameSpace m => Parser m a -> Parser m a
bracketsParser p = indented >> brackets coralLexer p

-- | Lexeme parser @identifierParser@ parses a legal identifier of Coral language.
-- Returns the identifier string. This parser will fail on identifiers that are reserved
-- words. Legal identifier (start) characters and reserved words are
-- defined in the 'coralDef'.
identifierParser :: NameSpace m => Parser m String
identifierParser = indented >> identifier coralLexer

-- | Lexeme parser @naturalParser@ parses a natural number (a positive whole
-- number). Returns the value of the number. The number is parsed according to the grammar
-- rules in the Haskell report.
naturalParser :: NameSpace m => Parser m Integer
naturalParser = indented >> natural coralLexer

strParser :: NameSpace m => Parser m String
strParser = stringLiteral coralLexer

-- | The lexeme parser @reservedOpParser name@ parses symbol @name@ which is a reserved
-- operator of Coral language. It also checks that the @name@ is not a prefix of a valid
-- operator.
reservedOpParser :: NameSpace m => String -> Parser m ()
reservedOpParser s = indented >> reservedOp coralLexer s

-- | The lexeme parser @reservedParser name@ parses symbol @name@ which is a reserved
-- identifier of Coral language. It also checks that the @name@ is not a prefix of a
-- valid identifier.
reservedParser :: NameSpace m => String -> Parser m ()
reservedParser s = indented >> reserved coralLexer s

reservedParserU :: NameSpace m => String -> Parser m ()
reservedParserU = reserved coralLexer

-- opParser :: NameSpace m => Parser m String
-- opParser = operator coralLexer

-- | Lexeme parser @commaSepParser p@ parses /zero/ or more occurrences of @p@ separated
-- by comma. Returns a list of values returned by @p@.
commaSepParser :: NameSpace m => Parser m a -> Parser m [a]
commaSepParser = commaSep coralLexer

-- | Lexeme parser @semiSepParser p@ parses /zero/ or more occurrences of @p@ separated
-- by semicolon. Returns a list of values returned by @p@.
semiSepParser :: NameSpace m => Parser m a -> Parser m [a]
semiSepParser = semiSep coralLexer

-- | Parses any white space. White space consists of /zero/ or more
-- occurrences of a space character (any character which satisfies isSpace), a line
-- comment or a block (multi line) comment. Block comments may be nested. How comments are
-- started and ended is defined in the 'coralDef'.
whiteSpaceParser :: NameSpace m => Parser m ()
whiteSpaceParser = whiteSpace coralLexer

funcApp :: NameSpace m => Parser m ()
funcApp = try $ char '`' >> notFollowedBy (letter <|> oneOf "[({") >> whiteSpaceParser
--  <|> do p0 <- getPos
--         whiteSpaceParser
--         p1 <- getPos
--         guard $ p0 /= p1
ident :: NameSpace m => Parser m String
ident = liftM2 (:) (identStart coralDef) (many (identLetter coralDef))

prefixFunc :: NameSpace m => Parser m Expr -> Parser m Expr
prefixFunc p =  whiteSpaceParser >> char '`' >> ((ident >>= symbolOrVar) <|> p)

infxFunc :: NameSpace m => Parser m Expr -> Parser m Expr -> Expr -> Parser m Expr
infxFunc parg pf first = do
  dbg "try infix function"
  f <- prefixFunc pf
  dbg "infix found"
  args <- option [] (funcApp >> many parg)
  whiteSpaceParser
  dbg $ "args = "++show args
  return $ Call f $ first:args

symbolOrVar :: NameSpace m => String -> Parser m Expr
symbolOrVar name = getLSymbol name >>= \case
  Just s  -> return (Sym s)
  Nothing -> getPVar name

-- | Parser of integers
intParser :: NameSpace m => Parser m Integer
intParser = fromInteger <$> naturalParser

extVarParser :: NameSpace m => Parser m Expr
extVarParser = do
  st <- getState
  guard (inFrag st)
  char '$'
  name <- identifierParser
  Var n <- getPVar name
  Var fn <- getPVar (name ++ "__")
  putState $ st { extvars = (n,fn):(extvars st) }
  return $ Var fn -- !!!!!!!!! TODO : ExtVar n

-- | Parser of symbols (logical symbols or variables)
symbolParser :: NameSpace m => Parser m Expr
symbolParser = extVarParser <|> (identifierParser >>= symbolOrVar)

terminalParser :: NameSpace m => Parser m Expr
terminalParser = symbolParser <|> fragParser <|> entryParser True

-- | Parser of program variables
varParser :: NameSpace m => Parser m Expr
varParser = identifierParser >>= \name -> getLSymbol name >>= \case
  Just s  -> unexpected ("The identifier " ++ name ++ " is reserved as logical symbol.\n")
  Nothing -> getPVar name

vparserstep :: NameSpace m => String -> Trie Char a -> Parser m (String,a)
vparserstep s t@(Trie b sub) = try (do {
      ; c <- sp <$> anyChar
      ; when (c==' ') whiteSpaceParser
      ; lmaybe (Map.lookup c sub) >>= vparserstep (c:s)
    }) <|> (s,) <$> lmaybe b
    where sp x = if isSpace x then ' ' else x

vparser :: (Show a,NameSpace m) => Trie Char a -> Parser m (String,a)
vparser t =  vparserstep "" t

opParser :: NameSpace m => Parser m String
opParser = do
  st <- getState
  --dbg (concat $ map (\x->"    "++show x++"\n") $ getkv $ opparts st)
  (s,b) <- vparser $ opparts st
  --guard (b>=0)
  when (b==1) $ notFollowedBy (identLetter coralDef)
  whiteSpaceParser
  return $ reverse s


lmaybe :: MonadPlus m => Maybe a -> m a
lmaybe (Just x) = return x
lmaybe Nothing = mzero

pfxinfo ::  NameSpace m => String -> Parser m OpInfo
pfxinfo op = (Map.lookup op) . prfxops <$> getState >>= lmaybe

prefixop :: NameSpace m => Parser m OpInfo
prefixop = try (opParser >>= pfxinfo)

data StItem = StItem {
  ophdr :: OpHeader,     -- ^ header of current operation
  nargs :: [OpArg],     -- ^ remaining arguments
  rargs :: [Expr],       -- ^ arguments that already read
  nint  :: Int,          -- ^ number of stack items OpInfx or OpInfxSeq
  rprior :: Int,
  closing :: [String]    -- ^ possible closing operators
} deriving (Show)

type EStack = [StItem]

conv1 :: Expr -> OpHeader -> [Expr] -> Expr
conv1 e (OpFunH h) es = Call h $ reverse (e:es)
conv1 e (OpCompileH c) es = c $ reverse (e:es)

-- | parser stack reduce by priority
reducestpr :: Expr -> Int -> EStack -> (EStack,Expr)
reducestpr NONE pr st@(StItem h ([OpLastSeq "" _]) (List a1:args) _ p _ : sts)
  | pr < p = reducestpr (conv1 (List $ reverse a1) h args) pr sts
  | otherwise = (st, NONE)

reducestpr NONE _ st = (st,NONE)

reducestpr arg pr st@(StItem h ([OpLastSeq sep _]) (List a1:args) _ p _ : sts)
  | pr < p = reducestpr (conv1 (List $ reverse(arg:a1)) h args) pr sts
  | otherwise = (st, arg)

reducestpr arg pr st@(StItem h ([OpLast _]) args _ p _ : sts)
  | pr < p =  reducestpr (conv1 arg h args) pr sts
  | otherwise = (st, arg)

reducestpr arg _ st = (st,arg)

reduceall :: Expr -> EStack -> (EStack,Expr)
reduceall e st = reducestpr e (minPriority-1) st

-- | auxilliary function, add empty list if next argument is sequence
addemptylist (OpInfxSeq _ _:_) l = List [] : l
addemptylist (OpLastSeq _ _:_) l = List [] : l
addemptylist _ l = l

topst [] = (0, minPriority, [])
topst (StItem _ _ _ ni rpr cl :_) = (ni,rpr,cl)

updaterpr (h:sts) = case h of
  StItem _ [OpLast pr] _ _ _ _ -> h { rprior = max pr rpr, nint = ni, closing = cl } : sts
  StItem _ [OpLastSeq s pr] r _ _ _ -> h { rargs = List []:r, rprior = max pr rpr, nint = ni, closing = add s cl } : sts
  StItem _ na@(OpInfx s:_) r _ _ _ -> h { rargs = addemptylist na r, rprior = minPriority, nint = ni+1, closing = [s] } : sts
  StItem _ na@(OpInfxSeq s e:_) r _ _ _ -> h { rargs = addemptylist na r, rprior = minPriority, nint = ni+1, closing = [s, e] } : sts
  where (ni, rpr, cl) = topst sts
        add "" x = x
        add s x = s:x

-- updaterpr [h@(StItem _ [OpLast pr] _ _ _)] = [h { rprior = pr, nint = 0 }]
-- updaterpr [h@(StItem _ [OpLastSeq _ pr] r _ _)] = [h { rargs = List []:r, rprior = pr, nint = 0 }]
-- updaterpr [h] = [h { rargs = addemptylist (nargs h) (rargs h), rprior = minPriority, nint = 1 }]

-- | parser stack reduce by separator
reducestop :: Expr -> String -> EStack -> (EStack,Expr,Bool)
reducestop arg op st@(sth@(StItem h ([OpLastSeq o _]) (List a1:args) _ _ _) : sts)
  | op == o   = (sth { rargs = List (arg:a1):args }:sts, NONE, False)
  | otherwise = reducestop (conv1 (List $ reverse(arg:a1)) h args) op sts

reducestop arg op st@(sth@(StItem h ([OpLast _]) args _ _ _) : sts) =
  reducestop (conv1 arg h args) op sts

reducestop arg op st@(sth@(StItem h (OpInfxSeq sep nxt:ops) a@(List a1:args) ni rpr _) : sts)
  | op == sep = (sth { rargs = List (arg:a1):args } : sts, NONE, False)
  | op == nxt =
      if null ops then (sts, conv1 (List $ reverse (arg:a1)) h args, False)
      else (updaterpr $ sth {
        rargs = List (reverse $ arg:a1):args,
        nargs = ops
      } : sts, NONE, False)
  | otherwise = error $ "unexpected `"++op++"` before `"++sep++"` or `"++nxt++"`"

reducestop arg op st@(sth@(StItem h (OpInfx nxt : ops) args ni rpr _) : sts)
  | op == nxt =
      if null ops then (sts, conv1 arg h args, False)
      else (updaterpr $ sth { rargs = arg:args, nargs = ops } :sts, NONE, False)
  | otherwise = error $ "unexpected `"++op++"` before `"++nxt++"`"

reducestop arg _ [] =([],arg,True) -- stack is empty

reducestop arg op st = error $ "strange reducestop case : arg = "++show arg++", op = "++show op++", st = "++show st

isend [] = True
isend (s:ss) = nint s == 0

indd :: NameSpace m => EStack -> Parser m ()
indd [] = indented
indd (a:_) = if nint a == 0 then indented else return ()

isprefix :: NameSpace m => String -> Parser m Bool
isprefix op = Map.member op . prfxops <$> getState
isinfix op  = Map.member op . infxops <$> getState
isclosing op [] = False
isclosing op (h:_) = elem op $ closing h

optype op st | isclosing op st = return (Nothing, Nothing)
             | otherwise = getState >>= \st -> return (Map.lookup op $ infxops st, Map.lookup op $ prfxops st)


iscall (Call _ _) = True
iscall _ = False

applyH = OpCompileH (\[h,(List l)] -> Call h l)

addfun :: [Expr] -> EStack -> Expr -> EStack
addfun as st@(StItem _ _ _ ni pr cl: _) h =
  StItem applyH [OpLastSeq "" $ applyPriority-1] [List as,h] ni (max pr $ applyPriority-1) cl:st
addfun as [] h = [StItem applyH [OpLastSeq "" $ applyPriority-1] [List as,h] 0 (applyPriority-1) []]

maketm :: [Expr] -> Expr
maketm [h, (List args)] = Term $ h :> map (\case {Term t -> t; x -> T x}) args
maketm [h, x] = Term $ h :>> x
maketm x = error $ "strange maketm call : arg = "++show x

addtm :: EStack -> Expr -> EStack
addtm st@(StItem _ _ _ ni pr cl : _) h =
  StItem (OpCompileH (\[x,y] -> Term $ x :>> y)) [OpLast $ termPriority] [h] ni (max pr $ termPriority) cl:st
addtm [] h = [StItem (OpCompileH maketm) [OpLast $ termPriority] [h] 0 (termPriority) []]

-- | parse VExpr, first argument means allow or not pattern-matching
exprElemParser :: NameSpace m => Parser m Expr
exprElemParser = fragParser
          <|> Int <$> intParser
          <|> Str <$> strParser
          <|> lambdaParser
          <|> caseExprParser False
          <|> (reservedParser "_" >> return Any)
          <|> (reservedParser "__" >> return AnySeq)
          <|> (reservedParser "true" >> return (Bool True))
          <|> (reservedParser "false" >> return (Bool False))
          <|> extVarParser
          <|> symbolParser
          <?> "vexpr"

pushst h ops args st =  updaterpr $ StItem h ops args 0 0 []:st

prstack [] = ""
prstack ((StItem h as rs _ _ _):s)= "\n\t"++show h++"\t"++show as++"\t"++show rs++prstack s

exprstep :: NameSpace m => (EStack, Expr, Bool) -> Parser m (EStack, Expr, Bool)
exprstep (st,NONE,_) = dbg "read expression" >> (
      (\(OpInfo _ as h) -> (pushst h as [] st,NONE,False)) <$> try prefixop
  <|> (indd st >> exprElemParser >>= return . (st,,False))
  <|> if null st then parserZero else return (st,NONE,True))

exprstep (st,e,_) = try (do { dbg "in exprstep"
  ; indd st -- if it is not inside parentheses or some operator, then it must be indented
  ; op <- opParser <|> return "" -- read operator or assume it is empty operator
  ; dbg ("read operator "++op++": e = "++show e++", stack = "++(if null st then "[]" else prstack st))
  -- ; ifx <- isinfix op
  -- ; pfx <- isprefix op
  ; (ifx, pfx) <- optype op st -- Map.lookup op . infxops <$> getState
  --; pfx <- Map.lookup op . prfxops <$> getState
  ; case (pfx, ifx, op=="") of
    (Just (OpInfo _ oa oh), Nothing, _) ->
      return (pushst oh oa [] $ hfun st hc, NONE, False)
    (_, _, True) -> case reducestpr e applyPriority st of
      (h1@(StItem _ [OpLastSeq "" _] (List es:args) _ _ _):sts1,e1) -> dbg ("add arg = "++show e1)>>
        return (h1 { rargs=List (e1:es):args } : sts1,NONE,False)
      (st1, e1@(Call h1 [])) -> dbg ("reducerpr -> "++show st1++", "++show e1)>>((exprElemParser >>= return . (addfun [] st1 h1,, False)) <|> return (st1,e1,True))
      (st1, e1) -> dbg ("reducerpr -> "++show st1)>> ((exprElemParser >>= return . (addtm st1 e1,,False)) <|> (dbg "stop" >> return (st1,e1,True)))
    (_, Just (OpInfo lpr [] h),_) ->
      dbg ("stack reduced by '"++op++"' -> "++show (cst,e1))>>return (cst, conv1 e1 h [],False)
      where (cst,e1) = reducestpr e lpr st
    (_, Just (OpInfo lpr args h),_) ->
      dbg ("stack reduced by '"++op++"' -> "++show (cst,e1))>>return (pushst h args [e1] cst, NONE,False)
      where (cst,e1) = reducestpr e lpr st
    (Nothing, Nothing, False) -> case reducestop e op st of
      (_,_,True) -> parserZero -- parser fails and termination signal is returned after <|>
      x          -> dbg ("stack reduced by '"++op++"': "++show x) >> return x
    -- otherwise -> Map.lookup op . infxops <$> getState >>= \case
    --   Just (OpInfo lpr args assoc h) -> return (updaterpr $ StItem h args [e1] 0 0:st, NONE,False)
    --                 where (cst,e1) = reducestpr e lpr st
    --   Nothing -> (\(x,y)->return (x,y,False)) $ reducestop e op st
  }) <|> return (st,e,True)
  where (hc, hfun, rpr) = case e of
          (Call hh as) -> (hh, addfun as, applyPriority)
          otherwise   -> (e, addtm, termPriority)

parseExpr :: NameSpace m => (EStack,Expr,Bool) -> Parser m Expr
parseExpr (st,e,False) = exprstep (st,e,False) >>= parseExpr
parseExpr r@(st,e,True) = case reduceall e st of
  ([],e) -> dbg ("stack reduced : e = "++show e) >> return e
  (StItem _ (OpInfxSeq sep nxt:_) _ _ _ _:_,_) -> error $ sep++"` or `"++nxt++"` expected"
  (StItem _ (OpInfx nxt:_) _ _ _ _:_,_) -> error $ "`"++nxt++"` expected"
  x -> error $ "cannot reduce all : "++show x

exprp :: NameSpace m => Parser m Expr
exprp = parseExpr ([],NONE,False)

data PatternType
  = NoPattern
  | ElemPattern
  | InListPattern
  deriving (Eq,Show)

pattp NoPattern _ = NoPattern
pattp _ pt        = pt

anySeq :: NameSpace m => Parser m Expr
anySeq = ispattern >> try (
  (reservedParser "__" >> return AnySeq) <|> do {
  ; Var n <- varParser
  ; reservedOpParser "@" >> reservedParser "__"
  ; return $ Ref n AnySeq
  })

anyElem :: NameSpace m => Parser m Expr
anyElem = ispattern >> reservedParser "_" >> return Any

entryTailParser :: NameSpace m => Bool -> Int -> Parser m Expr
entryTailParser trm n = do
  dbg "reading entry 1"
  refptr <- (reservedOpParser "&" >> return Ptr) <|> (reservedOpParser "@" >> return Ref)
  dbg "reading entry 2"
  expr <- if trm then Term <$> termParser else vexprParser
  return (refptr n expr)

entryParser :: NameSpace m => Bool -> Parser m Expr
entryParser trm = dbg "parse entry" >> do { Var n <- varParser
                     ; entryTailParser trm n
                     } <?> "entry"

compParser :: NameSpace m => Parser m Expr
compParser =  dbg "parse comp" >> (
              (List  <$> bracketsParser (commaSepParser $ anySeq <|> vexprParser))
          <|> (Tuple <$> parensParser (commaSepParser $ anySeq <|> vexprParser))
          <|> (Set   <$> bracesParser (commaSepParser $ anySeq <|> vexprParser))
          <|> (Term  <$> (termParser <* dbg "return Term"))
          <?> "composite")

simpleVExprParser :: NameSpace m => Parser m Expr
simpleVExprParser = exprp
-- simpleVExprParser = dbg "parse vexpr" >> (
--                       Int <$> intParser
--                   <|> lambdaParser
--                   <|> (compParser >>= \case
--                         Term (T v@(Var n)) -> entryTailParser False n <|> return v
--                         Term (T t)       -> return t
--                         Tuple [expr]     -> return expr  -- ^ expression in parentheses is parsed as one-element tuple
--                         expr             -> return expr
--                       )
--                   <?> "vexpr")

subExprParser :: NameSpace m => Bool -> Parser m Expr
subExprParser _ = exprp
--subExprParser tm = if tm then Term <$> simpleTermParser else vexprParser

-- | parses case-of with cases in each line or one-line expression case Var of {pat1->res1; pat2->res2;...}
caseExprParser :: NameSpace m => Bool -> Parser m Expr
caseExprParser tm = do { reservedParser "case"
                      ; dbg "parse case-of expression"
                      ; x <- nopattern simpleVExprParser
                      ; reservedParser "of"
                      ; cases <- many1 (indentBlock oneCase)
                              <|> bracesParser (semiSepParser oneCase)
                      ; return $ CaseOf x cases
                      }
                      where oneCase = liftM2 (,) simpleVExprParser (reservedOpParser "->" >> subExprParser tm)

ifExprParser :: NameSpace m => Bool -> Parser m Expr
ifExprParser tm = do { reservedParser "if"
                     ; dbg "parse if expression"
                     ; cond <- inpattern boolParser
                     ; reservedParser "then"
                     ; iftrue <- indentBlock $ subExprParser tm
                     ; reservedParser "else"
                     ; iffalse <- indentBlock $ subExprParser tm
                     ; return $ IfElse cond iftrue iffalse
                     }

complexExprParser :: NameSpace m => Bool -> Parser m Expr
complexExprParser tm = ifExprParser tm <|> ifExprParser tm

-- | Parse arguments of function call in expression
vexprTailParser :: NameSpace m => Expr -> Parser m Expr
vexprTailParser h = (dbg "try expr `" >> funcApp >> Call h <$> many (nopattern vexprParser))
                <|> infxFunc (dbg "try infx expr arg" >> nopattern vexprParser) (dbg "try infx expr header" >> {- nopattern ??? -} vexprParser) h
                <|> return h

-- | parse VExpr, first argument means allow or not pattern-matching
atomExprParser :: NameSpace m => Parser m Expr
atomExprParser = fragParser
          <|> (simpleVExprParser >>= vexprTailParser)
          <|> (complexExprParser False >>= vexprTailParser)
          <?> "vexpr"

lambdaParser:: NameSpace m => Parser m Expr
lambdaParser = do
  string "\\"
  args <- many varParser
  reservedOpParser "->"
  cmds <- indentBlock $ many stmtParser
  return $ Fun args cmds

-- | try parse term or function call with given header
parseTermArgs :: NameSpace m => Expr -> Parser m TExpr
parseTermArgs h =
          (funcApp >> (T . Call h) <$> many (nopattern vexprParser))
      <|> T <$> infxFunc (toExpr <$> nopattern simpleTermParser) (toExpr <$> simpleTermParser) h
      <|> (h :>) <$> (dbg "try brackets" >> bracketsParser (commaSepParser (T <$> anySeq <|> termParser) <|> return []))
      <|> (h :>>) <$> (dbg "try :>> var" >> indented >> varParser)
      <|> (dbg ("return T "++show h) >> return (T h))
      where toExpr (T t) = t
            toExpr t     = Term t

simpleTermParser :: NameSpace m => Parser m TExpr
simpleTermParser = getState >>= (\st -> dbg ("parse term or header : pm = " ++ show (isPattern st))) >> (
          (dbg "th : try if" >> T <$> ifExprParser True)
      <|> (dbg "th : try case" >> T <$> caseExprParser True)
      <|> (dbg "th : try parens" >> parensParser simpleTermParser >>= \case
              e@(T (Ref _ _)) -> return e   -- ^ already reference to term
              e@(T (Ptr _ _)) -> return e   -- ^ already pointer to term
              T h -> parseTermArgs h    -- ^ only header read which is symbol, conditional expression or function call
              t  -> return t            -- ^ otherwise it is already a term
          )
      <|> do { dbg "th : try symbol"
             ; sym <- anyElem <|> symbolParser <|> (parensParser lambdaParser <* lookAhead funcApp)
             ; dbg $ "sym = "++show sym
             ; T <$> tryEntry sym <|> parseTermArgs sym
             }
    )
    where
      tryEntry (Var n) = dbg "try entry" >> entryTailParser True n
      tryEntry _       = parserZero

termParser :: NameSpace m => Parser m TExpr
termParser =  dbg "parse term variants" >> do
                  first <- simpleTermParser
                  dbg $ "term : first var = "++show first
                  other <- many $ reservedOpParser "|" >> simpleTermParser
                  dbg $ "term : other vars = "++show other
                  return $ if null other then first else T (Alt $ map Term $ first:other)


relationParser :: NameSpace m => Parser m Expr
relationParser = inpattern vexprParser >>= \left ->
                  do {
                     ; rel <- choice[reservedParser "eq" >> return Equal, reservedParser "ne" >> return NEqual]
                     ; right <- inpattern vexprParser
                     ; return (rel left right)
                     }
              <|> do {
                     ; reservedParser "in"
                     ; set <- (inpattern compParser <|> varParser) -- ??? do we allow patterns in sets or lists in the right part of `in`
                     ; return (In left set)
                     }
              <|> case left of
                    (Var n) -> return (Var n)
                    _       -> parserZero
              <?> "relation"

atomBool :: NameSpace m => Parser m Expr
atomBool =  dbg "atomBool" >> (parensParser boolParser
        <|> (reservedParser "True"  >> return (Bool True ))
        <|> (reservedParser "False" >> return (Bool False))
        <|> relationParser
        <?> "atomic bool")

boolParser :: NameSpace m => Parser m Expr
boolParser = exprp
-- boolParser = buildExpressionParser tableBool atomBool
--           <?> "boolean expression expected"

tableBool :: NameSpace m => [[Operator Text.Text PState m Expr]]
tableBool = [ [Prefix (dbg "try parse no" >> reservedParser "no"  >> return pNot)]
            , [Infix  (dbg "try parse &&" >> reservedOpParser "&&" >> return pAnd) AssocRight]
            , [Infix  (dbg "try parse ||" >> reservedOpParser "||"  >> return pOr)  AssocRight]
            ]

tableExpr ::  NameSpace m => [[Operator Text.Text PState m Expr]]
tableExpr = map (map makeOp) getBuiltInOps
-- tableExpr = [ [Infix  (dbg "try parse ^" >> op False "^") AssocRight]
--             , [Infix  (dbg "try parse *" >> op True "*") AssocLeft
--               ,Infix  (dbg "try parse /" >> op False "/") AssocLeft]
--             , [Infix  (dbg "try parse +" >> op True "+") AssocLeft
--               ,Infix  (dbg "try parse -" >> op False "-") AssocLeft]
--             , [Infix  (dbg "try parse :" >> op False ":") AssocRight]
--             , [Infix  (dbg "try parse ++" >> op True "++")  AssocRight]
--             ] where op assoc name = reservedOpParser name >> makeBinOp assoc name

makeOp :: NameSpace m => (BIFunc Identity, Int) -> Operator Text.Text PState m Expr
makeOp (bf,num) =
  if arity (fun bf) == 2
  then Infix ((dbg $ "try parse "++fname bf) >> reservedOpParser (fname bf) >> makeBinOp (commut bf) num (funct $ fun bf)) (assoc bf)
  else Prefix ((dbg $ "try parse "++fname bf) >> reservedOpParser (fname bf) >> makeUnOp (commut bf) num (funct $ fun bf))

makeBinOp :: NameSpace m => Int -> Int -> ([Expr] -> Identity Expr) -> Parser m (Expr -> Expr -> Expr)
makeBinOp 0 op _ = return (\x y -> Call (Sym (IL op)) [x, y])
makeBinOp 1 op _ = return (\x y -> Call (Sym (IL op)) (args op x++args op y))
           where args op t@(Call (Sym (IL o)) a) = if o==op then a else [t]
                 args _ t                   = [t]
makeBinOp 2 op f = return (\x y -> runIdentity (f [x,y]))

makeUnOp 0 op _ = return (\x -> Call (Sym (IL op)) [x])
makeUnOp 2 op f = return (\x -> runIdentity (f [x]))

vexprParser :: NameSpace m => Parser m Expr
vexprParser = exprp
--vexprParser = buildExpressionParser tableExpr atomExprParser

-- | Parser of program expressions
exprParser :: NameSpace m => Parser m Expr
exprParser = exprp
-- exprParser = dbg "parse expr" >> (
--                   try vexprParser
--               <|> try boolParser
--               <?> "expr")

-- | Parser of where-statement
whereParser :: NameSpace m => Bool -> Parser m Expr
whereParser ind = dbg "parse where" >> (
  do { dbg "before reservedParser where"
     ; when ind indented
     ; reservedParser "where"
     ; dbg "after reservedParser where"
     ; ts <- many1 (indentBlock (boolParser <?> "where condition"))
     ; dbg "where finished"
     ; return (foldr1 pAnd ts)
     }
  <|> return (Bool True))

-- | Parser of do-statement
doParser :: NameSpace m => Parser m [Command]
doParser = dbg "parse do" >> indentBlock (reservedParser "do"
                          >> many stmtParser)
                          <* reservedParserU "done"

fragParser :: NameSpace m => Parser m Expr
fragParser = dbg "parse fragment" >>  do
  string "{" >> whiteSpaceParser
  st <- getState
  putState st{ inFrag=True }
  res <- many stmtParser
  string "}" >> whiteSpaceParser
  args <- extvars <$> getState
  modifyState (\s -> s{ inFrag = inFrag st, extvars = extvars st })
  return $ Call (Fun (Var . snd <$> args) res) (Var . fst <$> args)

nameParser:: NameSpace m => Parser m String
nameParser = identifierParser >>= \name -> getLSymbol name >>= \case
  Just s  -> unexpected ("The identifier " ++ name ++ " is reserved as logical symbol.\n")
  Nothing -> return name

caseParser :: NameSpace m => Parser m [(Expr, Expr, [Command])]
caseParser = do
  pts <- many1 . indentBlock $ liftM2 (,) (inpattern vexprParser) (option (Bool True) (indentBlock $ whereParser False))
  dbg "done cases, find do"
  common <- whereParser True
  doblock <- doParser
  return $ map (\(a,c) -> (a, pAnd c common, doblock)) pts

-- | Parser of statements
stmtParser :: NameSpace m => Parser m Command
stmtParser = (dbg "parse statement " >> try (
         -- Parse an assigning instruction

         -- Parse an branching instruction
             do { -- reservedParser "if"
                ; c <- indentBlock (reservedParser "if" >> boolParser)
                ; b <- doParser
                ; return (Branch c b)
                }

         -- Parse an switching instruction
         <|> indentBlock (
             do { reservedParser "case"
                ; dbg "case found"
                ; e <- inpattern vexprParser
                ; dbg "case expression read"
                ; reservedParser "of"
                ; c <- whereParser True
                ; dbg "start read cases"
                ; cs <- mconcat <$> many1 caseParser -- liftM2 (,) vexprParser doParser
                ; return (Switch e c cs)
                }

          <|> (inpattern vexprParser >>= \p -> do{ -- ??? now we allow patterns in right part : [A, Var [_,Var [__]]] = [f [_, g[__]], B], where A,B,f,g are defined, Var must be defined by this expression
                (tp,g) <- (,) Select <$> ((reservedOpParser "=" >> toList <$> inpattern vexprParser)
                      <|> (reservedOpParser "<-" >> inpattern vexprParser))
                      <|> (,) Unord <$> (reservedOpParser "~=" >> inpattern vexprParser)
                      <|> (,) Append . List <$> many1 (reservedOpParser "<<" >> inpattern vexprParser)
                ; c <- whereParser True
                ; return (Assign tp p g c)
                } <|>
             -- Parse an acting instruction
             do { c <- whereParser True
                ; return (Apply p c)
                })
         <?> "Statement"))) <|> (dbg "no statement" >> parserZero)

-- | Program header parser
headerParser :: NameSpace m => Parser m Header
headerParser = try ( do { Var n <- varParser
              ; vs <- many varParser
              ; reservedOpParser "="
              ; return (Header n $ map (\case { Var x -> x} ) vs)
              } )

-- | Parser of programs
programParser :: NameSpace m => Parser m Program
programParser = do
  h <- headerParser
  stmts <- many stmtParser <* reservedParserU "done"
  return $ Program h stmts


mktuple [List [x]] = x
mktuple [List l] = Tuple l

mkset [List l] = Set l

ifthenelse a b c = IfElse a b c

isleft AssocLeft = True
isleft _ = False

topfxdef :: (BIFunc m,Int) -> Maybe (String,OpInfo)
topfxdef (i,op) = if isOp i && isleft (assoc i) && arity (fun i) == 1
                  then Just (fname i, OpInfo 0 [OpLast $ 2*prior i] $ OpFunH $ Sym (IL op))
                  else Nothing

toifxdef :: (BIFunc m,Int) -> Maybe (String,OpInfo)
toifxdef (i,op) = if isOp i && arity (fun i) == 2
                  then Just (fname i, OpInfo lpr [OpLast rpr] $ OpFunH $ Sym (IL op))
                  -- else if isOp i && arity (fun i) == 1
                  -- then Just (fname i, OpInfo lpr [OpLast rpr] $ OpFunH $ Sym (IL op))
                  else Nothing
                  where pr = prior i
                        (lpr,rpr) = if isleft (assoc i) then (2*pr, 2*pr+1) else (2*pr+1,2*pr)


pfxBuiltinS :: [(String,OpInfo)]
pfxBuiltinS = [
   ("(", OpInfo 0 [OpInfxSeq "," ")"] (OpCompileH mktuple)), -- parentheses
   ("[", OpInfo 0 [OpInfxSeq "," "]"] (OpCompileH head)), -- brackets
   ("{|", OpInfo 0 [OpInfxSeq "," "|}"] (OpCompileH mkset)), -- braces
   ("!", OpInfo 0 [OpLast 20] $ OpCompileH (\[a] -> pNot a)),
   ("if", OpInfo 0 [OpInfx "then", OpInfx "else", OpLast (-1)] (OpCompileH (\[a,b,c] -> IfElse a b c)))
   --("-", OpInfo 0 [OpLast 20] (OpCompileH (\[x] -> Call  )))
   --("` ", OpInfo 0 [OpInfx "then", OpInfx "else", OpLast 0] (OpCompileH ([a,b,c] -> IfElse a b c))),
  ] ++ mapMaybe topfxdef (concat getBuiltInOps)

createPtr [Var x,y] = Ptr x y
createPtr _ = error "left side of '&' must be a variable"

createRef [Var x,y] = Ref x y
createRef _ = error "left side of '@' must be a variable"

ifxBuiltinS :: [(String,OpInfo)]
ifxBuiltinS = [
  ("&", OpInfo maxPriority [OpLast $ maxPriority-1] $ OpCompileH createPtr),
  ("@", OpInfo maxPriority [OpLast $ maxPriority-1] $ OpCompileH createRef),
  ("&&", OpInfo 3 [OpLast 4] $ OpCompileH (\[a,b]-> pAnd a b)),
  ("||", OpInfo 2 [OpLast 3] $ OpCompileH (\[a,b]-> pOr a b)),
  ("|", OpInfo 2 [OpLast 3] $ OpCompileH (\[a,b]-> pAlt a b)),
  ("in", OpInfo 5 [OpLast 6] $ OpCompileH (\[a,b] -> In a b)),
  ("?", OpInfo 1 [OpInfx ":", OpLast 0] $ OpCompileH (\[a,b,c] -> IfElse a b c)),
  ("` ",OpInfo applyPriority [] $ OpCompileH (\[e] -> Call e [])),
  ("`",OpInfo 17 [OpInfx "` ", OpLastSeq "" 18] $ OpCompileH (\[a,f,List b] -> Call f (a:b)))
  --("-", OpInfo 0 [OpLast 20] (OpCompileH (\[x] -> Call  )))
  --("` ", OpInfo 0 [OpInfx "then", OpInfx "else", OpLast 0] (OpCompileH ([a,b,c] -> IfElse a b c))),
  ] ++ mapMaybe toifxdef (concat getBuiltInOps)

addTrie :: String -> Trie Char Int -> Trie Char Int
addTrie s = Structs.Trie.insert s (if isJust $ find isLetter s then 1 else 0)
addRsv s = Structs.Trie.insert s (-1)

getopparts [] = []
getopparts (OpInfx s:ops) = s:getopparts ops
getopparts (OpInfxSeq s u:ops) = s:u:getopparts ops
getopparts (OpLastSeq s _:ops) = s:getopparts ops
getopparts (OpLast _:ops) = getopparts ops

addIfxop :: (String,OpInfo) -> PStateM -> PStateM
addIfxop (s,o) st = st {
  infxopsM = Map.insert s o (infxopsM st),
  oppartsM = foldr addTrie (oppartsM st) (s:getopparts (opargs o))
  }

addPfxop :: (String,OpInfo) -> PStateM -> PStateM
addPfxop (s,o) st = st {
  prfxopsM = Map.insert s o (prfxopsM st),
  oppartsM = foldr addTrie (oppartsM st) (s:getopparts (opargs o))
  }

zeroStateM = PStateM Map.empty Map.empty $ foldr addTrie empty $ rsvOpNm

initStateM :: PStateM
initStateM = foldr addPfxop (foldr addIfxop zeroStateM ifxBuiltinS) pfxBuiltinS

------------------------------------------------------------------------------------------
-- Composing functions

-- | Convert a program term to list of terms
toList :: Expr -> Expr
toList x = List [x]

pAlt :: Expr -> Expr -> Expr
pAlt (Alt e1) (Alt e2) = Alt $ e1++e2
pAlt (Alt e1) x = Alt $ e1++[x]
pAlt x (Alt e2) = Alt $ x:e2
pAlt x y = Alt [x,y]

-- | Negate a given program term
pNot :: Expr -> Expr
pNot (Bool x)     = Bool (not x)
pNot (Not x)      = x
pNot (Equal x y)  = NEqual x y
pNot (NEqual x y) = Equal x y
pNot x            = Not x

-- | Take the logical and of given program terms
pAnd :: Expr -> Expr -> Expr
pAnd (Bool True) y     = y
pAnd x (Bool True)     = x
pAnd x@(Bool False) y  = x
pAnd x y@(Bool False)  = y
pAnd (And xs) (And ys) = And (xs ++ ys)
pAnd x (And ys)        = And (x : ys)
pAnd (And xs) y        = And (xs ++ [y])
pAnd x y               = And [x,y]

-- | Take the logical or of given program terms
pOr :: Expr -> Expr -> Expr
pOr x@(Bool True) y = x
pOr x y@(Bool True) = y
pOr (Bool False) y  = y
pOr x (Bool False)  = x
pOr (Or xs) (Or ys) = Or (xs ++ ys)
pOr x (Or ys)       = Or (x : ys)
pOr (Or xs) y       = Or (xs ++ [y])
pOr x y             = Or [x,y]

------------------------------------------------------------------------------------------
-- Auxiliary functions
