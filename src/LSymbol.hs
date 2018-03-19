{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeSynonymInstances  #-}

{-|
Module      : LSymbol
Description : Logical symbols
Copyright   : (c) Grigoriy Bokov, 2017
License     : GPL-3
Maintainer  : bokov@intsys.msu.ru
Stability   : experimental
Portability : POSIX

Logical symbols are user-defined symbols used for denoting specific notions (relations between objects, operations over them, names of objects, logical connectives, quantifiers and etc.).

Logical symbols are numbered with positive integers. In fact, every logical symbol is a positive integer which is its identifier. The name of symbol and other information contain in a database of logical symbols loaded on startup.

In order to add new logical symbol need:
  1. from the outside: add symbol specification in the loaded database.
  2. from the inside: call the function add.
-}
module LSymbol
    (
      -- exports
      addLSymbol,
      Base,
      checkLSymbol,
      getLSymbol,
      getLSymbols,
      initLSymbols,
      listLSymbols,
      LSymbol,
      LSymbols,
      LTerm,
      nameLSymbol,
      newLSymbols,
      setLSymbols
    )
where

-- External imports
import           Control.Exception
import           Control.Monad.IO.Class
import           Data.Array
import           Data.Char
import qualified Data.Map               as M
import           Data.Maybe
import           System.IO

-- Internal imports
import           Database
import           Term

------------------------------------------------------------------------------------------
-- Data and type declaration

-- | Type of logical symbols
data LSymbol = X Int                    -- ^ variable
             | I Int                    -- ^ integer constant
             | S Int                    -- ^ user-defined logical symbol
             deriving (Eq, Ord)

-- | Type for informational structure of logical symbol
data LSymbolInfo = Symbol
  {
    id_          :: Int,      -- ^ identifier
    name_        :: String,   -- ^ name
    synonyms_    :: [String], -- ^ list of synonyms
    categories_  :: [String], -- ^ list of categories
    description_ :: String    -- ^ description
  }

-- | Type for database of logical symbols
type LSymbols = (Array Int LSymbolInfo, M.Map String LSymbol)

-- | Init a database of logical symbols
initLSymbols :: LSymbols
initLSymbols = (array (1,0) [], M.empty)

-- | Type of logical terms
type LTerm = Term LSymbol

-- | Base class of logical symbols
class Monad m => Base m where
  {-# MINIMAL getLSymbols, setLSymbols #-}

  -- | Get a database of logical symbols
  getLSymbols :: m LSymbols

  -- | Set a database of logical symbols
  setLSymbols :: LSymbols -> m ()

  -- | Init a new database of logical symbols
  newLSymbols :: m LSymbols
  newLSymbols = let db = initLSymbols in setLSymbols db >> return db

  listLSymbols :: m [String]
  listLSymbols = getLSymbols >>= return . (map name_) . elems . fst

  -- | Get the name of a logical symbol
  nameLSymbol :: LSymbol -> m String
  nameLSymbol (S n) = getLSymbols >>= \db -> return (name_ $ fst db ! n)

  -- | Get a logical symbol by the name or synonym
  getLSymbol :: String -> m (Maybe LSymbol)
  getLSymbol str = M.lookup str . snd <$> getLSymbols

  -- | Add a new logical symbol defined by 'info' to a database
  addLSymbol :: String   -- ^ name
             -> [String] -- ^ list of synonyms
             -> [String] -- ^ list of categories
             -> String   -- ^ description
             -> m ()
  addLSymbol n s c d = do
    db <- getLSymbols
    let (f,l) = bounds $ fst db
    let i = l-1
    let sym = S i
    let a = listArray (f,i) $ elems (fst db) ++ [Symbol i n s c d]
    let b = foldr (\x y -> M.insert x sym y) (M.insert n sym $ snd db) s
    setLSymbols (a,b)

  -- | Does a string correspond to a logical symbol
  checkLSymbol :: String -> m Bool
  checkLSymbol s = (isJust . M.lookup s . snd) <$> getLSymbols

------------------------------------------------------------------------------------------
-- Show instances

-- Show instance for logical symbols
instance Show LSymbol where
  show (X i) = 'x' : show i
  show (I i) = 'i' : show i
  show (S i) = 's' : show i

-- Show instance for informational structure of logical symbols
instance Show LSymbolInfo where
  show (Symbol i n s c d) =
       "\nSymbol {\n"
    ++ "  id          = " ++ show i ++ ",\n"
    ++ "  name        = " ++ show n ++ ",\n"
    ++ "  synonyms    = " ++ show s ++ ",\n"
    ++ "  categories  = " ++ show c ++ ",\n"
    ++ "  description = " ++ show d ++ "\n}"

------------------------------------------------------------------------------------------
-- Read instances

-- | Read instance for logical symbols
instance Read LSymbol where
  readsPrec p r =  [ (X (read x), "") | ('x':x,"") <- lex r, all isDigit x ]
                ++ [ (I (read x), "") | ('i':x,"") <- lex r, all isDigit x ]
                ++ [ (S (read x), "") | ('s':x,"") <- lex r, all isDigit x ]

-- | Read instance for informational structure of logical symbols
instance Read LSymbolInfo where
  readsPrec p s0 =  [ (Symbol i n s c d, s12) |
                      ("Symbol", s1) <- lex s0,
                      ("{", s2) <- lex s1,
                      (i, s3) <- f "id" p s2, (",", s4) <- lex s3,
                      (n, s5) <- f "name" p s4, (",", s6) <- lex s5,
                      (s, s7) <- f "synonyms" p s6, (",", s8) <- lex s7,
                      (c, s9) <- f "categories" p s8, (",", s10) <- lex s9,
                      (d, s11) <- f "description" p s10,
                      ("}", s12) <- lex s11 ]
    where
      f pat p s0 = [ (x, s3) | (pat, s1) <- lex s0,
                               ("=", s2) <- lex s1,
                               (x, s3) <- readsPrec p s2 ]

------------------------------------------------------------------------------------------
-- Database instances

-- | Database instance for logical symbols
instance MonadIO m => Database String LSymbols m where

  -- | Load the database of logical symbols from a given file
  load file = do
    content <- liftIO (try (readFile file) :: IO (Either IOError FilePath))
    case content of
      Left _        -> fail ( "Can't read database of logical symbols from the file \'"
                              ++ file ++ "\'")
      Right content -> return (a,b)
        where
          a = listArray (1, length sis) sis
          b = foldr f M.empty sis
          sis = read content :: [LSymbolInfo]
          f si db = let i = id_ si
                        n = name_ si
                        s = synonyms_ si
                        sym = S i
                    in foldr (\x y -> M.insert x sym y) (M.insert n sym db) s

  -- | Save a database of logical symbols to a given file
  save db file = (liftIO . writeFile file) $ show (elems $ fst db)

------------------------------------------------------------------------------------------
-- Functions
