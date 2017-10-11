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
      LSymbol, LSymbols,
      initLSymbols,
      name, lsymbol, addLSymbol, isLSymbol
    )
where

-- External imports
import           Control.Exception
import           Data.Array
import           Data.Char
import qualified Data.Map          as M
import           Data.Maybe
import           System.IO

-- Internal imports
import           Database

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
  deriving (Read)

-- | Type for database of logical symbols
type LSymbols = (Array Int LSymbolInfo, M.Map String LSymbol)

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
       "Symbol {\n"
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

------------------------------------------------------------------------------------------
-- Database instances

-- | Database instance for logical symbols
instance Database String LSymbols IO where

  -- | Load the database of logical symbols from a given file
  load file = do
    content <- try (readFile file) :: IO (Either IOError FilePath)
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
  save db file = writeFile file $ show (elems $ fst db)

------------------------------------------------------------------------------------------
-- Functions

-- | Init a database of logical symbols
initLSymbols :: LSymbols
initLSymbols = (array (1,0) [], M.empty)

-- | Get the name of a logical symbol
name :: LSymbol -> LSymbols -> String
name (S s) db = name_ $ fst db ! s

-- | Get a logical symbol by the name or synonym
lsymbol :: String -> LSymbols -> LSymbol
lsymbol s db = fromJust $ M.lookup s (snd db)

-- | Add a new logical symbol defined by 'info' to a database
addLSymbol :: String   -- ^ name
           -> [String] -- ^ list of synonyms
           -> [String] -- ^ list of categories
           -> String   -- ^ description
           -> LSymbols -> LSymbols
addLSymbol n s c d db = do
  let (f,l) = bounds $ fst db
  let i = l-1
  let sym = S i
  let a = listArray (f,i) $ elems (fst db) ++ [Symbol i n s c d]
  let b = foldr (\x y -> M.insert x sym y) (M.insert n sym $ snd db) s
  (a,b)

-- | Does a string correspond to a logical symbol
isLSymbol :: String -> Bool
isLSymbol ('x':s) = all isDigit s
isLSymbol ('i':s) = all isDigit s
isLSymbol ('s':s) = all isDigit s
