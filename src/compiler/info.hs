{-# LANGUAGE FlexibleInstances #-}

{-|
Module      : Compiler.Info
Description : Representation of information structure in compiler
Copyright   : (c) Grigoriy Bokov, 2017
License     : GPL-3
Maintainer  : bokov@intsys.msu.ru
Stability   : experimental
Portability : POSIX
-}
module Compiler.Info
    (
      -- exports
      Info(..), Unit(..),
      initInfo,
      getRule,
      newProgVar,
      getInfoUnit, addInfoUnit,
      getProgChunks, addProgChunk
    )
where

-- External imports
import           Control.Monad.State
import qualified Data.Map            as Map

-- Internal imports
import           Compiler.Program
import           LSymbol
import           Rule
import           Term

-- | Type of table of information units
type Units = Map.Map LSymbol Unit

-- | Type of information structure in compiler
data Info = Info
  {
    rule   :: Rule,     -- ^ compiled rule
    varnum :: Int,      -- ^ number of first free program variable
    units  :: Units,    -- ^ table of information units
    chunks :: [Program] -- ^ list of program fragments
  }

initInfo :: Rule -> Info
initInfo rule = Info rule 1 Map.empty []

getRule :: State Info Rule
getRule = state $ \info -> (rule info, info)

newProgVar :: State Info LSymbol
newProgVar = state $ \info -> let n = varnum info in (P n, info {varnum = n + 1})

-- | Type of information units
data Unit = I Int
          | S LSymbol
          | T Term
          | IS [Int]
          | SS [LSymbol]
          | TS [Term]
          | EmptyUnit

class GetUnit a where
  getInfoUnit :: LSymbol -> State Info (Maybe a)

instance GetUnit Int where
  getInfoUnit sym = getUnit sym >>= \x -> state $ \info -> (fmap (\(I n) -> n) x, info)

instance GetUnit LSymbol where
  getInfoUnit sym = getUnit sym >>= \x -> state $ \info -> (fmap (\(S s) -> s) x, info)

instance GetUnit Term where
  getInfoUnit sym = getUnit sym >>= \x -> state $ \info -> (fmap (\(T t) -> t) x, info)

instance GetUnit [Int] where
  getInfoUnit sym = getUnit sym >>= \x -> state $ \info -> (fmap (\(IS ns) -> ns) x, info)

instance GetUnit [LSymbol] where
  getInfoUnit sym = getUnit sym >>= \x -> state $ \info -> (fmap (\(SS ss) -> ss) x, info)

instance GetUnit [Term] where
  getInfoUnit sym = getUnit sym >>= \x -> state $ \info -> (fmap (\(TS ts) -> ts) x, info)

getUnit :: LSymbol -> State Info (Maybe Unit)
getUnit sym = state $ \info -> (Map.lookup sym (units info), info)

class AddUnit a where
  addInfoUnit :: LSymbol -> a -> State Info ()

instance AddUnit Int where
  addInfoUnit sym n = addUnit sym $ I n

instance AddUnit LSymbol where
  addInfoUnit sym s = addUnit sym $ S s

instance AddUnit Term where
  addInfoUnit sym t = addUnit sym $ T t

instance AddUnit [Int] where
  addInfoUnit sym ns = addUnit sym $ IS ns

instance AddUnit [LSymbol] where
  addInfoUnit sym ss = addUnit sym $ SS ss

instance AddUnit [Term] where
  addInfoUnit sym ts = addUnit sym $ TS ts

addUnit :: LSymbol -> Unit -> State Info ()
addUnit sym unit = modify (\info -> info { units = Map.insert sym unit (units info) })

getProgChunks :: State Info [Program]
getProgChunks = state $ \info -> (chunks info, info)

addProgChunk :: Program -> State Info ()
addProgChunk prog = modify (\info -> info { chunks = chunks info ++ [prog] })
