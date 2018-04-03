{-# LANGUAGE ScopedTypeVariables #-}

-- External imports
import           Control.Concurrent
import           Control.Monad
import           Control.Monad.State
import           Data.Typeable
import           Prelude             hiding (init)
import           System.Directory
import           System.IO

-- Internal imports
import           Global
import           LSymbol
import           Program
import           Program.Database
import           Program.Parser
import           Program.Writer

-- | Test implementation
test :: Integer -> Global Bool
test 1 = do
    load "database/lsymbols.db" >>= setLSymbols
    let file = "database/programs/before.coral"
    prog_str <- liftIO $ readFile file
    prog :: Program <- parse prog_str file
    liftIO . writeFile "database/programs/after.coral" =<< write prog
    return True

test 2 = do
    load "database/lsymbols.db" >>= setLSymbols
    let file = "database/programs/before1.coral"
    prog_str <- liftIO $ readFile file
    prog :: Program <- parse prog_str file
    liftIO . writeFile "database/programs/after1.coral" =<< write prog
    return True

-- | Run test with number num
runTest :: Integer -> IO ()
runTest num = do
  putStr (show num ++ " ... ")
  res <- make (test num)
  if res then putStrLn "ok" else putStrLn "fail"

main :: IO ()
main = do
  createDirectoryIfMissing False "tmp"
  putStrLn "Run tests:"
  let test_num = 2 in mapM runTest [1..test_num]
  putStrLn "Done"
  removeDirectoryRecursive "tmp"
