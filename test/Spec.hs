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
import           Program.Parser

-- | Test implementation
test :: Integer -> IO Bool
test num = case num of
  1 -> make $ do
    lsym_db <- load "database/lsymbols.db"
    prog_str <- liftIO $ readFile "database/programs/before.coral"
    let prog = parse prog_str lsym_db :: Program
    liftIO $ writeFile "database/programs/after.coral" (write prog lsym_db)
    return True

-- | Run test with number num
runTest :: Integer -> IO ()
runTest num = do
  putStr (show num ++ " ... ")
  res <- test num
  if res then putStrLn "ok" else putStrLn "fail"

main :: IO ()
main = do
  createDirectoryIfMissing False "tmp"
  putStrLn "Run tests:"
  let test_num = 1 in mapM runTest [1..test_num]
  putStrLn "Done"
  removeDirectoryRecursive "tmp"
