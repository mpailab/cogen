{-# LANGUAGE ScopedTypeVariables #-}

-- External imports
import           Control.Concurrent
import           Control.Monad
import           Data.Typeable
import           Prelude            hiding (init)
import           System.Directory
import           System.IO

-- Internal imports
import           Compiler
import           Global
import           Lsymbol
import           Program.Parser
import           Program
import           Rule

-- | Test implementation
test :: Integer -> IO Bool
test num = case num of
  1 -> make $ do
    lsyms_db <- load "database/lsymbols.db"
    prog_str <- liftM readFile "database/programs/compiler.coral"
    prog <- parse prog lsyms_db
    writeFile "database/programs/qq.coral" (write prog lsyms_db)
    return True

  2 -> do
    rulesFile <- readFile "database/rules.db"
    let rules = read rulesFile :: [Rule]
    forM_ rules compile
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
