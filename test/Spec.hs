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
import           LSymbol
import           Rule
import           Term

-- | Test implementation
test :: Integer -> IO Bool
test num = case num of
  1 -> do
    let sym = Plus
    let thrm = Forall :> [Var (X 1), Var (X 2), Var (X 3), Equivalence :> [Equal :> [Plus :> [Var (X 1), Var (X 2)], Plus :> [Neg :> [Var (X 1)], Var (X 3)]], Equal :> [Var (X 2), Neg :> [Var (X 3)]]]]
    let h = Const LeftToRight
    let rule = Rule sym thrm h [Level :> [Var (X 3)]] [Var (X 3)] [Var (X 3)]
    writeFile "tmp/test1.txt" (show rule)
    writeFile "tmp/draw.txt" (drawTerm thrm)
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
  let test_num = 2 in mapM runTest [1..test_num]
  putStrLn "Done"
  removeDirectoryRecursive "tmp"
