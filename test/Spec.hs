{-# LANGUAGE ScopedTypeVariables #-}

-- External imports
import           Control.Concurrent
import           Prelude                hiding (init)
import           System.Directory
import           System.IO

-- Internal imports
import           Compiler.Tree
import           Compiler.Tree.Database
import           Rule
import           Term

-- | Test implementation
test :: Integer -> IO Bool
test num = case num of
  1 -> do
    let tree_before = Switch ("level"&[Var "x1"]) [([], Terminal ["f"&[Var "x"]]),([],Terminal ["g"&[Var "y"]])]
    let file = "tmp/test1.txt"
    saveTree "example" tree_before file
    tree_after <- loadTree "example" file
    return (tree_before == tree_after)

  2 -> do
    rulesFile <- readFile "database/rules.db"
    let rules::[Rule] = read rulesFile
    writeFile "tmp/test2.txt" $! show rules
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
