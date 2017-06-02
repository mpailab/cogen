{-# LANGUAGE ScopedTypeVariables #-}

-- External imports
import           Control.Concurrent
import           System.Directory

-- Internal imports
import           Compiler.Tree
import           Compiler.Tree.Database
import           Lib
import           Rule
import           Terms

-- | Test implementation
test :: Integer -> IO Bool
test num = case num of
  1 -> do
    db <- initialize "ruletrees.db"
    let tree = Switch ("level"&[Var "x1"]) [([], Terminal ["f"&[Var "x"]]),([],Terminal ["g"&[Var "y"]])]
    finalize "ruletrees.db" (save db "example" tree)
    removeFile "ruletrees.db"
    return True

  2 -> do
    rulesFile <- readFile "pls_hs.txt"
    let plsRules::[Rule] = read rulesFile
    print plsRules
    return True

-- | Run test with number num
runTest :: Integer -> IO ()
runTest num = do
  putStr (show num ++ " ... ")
  res <- test num
  if res then putStrLn "ok" else putStrLn "fail"

main :: IO ()
main = do
  putStrLn "Run tests:"
  let test_num = 1 in mapM runTest [1..test_num]
  putStrLn "Done"
