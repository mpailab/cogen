{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import Lib
import Terms
import Compiler.Tree
import Compiler.Tree.Database
import Control.Concurrent
import Rule

main :: IO ()
main = do
    db <- initialize "ruletrees.db"
    let exampleTree = Switch ("level"&[Var "x1"]) [([], Terminal ["f"&[Var "x"]]),([],Terminal ["g"&[Var "y"]])]
    let db1 = save db "example" exampleTree
    rulesFile <- readFile "pls_hs.txt"
    let plsRules::[Rule] = read rulesFile
    print plsRules
    finalize "ruletrees.db" db1
