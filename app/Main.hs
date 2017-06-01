module Main where

import Lib
import Terms
import Compiler.Tree
import Compiler.Tree.Database

main :: IO ()
main = do
    db <- initialize "ruletrees.db"
    let exampleTree = Switch ("level"&[Var "x1"]) [([], Terminal ["f"&[Var "x"]]),([],Terminal ["g"&[Var "y"]])]
    let db1 = save db "example" exampleTree
    finalize "ruletrees.db" db1
