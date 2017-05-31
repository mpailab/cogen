module Compiler.Tree.Database
    (
      init, load, save
    ) where

import Compiler.Tree
import Terms

data Database = Empty

init :: String -> Database
load :: Database -> TermSym -> (Database,Tree)
save :: Database -> TermSym -> Tree -> Database
