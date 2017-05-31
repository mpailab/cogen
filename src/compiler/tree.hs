module Compiler.Tree
    (
      Tree(..),
      save, load
    ) where

import Terms

type Edge = [STerm]
type TerminalNode = [STerm]

data Tree =
    Terminal TerminalNode
  | Switch STerm [Edge]
  | Branch [Edge]
