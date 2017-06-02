module Compiler.Tree
    (
      Tree(..)
    ) where

import           Terms

type Edge = ([Term],Tree)
type TerminalNode = [Term]

data Tree =
    Terminal TerminalNode
  | Switch Term [Edge]
  | Branch [Edge]
  deriving(Eq, Read, Show)
