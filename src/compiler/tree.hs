{-|
Module      : Compiler.Tree
Description : Representation of compiler's decision tree
Copyright   : (c) Grigoriy Bokov, 2017
                  Gleb Kalachev, 2017
License     : GPL-3
Maintainer  : sample@email.com
Stability   : experimental
Portability : POSIX

Модуль содержит реализацию разрешающего дерева компилятора приемов, а также
интерфейсы для работы с ним.
-}
module Compiler.Tree
    (
      -- exports
      Tree(..)
    )
where

  -- Internal imports
import           Term

type Edge = ([Term],Tree)
type TerminalNode = [Term]

data Tree
  = Terminal TerminalNode
  | Switch Term [Edge]
  | Branch [Edge]
  deriving(Eq, Read, Show)
