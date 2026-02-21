module Language.Futhark.Interpreter.FFI
  ( Function (..),
    Interface (..),
  )
where

import Data.Map qualified as M
import Language.Futhark.Core (Name)
import Language.Futhark.Interpreter.FFI.Values
import Prelude hiding (lookup, read)

data Function
  = Function [Type] Type
  deriving (Show, Eq)

newtype Interface
  = Interface (M.Map Name Function)
  deriving Show
