module Language.Futhark.Interpreter.FFI.Server.Interface
  ( EntryPoint (..),
    ServerInterface (..),
  )
where

import Data.Map qualified as M
import Futhark.Server qualified as S
import Language.Futhark.Interpreter.FFI.Server.TypeLayout (TypeLayout)

data EntryPoint = EntryPoint [TypeLayout] [TypeLayout]
  deriving (Eq, Ord, Show)

newtype ServerInterface = ServerInterface (M.Map S.EntryName EntryPoint)
  deriving (Show, Semigroup, Monoid)
