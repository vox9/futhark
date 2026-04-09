module Language.Futhark.Interpreter.FFI.Server.TypeLayout
  ( TypeLayout (..),
  )
where

import Data.Map qualified as M
import Futhark.Data (PrimType)
import Futhark.Server qualified as S
import Prelude hiding (init)

data TypeLayout
  = TLPrimitive PrimType
  | TLArray TypeLayout
  | TLRecord [(S.FieldName, TypeLayout)]
  | TLSum (M.Map S.VariantName [TypeLayout])
  | TLOpaque
  deriving (Show, Eq, Ord)
