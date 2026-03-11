module Language.Futhark.Interpreter.FFI.Server.TypeLayout
  ( TypeLayout (..)
  )
where

import Data.Map qualified as M
import Futhark.Server qualified as S
import Language.Futhark.Interpreter.FFI.ExID
import Language.Futhark.Interpreter.FFI.Values
import Prelude hiding (init)

data TypeLayout
  = TLPrimitive PrimitiveType
  | TLArray Int ExTypeID
  | TLRecord [(S.FieldName, ExTypeID)]
  | TLSum (M.Map S.VariantName [ExTypeID])
  | TLOpaque
  deriving (Show, Eq, Ord)
