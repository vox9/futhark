module Language.Futhark.Interpreter.FFI.FutharkServer.TypeLayout
  ( TypeLayout (..),
    typeName,
    primArray,
  )
where

import Data.Map qualified as M
import Futhark.Data (PrimType (..))
import Futhark.Server qualified as S

data TypeLayout
  = TLPrimitive PrimType
  | TLArray S.TypeName Word TypeLayout
  | TLRecord S.TypeName [(S.FieldName, TypeLayout)]
  | TLSum S.TypeName (M.Map S.VariantName [TypeLayout])
  | TLOpaque S.TypeName
  deriving (Show, Eq, Ord)

typeName :: TypeLayout -> S.TypeName
typeName (TLPrimitive I8) = "i8"
typeName (TLPrimitive I16) = "i16"
typeName (TLPrimitive I32) = "i32"
typeName (TLPrimitive I64) = "i64"
typeName (TLPrimitive U8) = "u8"
typeName (TLPrimitive U16) = "u16"
typeName (TLPrimitive U32) = "u32"
typeName (TLPrimitive U64) = "u64"
typeName (TLPrimitive F16) = "f16"
typeName (TLPrimitive F32) = "f32"
typeName (TLPrimitive F64) = "f64"
typeName (TLPrimitive Bool) = "b"
typeName (TLArray n _ _) = n
typeName (TLRecord n _) = n
typeName (TLSum n _) = n
typeName (TLOpaque n) = n

primArray :: TypeLayout -> Bool
primArray (TLPrimitive {}) = True
primArray (TLArray _ _ e) = primArray e
primArray _ = False
