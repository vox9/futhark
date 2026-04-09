{-# LANGUAGE TypeFamilies #-}

module Language.Futhark.Interpreter.FFI.Class
  ( FFI (..),
  )
where

import Language.Futhark.Core (Name)
import Language.Futhark.Interpreter.FFI.Monad (FFIM)
import Language.Futhark.Interpreter.FFI.ValueTree (ValueRef)
import Language.Futhark.Syntax (PrimValue)

class FFI i where
  type Metadata i

  call :: Name -> [ValueRef (Metadata i)] -> i -> [FFIM (ValueRef (Metadata i))]
  indexArray :: [Int] -> i -> ValueRef (Metadata i) -> FFIM (ValueRef (Metadata i))
  projectRecord :: Name -> i -> ValueRef (Metadata i) -> FFIM (ValueRef (Metadata i))
  destroyVariant :: Int -> i -> ValueRef (Metadata i) -> FFIM (ValueRef (Metadata i))
  pushPrimitives :: [PrimValue] -> i -> [FFIM (ValueRef (Metadata i))]
  pullPrimitives :: [ValueRef (Metadata i)] -> i -> [FFIM PrimValue]

  pushPrimitive :: PrimValue -> i -> FFIM (ValueRef (Metadata i))
  pushPrimitive v i = head $ pushPrimitives [v] i

  pullPrimitive :: ValueRef (Metadata i) -> i -> FFIM PrimValue
  pullPrimitive r i = head $ pullPrimitives [r] i
