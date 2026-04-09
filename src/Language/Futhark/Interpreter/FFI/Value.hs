module Language.Futhark.Interpreter.FFI.Value
  ( FFIValue,
    GenFFIValue,
    doOp,
    indexArray,
    projectRecord,
    destroyVariant,
    call,
  )
where

import Language.Futhark.Core (Name)
import Language.Futhark.Interpreter.FFI.Class qualified as C
import Language.Futhark.Interpreter.FFI.Monad (FFIM)
import Language.Futhark.Interpreter.FFI.ValueTree (ValueRef)
import Control.Monad (forM)

data FFIValue i = C.FFI i => FFIValue i (FFIM (ValueRef (C.Metadata i)))

type GenFFIValue = forall i. C.FFI i => FFIValue i

doOp :: (i -> ValueRef (C.Metadata i) -> FFIM (ValueRef (C.Metadata i))) -> FFIValue i -> FFIValue i
doOp f (FFIValue i m) = FFIValue i $ m >>= f i

indexArray :: C.FFI i => [Int] -> FFIValue i -> FFIValue i
indexArray i = doOp $ C.indexArray i

projectRecord :: C.FFI i => Name -> FFIValue i -> FFIValue i
projectRecord i = doOp $ C.projectRecord i

destroyVariant :: C.FFI i => Int -> FFIValue i -> FFIValue i
destroyVariant i = doOp $ C.destroyVariant i

call :: C.FFI i => Name -> [FFIValue i] -> i -> FFIM [FFIValue i]
call n ps i = do
  ps' <- forM ps $ \(FFIValue _ m) -> m
  pure $ map (FFIValue i) $ C.call n ps' i
