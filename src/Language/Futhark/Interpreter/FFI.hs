module Language.Futhark.Interpreter.FFI
  ( C.FFI,
    FFI.FFIValue,
    FFI.GenFFIValue,
    Direction,
    FFIManagerM,
    pull,
    call,
  )
where

import Data.Map qualified as M
import Language.Futhark.Core (Name)
import Language.Futhark.Interpreter.FFI.Class qualified as C
import Language.Futhark.Interpreter.FFI.Value qualified as FFI
import Language.Futhark.Interpreter.FFI.ValueTree (Direction)
import Language.Futhark.Interpreter (Value)
import Control.Monad.State (State, gets)

newtype InterfaceMap = InterfaceMap (forall i. C.FFI i => M.Map Name i)

newtype FFIManagerM a = FFIManagerM (State InterfaceMap a)
  deriving (Functor, Applicative, Monad)

interfaceOf :: Name -> FFIManagerM (Maybe i)
interfaceOf n = FFIManagerM $ gets $ \(InterfaceMap m) -> M.lookup n m

push :: i -> Value -> FFI.FFIValue i
push = undefined

pull :: i -> FFI.FFIValue i -> FFIManagerM Value
pull = undefined

call :: Name -> [Value] -> FFIManagerM (Maybe (FFI.FFIValue i))
call n ps = do
  i <- interfaceOf n
  case i of
    Just i' -> pure $ Just $ FFI.call n (map (push i') ps) i'
    Nothing -> pure Nothing
