module Language.Futhark.Interpreter.FFI
  ( ValueRef (..),
    EFI (..)
  )
where

import Control.Monad.IO.Class (MonadIO)
import Language.Futhark.Core (Name)
import Language.Futhark.Interpreter.FFI.UIDs (InterfaceUID, MonadUIDSource)

class ValueRef v where
  -- | Indexes an array
  index :: [Int] -> v -> v
  -- | Projects a record
  project :: Name -> v -> v
  -- | Destructs a variant
  destruct :: Int -> v -> v

-- | External Function Interface
data EFI v h = EFI
  { -- | A unique ID
    uid :: InterfaceUID,
    -- | The functions callable from the FFI
    functions :: [Name],
    -- | Pushes changes to an external value to its interface
    push :: forall m. (MonadIO m, MonadUIDSource m) => h -> m h,
    -- | Receives a value from an interface
    fetch :: forall m. (MonadIO m, MonadUIDSource m) => h -> m v,
    -- | Calls a function on the foreign interface
    call :: forall m. (MonadIO m, MonadUIDSource m) => Name -> [Either v h] -> m h
  }
