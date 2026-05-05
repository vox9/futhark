module Language.Futhark.Interpreter.FFI.Generic
  ( FFIValue,
    push,
    fetch,
    call,
    index,
    project,
    destruct
  )
where

import Control.Monad.IO.Class (MonadIO)
import Data.Typeable (Typeable, cast)
import Language.Futhark.Core (Name)
import Language.Futhark.Interpreter.FFI qualified as FFI
import Language.Futhark.Interpreter.FFI.UIDs (MonadUIDSource)

data FFIValue v = forall h. (Typeable h, FFI.ValueRef h) => FFIValue (FFI.EFI v h) h

push :: (MonadIO m, MonadUIDSource m) => FFIValue v -> m (FFIValue v)
push (FFIValue i h) = FFIValue i <$> FFI.push i h

fetch :: (MonadIO m, MonadUIDSource m) => FFIValue v -> m v
fetch (FFIValue i h) = FFI.fetch i h

call :: (MonadIO m, MonadUIDSource m, Typeable h, FFI.ValueRef h) => FFI.EFI v h -> Name -> [Either v (FFIValue v)] -> m (FFIValue v)
call i n ps = FFIValue i <$> (mapM ensureOnInterface ps >>= FFI.call i n)
  where
    ensureOnInterface (Left v) = pure $ Left v
    ensureOnInterface (Right (FFIValue i' h)) =
      if FFI.uid i == FFI.uid i' then maybe (Left <$> FFI.fetch i' h) (pure . Right) $ cast h
                                 else Left <$> FFI.fetch i' h

index :: [Int] -> FFIValue v -> FFIValue v
index is (FFIValue i h) = FFIValue i $ FFI.index is h

project :: Name -> FFIValue v -> FFIValue v
project f (FFIValue i h) = FFIValue i $ FFI.project f h

destruct :: Int -> FFIValue v -> FFIValue v
destruct f (FFIValue i h) = FFIValue i $ FFI.destruct f h

--destruct :: FFIValue v -> [FFIValue v]
--destruct (FFIValue i h) = map (FFIValue i) $ FFI.destruct h
