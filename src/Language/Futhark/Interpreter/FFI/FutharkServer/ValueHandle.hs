module Language.Futhark.Interpreter.FFI.FutharkServer.ValueHandle
  ( ValueHandle (typeLayout),
    Direction (..),
    ValueReference (..),
    mkValueHandle,
    withValueHandle,
    mkValueHandles,
    withValueHandles,
    index,
    project,
    destruct,
  )
where

import Control.Monad (forM, zipWithM)
import Control.Monad.IO.Class (MonadIO (liftIO))
import Data.IORef (IORef, newIORef, readIORef, mkWeakIORef)
import Language.Futhark.Core (Name)
import Language.Futhark.Interpreter.FFI (ValueRef (..))
import Language.Futhark.Interpreter.FFI.FutharkServer.TypeLayout (TypeLayout (..))
import Language.Futhark.Interpreter.FFI.UIDs (ValueUID, MonadUIDSource, getUIDs)
import Language.Futhark.Interpreter.FFI.Util.AtomicList (AtomicList, prepend)

-- | An external value
data ValueHandle = ValueHandle
  { typeLayout :: TypeLayout,
    vidRef :: IORef ValueUID
  }

mkValueHandle :: (MonadIO m, MonadUIDSource m) => AtomicList ValueUID -> (ValueUID -> IO TypeLayout) -> m ValueHandle
mkValueHandle l c = head <$> mkValueHandles l 1 (fmap (:[]) . c . head)

mkValueHandles :: (MonadIO m, MonadUIDSource m) => AtomicList ValueUID -> Word -> ([ValueUID] -> IO [TypeLayout]) -> m [ValueHandle]
mkValueHandles l n c = do
  vids <- getUIDs n
  liftIO $ do
    tls <- c vids
    rs <- forM vids newIORef
    _ <- zipWithM (\r vid -> mkWeakIORef r $ prepend vid l) rs vids
    pure $ zipWith ValueHandle tls rs

withValueHandle :: MonadIO m => ValueHandle -> (ValueUID -> m a) -> m a
withValueHandle h f = withValueHandles [h] $ f . head

withValueHandles :: MonadIO m => [ValueHandle] -> ([ValueUID] -> m a) -> m a
withValueHandles hs f = forM hs (liftIO . readIORef . vidRef) >>= f

data Direction
  = ArrayElement [Int]
  | RecordField Name
  | VariantField Int
  deriving (Show, Eq, Ord)

-- | A reference to an external value
data ValueReference = ValueReference ValueHandle [Direction]

instance ValueRef ValueReference where
  -- | Indexes into a `ValueHandle`, assuming it is an array of appropriate rank
  index :: [Int] -> ValueReference -> ValueReference
  index is (ValueReference h (ArrayElement is' : d)) = ValueReference h $ ArrayElement (is' ++ is) : d
  index is (ValueReference h d) = ValueReference h $ ArrayElement is : d

  -- | Projects a `ValueHandle`, assuming it is a record of appropriate type
  project :: Name -> ValueReference -> ValueReference
  project f (ValueReference h d) = ValueReference h $ RecordField f : d

  -- | Fetches a field from `ValueHandle`, assuming it is a variant of appropriate size
  destruct :: Int -> ValueReference -> ValueReference
  destruct f (ValueReference h d) = ValueReference h $ VariantField f : d
