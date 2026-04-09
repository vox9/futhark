module Language.Futhark.Interpreter.FFI.ValueTree
  ( ValueRef,
    Direction (..),
    pin,
    metadata,
    mkRoot,
    mkChild,
  )
where

import Control.Monad.IO.Class (MonadIO (liftIO))
import Data.IORef (IORef, newIORef, mkWeakIORef, atomicModifyIORef)
import Data.Map qualified as M
import GHC.Weak (Weak)
import Language.Futhark.Core (Name)
import Language.Futhark.Interpreter.FFI.Monad (FFIM)
import Language.Futhark.Interpreter.FFI.UIDs (getUID, ValueUID)

data Direction = ArrayIndex [Int] | RecordField Name | VariantValue Int
  deriving (Eq, Ord, Show)

data Node = Node (M.Map Direction (Weak (IORef Node)))

data ValueRef t = ValueRef t ValueUID (IORef Node)

pin :: [ValueRef t] -> ([ValueUID] -> a) -> a
pin vs f = f $ map (\(ValueRef _ vid _) -> vid) vs

metadata :: ValueRef t -> t
metadata (ValueRef t _ _) = t

mkRoot :: t -> (ValueUID -> FFIM (IO ())) -> FFIM (ValueRef t)
mkRoot t c = do
  vid <- getUID
  d <- c vid
  liftIO $ do
    r <- newIORef $ Node M.empty
    _ <- mkWeakIORef r d
    pure $ ValueRef t vid r

mkChild :: ValueRef t -> Direction -> t -> (ValueUID -> FFIM (IO ())) -> FFIM (ValueRef t)
mkChild (ValueRef _ _ p) d t c = do
  (ValueRef _ vid c') <- mkRoot t c
  liftIO $ do
    w <- mkWeakIORef c' $ pure ()
    atomicModifyIORef p $ (,()) . \(Node m) -> Node $ M.insert d w m
    pure $ ValueRef t vid c'
