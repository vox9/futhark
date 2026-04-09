module Language.Futhark.Interpreter.FFI.Monad
  ( FFIM,
  )
where

import Control.Monad.IO.Class (MonadIO)
import Language.Futhark.Interpreter.FFI.UIDs (UIDSourceT, MonadUIDSource)

newtype FFIM a = FFIM (UIDSourceT IO a)
  deriving (Functor, Applicative, Monad, MonadIO, MonadUIDSource)
