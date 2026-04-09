{-# LANGUAGE TypeFamilies #-}

module Language.Futhark.Interpreter.FFI.Server
  ( FutharkServer (..),
    startServer,
    FutharkServerM,
    server,
    interface,
    runFutharkServerM,
  )
where

import Control.Monad.Reader (MonadIO (liftIO), MonadReader, ReaderT (runReaderT), asks)
import Data.Map qualified as M
import Data.Text qualified as T
import Futhark.Server qualified as S
import Futhark.Util (isEnvVarAtLeast)
import Language.Futhark.Core (Name, nameToText)
import Language.Futhark.Interpreter.FFI.Class (FFI (..))
import Language.Futhark.Interpreter.FFI.Monad (FFIM)
import Language.Futhark.Interpreter.FFI.Server.Explorer (exploreProgram)
import Language.Futhark.Interpreter.FFI.Server.Interface (ServerInterface (..), EntryPoint (..))
import Language.Futhark.Interpreter.FFI.Server.TypeLayout (TypeLayout (..))
import Language.Futhark.Interpreter.FFI.UIDs (uid, ValueUID)
import Language.Futhark.Interpreter.FFI.ValueTree (ValueRef, pin, mkRoot, mkChild, Direction (..), metadata)
import Language.Futhark.Syntax (PrimValue)
import Prelude hiding (init)
import Control.Monad (void)

-- Server and function calling
data FutharkServer = FutharkServer
  { fsInfo :: FutharkServerInfo
  }

toVarName :: ValueUID -> T.Text
toVarName = ("v"<>) . T.show . uid

instance FFI FutharkServer where
  type Metadata FutharkServer = TypeLayout
  
  call :: Name -> [ValueRef TypeLayout] -> FutharkServer -> [FFIM (ValueRef TypeLayout)]
  call n ps i = do
    let s = fsiServer $ fsInfo i
        (ServerInterface i') = fsiInterface $ fsInfo i
        (EntryPoint _ ols) = i' M.! nameToText n
    
    let k = pin ps $ \ps' -> mkRoot (head ols) $ \oid -> do
          void $ liftIO $ S.cmdCall s (nameToText n) (map toVarName ps') (map toVarName [oid])
          pure $ void $ S.cmdFree s [toVarName oid] -- TODO: This should be a service
    [k]

  indexArray :: [Int] -> FutharkServer -> ValueRef TypeLayout -> FFIM (ValueRef TypeLayout)
  indexArray is i v = do
    let s = fsiServer $ fsInfo i
    -- TODO: Fix metadata
    pin [v] $ \[pvid] -> mkChild v (ArrayIndex is) (metadata v) $ \cvid -> do
          void $ liftIO $ S.cmdIndex s (toVarName pvid) (toVarName cvid) is
          pure $ void $ S.cmdFree s [toVarName cvid] -- TODO: This should be a service

  projectRecord :: Name -> FutharkServer -> ValueRef TypeLayout -> FFIM (ValueRef TypeLayout)
  projectRecord f i v = do
    let s = fsiServer $ fsInfo i
    -- TODO: Fix metadata
    pin [v] $ \[pvid] -> mkChild v (RecordField f) (metadata v) $ \cvid -> do
          void $ liftIO $ S.cmdProject s (toVarName pvid) (toVarName cvid) $ nameToText f
          pure $ void $ S.cmdFree s [toVarName cvid] -- TODO: This should be a service

  destroyVariant :: Int -> FutharkServer -> ValueRef TypeLayout -> FFIM (ValueRef TypeLayout)
  destroyVariant = error "TODO destroyVariant"

  pushPrimitives :: [PrimValue] -> FutharkServer -> [FFIM (ValueRef TypeLayout)]
  pushPrimitives = error "TODO pushPrimitives"

  pullPrimitives :: [ValueRef TypeLayout] -> FutharkServer -> [FFIM PrimValue]
  pullPrimitives = error "TODO pullPrimitives"


data FutharkServerInfo = FutharkServerInfo
  { fsiServer :: S.Server,
    fsiInterface :: ServerInterface
  }

init :: S.Server -> IO FutharkServer
init s = do
  info <- FutharkServerInfo s <$> exploreProgram s
  pure $ FutharkServer info

futharkServerCfg :: FilePath -> [String] -> S.ServerCfg
futharkServerCfg prog opts =
  (S.newServerCfg prog opts)
    { S.cfgDebug = isEnvVarAtLeast "FUTHARK_COMPILER_DEBUGGING" 1
    }

startServer :: FilePath -> IO FutharkServer
startServer prog = S.startServer (futharkServerCfg prog []) >>= init

newtype FutharkServerM a = FutharkServerM (ReaderT FutharkServerInfo FFIM a)
  deriving (Functor, Applicative, Monad, MonadIO, MonadReader FutharkServerInfo)

runFutharkServerM :: FutharkServerM a -> FutharkServer -> FFIM a
runFutharkServerM (FutharkServerM m) s = runReaderT m $ fsInfo s

server :: FutharkServerM S.Server
server = asks fsiServer

interface :: FutharkServerM ServerInterface
interface = asks fsiInterface
