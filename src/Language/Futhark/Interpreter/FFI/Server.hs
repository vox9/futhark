module Language.Futhark.Interpreter.FFI.Server
  ( FutharkServer (..),
    --toVar,
    startServer,
    --call
    FutharkServerM,
    server,
    interface,
    getValueID,
    runFutharkServerM
  )
where

import Futhark.Server qualified as S
import Language.Futhark.Interpreter.FFI.Server.Explorer (exploreProgram)
import Language.Futhark.Interpreter.FFI.Server.Interface (ServerInterface (..))
import Prelude hiding (init)
import Language.Futhark.Interpreter.FFI.ExID (ExIDSrcT, ExValueID)
import Language.Futhark.Interpreter.FFI.ExID qualified as EID
import Control.Monad.Reader (ReaderT (runReaderT), asks, MonadIO, MonadReader, MonadTrans (lift))

-- Server and function calling
data FutharkServer = FutharkServer
  { fsServer :: S.Server,
    fsInterface :: ServerInterface
  }

init :: S.Server -> IO FutharkServer
init s = FutharkServer s <$> exploreProgram s

startServer :: FilePath -> IO FutharkServer
startServer p = S.startServer (S.newServerCfg p []) >>= init

newtype FutharkServerM a = FutharkServerM (ReaderT FutharkServer (ExIDSrcT IO) a)
  deriving (Functor, Applicative, Monad, MonadIO, MonadReader FutharkServer)

runFutharkServerM :: FutharkServerM a -> FutharkServer -> IO a
runFutharkServerM (FutharkServerM m) s =
  fst <$> EID.runExIDSrcT (runReaderT m s) EID.initIDSrc

server :: FutharkServerM S.Server
server = asks fsServer

interface :: FutharkServerM ServerInterface
interface = asks fsInterface

getValueID :: FutharkServerM ExValueID
getValueID = FutharkServerM $ lift $ EID.getValueID
