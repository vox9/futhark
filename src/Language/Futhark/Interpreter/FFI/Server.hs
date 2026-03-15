module Language.Futhark.Interpreter.FFI.Server
  ( FutharkServer (..),
    startServer,
    FutharkServerM,
    server,
    interface,
    state,
    getValueID,
    runFutharkServerM
  )
where

import Futhark.Server qualified as S
import Data.Map qualified as M
import Language.Futhark.Interpreter.FFI.Server.Explorer (exploreProgram)
import Language.Futhark.Interpreter.FFI.Server.Interface (ServerInterface (..))
import Prelude hiding (init)
import Language.Futhark.Interpreter.FFI.ExID (ExIDSrcT, ExValueID, ExTypeID)
import Language.Futhark.Interpreter.FFI.ExID qualified as EID
import Control.Monad.Reader (ReaderT (runReaderT), asks, MonadIO, MonadReader, MonadTrans (lift))

-- Server and function calling
data FutharkServer = FutharkServer
  { fsServer :: S.Server,
    fsState :: M.Map ExValueID ExTypeID,
    fsIdSource :: EID.ExIDSrc,
    fsInterface :: ServerInterface
  }

init :: S.Server -> IO FutharkServer
init s = FutharkServer s mempty EID.initIDSrc <$> exploreProgram s

startServer :: FilePath -> IO FutharkServer
startServer p = S.startServer (S.newServerCfg p []) >>= init

newtype FutharkServerM a = FutharkServerM (ReaderT FutharkServer (ExIDSrcT IO) a)
  deriving (Functor, Applicative, Monad, MonadIO, MonadReader FutharkServer)

runFutharkServerM :: FutharkServerM a -> FutharkServer -> IO (a, FutharkServer)
runFutharkServerM (FutharkServerM m) s = do
  (o, src) <- EID.runExIDSrcT (runReaderT m s) $ fsIdSource s
  pure (o, s { fsIdSource = src })

server :: FutharkServerM S.Server
server = asks fsServer

state :: FutharkServerM (M.Map ExValueID ExTypeID)
state = asks fsState

interface :: FutharkServerM ServerInterface
interface = asks fsInterface

getValueID :: FutharkServerM ExValueID
getValueID = FutharkServerM $ lift $ EID.getValueID
