module Futhark.AutoImp
  ( ImplementM,
    implement,
    declare
  )
where

import Control.Monad.Error.Class (MonadError (throwError))
import Control.Monad.State (StateT, get, put, MonadState, evalStateT)
import Data.Text qualified as T
import Futhark.FreshNames (VNameSource)
import Futhark.Util.Pretty (docText)
import Language.Futhark.Parser (parseDecOrExp, syntaxErrorMsg)
import Language.Futhark.Semantic (FileModule (..), Imports)
import Language.Futhark.Syntax (ImportName (ImportName), ProgBase (progDecs))
import Language.Futhark.TypeChecker (checkDec, prettyTypeError)
import Control.Monad.Reader (ReaderT (runReaderT), MonadReader (ask))

-- TODO: Better error type
newtype ImplementM a = ImplementM (StateT (VNameSource, FileModule) (ReaderT (Imports, FilePath) (Either T.Text)) a)
  deriving (Functor, Applicative, Monad, MonadState (VNameSource, FileModule), MonadReader (Imports, FilePath), MonadError T.Text)

implement :: ImplementM a -> Imports -> VNameSource -> Either T.Text (Imports, VNameSource)
implement m i ns = implement' (m *> getInfo)
  where
    implement' :: ImplementM a -> Either T.Text a
    implement' (ImplementM m') =
      let (ImportName p, fm) = last i
      in runReaderT (evalStateT m' (ns, fm)) (init i, p)

    getInfo :: ImplementM (Imports, VNameSource)
    getInfo = do
      i' <- getImports
      ns' <- fst <$> get
      pure (i', ns')

getImports :: ImplementM Imports
getImports = do
  (ii, p) <- ask
  fm <- snd <$> get
  pure $ ii ++ [(ImportName p, fm)]

fromEither :: MonadError e m => (a -> e) -> Either a b -> m b
fromEither f = either (throwError . f) pure

withInfo :: (Imports -> FilePath -> (VNameSource, FileModule) -> ImplementM (VNameSource, FileModule)) -> ImplementM ()
withInfo f = do
  i <- getImports
  p <- (++".auto") . snd <$> ask
  get >>= f i p >>= put

declare :: T.Text -> ImplementM ()
declare t = withInfo declare'
  where
    declare' :: Imports -> FilePath -> (VNameSource, FileModule) -> ImplementM (VNameSource, FileModule)
    declare' i p (ns, fm) = do
      parsed <- fromEither syntaxErrorMsg $ parseDecOrExp p t
      dec <- either pure (const $ throwError "TODO: Not a declaration (91e8uqowidj)") parsed
      (env, dec', ns') <- fromEither (docText . prettyTypeError) . snd $ checkDec i ns (fileScope fm) (ImportName "TODO (9r3u8wdoij)") dec
      let fp = fileProg fm
          fm' = fm {
            fileEnv = env <> fileEnv fm,
            fileProg = fp {
              progDecs = dec' : progDecs fp
            },
            fileScope = env <> fileScope fm
          }
      pure (ns', fm')
