module Language.Futhark.Interpreter.FFI.FutharkServer.Explorer
  ( exploreProgram,
    EntryMap,
  )
where

import Control.Arrow (Arrow (first, second))
import Control.Monad (forM, forM_)
import Control.Monad.State (MonadIO (liftIO), MonadState, StateT, gets, modify, execStateT)
import Data.Map qualified as M
import Data.Text qualified as T
import Futhark.Data (PrimType (..))
import Futhark.Server qualified as S
import Language.Futhark.Interpreter.FFI.FutharkServer.TypeLayout (TypeLayout (..))

-- The explorer monad
type EntryMap = M.Map S.EntryName ([TypeLayout], TypeLayout)
type SEState = (EntryMap, M.Map S.TypeName TypeLayout)
newtype ServerExplorer a = ServerExplorer (StateT SEState IO a)
  deriving (Functor, Applicative, Monad, MonadIO, MonadState SEState)

runServerExplorer :: ServerExplorer a -> IO EntryMap
runServerExplorer (ServerExplorer m) = fst <$> execStateT m mempty

-- Utility functions
lookupTypeName :: S.TypeName -> ServerExplorer (Maybe TypeLayout)
lookupTypeName n = ServerExplorer $ gets $ M.lookup n . snd

putEntryPoint :: S.EntryName -> [TypeLayout] -> TypeLayout -> ServerExplorer ()
putEntryPoint n i o = ServerExplorer $ modify $ first $ M.insert n (i, o)

putType :: S.TypeName -> TypeLayout -> ServerExplorer ()
putType n = ServerExplorer . modify . second . M.insert n

printError :: Maybe S.CmdFailure -> T.Text -> a
printError (Just f) e = error $ T.unpack $ T.unlines $ "During FLP server exploration:" : e : "Failure message:" : S.failureMsg f
printError Nothing e = error $ T.unpack $ "During FLP server exploration:\n" <> e

-- Exploration logic
exploreType :: S.Server -> S.TypeName -> ServerExplorer TypeLayout
exploreType s n = do
  tid <- lookupTypeName n
  case tid of
    Just tid' -> pure tid'
    Nothing -> do
      k <- liftIO $ S.cmdKind s n
      tl <- case k of
            Right S.Primitive -> handlePrimitive
            Right S.Array -> handleArray
            Right S.Record -> handleRecord
            Right S.Sum -> handleSum
            Right S.Opaque -> handleOpaque
            Left f -> printError (Just f) $ "cmdKind failed on type " <> n
      putType n tl
      pure tl
  where
    handlePrimitive =
      pure $ TLPrimitive $
        case n of
          "i8" -> I8
          "i16" -> I16
          "i32" -> I32
          "i64" -> I64
          "u8" -> U8
          "u16" -> U16
          "u32" -> U32
          "u64" -> U64
          "f16" -> F16
          "f32" -> F32
          "f64" -> F64
          "bool" -> Bool
          _ -> printError Nothing $ "Encountered invalid primitive type " <> n
    handleArray = do
      e <- liftIO $ S.cmdElemtype s n
      r <- liftIO $ S.cmdRank s n
      case (e, r) of
        (Right e', Right r') -> TLArray n (fromIntegral r') <$> exploreType s e'
        (Left f, _) -> printError (Just f) $ "cmdElemtype failed on type " <> n
        (_, Left f) -> printError (Just f) $ "cmdRank failed on type " <> n
    handleRecord = do
      fs <- liftIO $ S.cmdFields s n
      case fs of
        Right fs' ->
          TLRecord n <$> forM fs' (\f -> (S.fieldName f,) <$> exploreType s (S.fieldType f))
        Left f -> printError (Just f) $ "cmdFields failed on type " <> n
    handleSum = do
      vs <- liftIO $ S.cmdVariants s n
      case vs of
        Right vs' ->
          TLSum n . M.unions <$> forM vs' (\v -> M.singleton (S.variantName v) <$> mapM (exploreType s) (S.variantTypes v))
        Left f -> printError (Just f) $ "cmdVariants failed on type " <> n
    handleOpaque = pure $ TLOpaque n

exploreEntryPoint :: S.Server -> S.EntryName -> ServerExplorer ()
exploreEntryPoint s n = do
  is <- liftIO $ S.cmdInputs s n
  o <- liftIO $ S.cmdOutput s n
  case (is, o) of
    (Right is', Right os') -> do
      is'' <- forM is' $ exploreType s . S.inputType
      o'' <- exploreType s $ S.outputType os'
      putEntryPoint n is'' o''
    (Left f, _) -> printError (Just f) $ "inputType failed on function " <> n
    (_, Left f) -> printError (Just f) $ "outputType failed on function " <> n

exploreProgram :: S.Server -> IO EntryMap
exploreProgram s = runServerExplorer exploreProgram'
  where
    exploreProgram' = do
      es <- liftIO $ S.cmdEntryPoints s
      case es of
        Right es' -> forM_ es' $ exploreEntryPoint s
        Left f -> printError (Just f) $ "cmdEntryPoints failed"
