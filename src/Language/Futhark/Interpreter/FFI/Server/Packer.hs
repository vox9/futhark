module Language.Futhark.Interpreter.FFI.Server.Packer
  ( --runPacker,
    --packing,
    --packAll,
    --load,
    call
  )
where

import Control.Arrow (Arrow (second))
import Control.Monad (forM, zipWithM, forM_, void)
import Control.Monad.Reader (ReaderT (runReaderT), MonadReader (ask), MonadIO (liftIO))
import Control.Monad.State (modify, MonadTrans (lift), gets, StateT (runStateT))
import Data.Binary qualified as B
import Data.ByteString.Lazy qualified as BL
import Data.Map qualified as M
import Data.Maybe (fromJust)
import Data.Text qualified as T
import Futhark.Server qualified as S
import Futhark.Test.Values qualified as V
import Futhark.Util.BiMap qualified as BM
import GHC.IO.Handle (hClose)
import Language.Futhark.Interpreter.FFI (ExValue, ExValueAtom)
import Language.Futhark.Interpreter.FFI.ExID
import Language.Futhark.Interpreter.FFI.Server (FutharkServerM)
import Language.Futhark.Interpreter.FFI.Server qualified as FS
import Language.Futhark.Interpreter.FFI.Server.Interface (ServerInterface (..), Entry (Entry))
import Language.Futhark.Interpreter.FFI.Server.TypeLayout (TypeLayout (..))
import Language.Futhark.Interpreter.FFI.Values
import Prelude hiding (init)
import System.IO.Temp (withSystemTempFile)
import Data.Functor.Identity (Identity (runIdentity))
import qualified Data.Binary.Get as B

newtype PackerT v m a = PackerT (ReaderT ServerInterface (StateT [v] m) a)
  deriving (Functor, Applicative, Monad, MonadIO)

instance MonadTrans (PackerT v) where
  lift = PackerT . lift . lift

type PackerM v = PackerT v Identity

runPackerT :: Monad m => PackerT v m a -> ServerInterface -> m (a, [v])
runPackerT (PackerT m) i = second reverse <$> runStateT (runReaderT m i) mempty

runPackerM :: PackerM v a -> ServerInterface -> (a, [v])
runPackerM m = runIdentity . runPackerT m

addValue :: Monad m => v -> PackerT v m Int
addValue v = PackerT $ do
  modify (v:)
  gets $ (+(-1)) . length

interface :: Monad m => PackerT v m ServerInterface
interface = PackerT $ ask

pack :: Monad m => (TypeLayout -> a -> PackerT v m (Value b)) -> ExTypeID -> Value a -> PackerT v m (Value b)
pack f tid v = do
  i <- interface
  case M.lookup tid $ siTypeLayout i of
    Just l -> pack' l v
    Nothing -> error "TODO (ru98qwojialskcm)"
  where
    pack' l (Atom a) = f l a
    pack' (TLRecord fs) (Record m) = do
      ms <- mapM (\(n, t) -> pack f t $ fromJust $ M.lookup n m) fs
      let m' = M.fromList $ zip (map fst fs) ms
      pure $ Record m'
    pack' (TLSum m) (Sum svn svs) = do
      let ts = fromJust $ M.lookup svn m
      svs' <- zipWithM (pack f) ts svs
      pure $ Sum svn svs'
    pack' _ _ = error "TODO: (9r8uqowfijlas)" -- Array or mismatch

fIn :: TypeLayout -> ExValueAtom -> PackerM PrimitiveValue (Value (Either ExValueID Int))
fIn (TLPrimitive _) (Right p) = Atom . Right <$> addValue p
fIn TLOpaque (Left i) = pure $ Atom $ Left i
fIn _ _ = error "TODO (u8roqjiwlfa)"

fEx :: TypeLayout -> ExValueID -> PackerT (ExValueID, PrimitiveType) FutharkServerM (Value (Either ExValueID Int))
fEx (TLPrimitive t) vid = Atom . Right <$> addValue (vid, t)
fEx TLOpaque vid = pure $ Atom $ Left vid
fEx (TLRecord f) vid = do
  s <- lift $ FS.server
  vids <- forM f $ \(n, _) -> do
    fvid <- lift $ FS.getValueID
    void $ liftIO $ S.cmdProject s (toVar fvid) (toVar vid) n
    pure fvid
  Record . M.fromList . zip (map fst f) <$> zipWithM (pack fEx) (map snd f) (map Atom vids)
fEx (TLSum m) vid = do
  s <- lift $ FS.server
  vn <- either (error "TODO (uojdqlamk") id <$> liftIO (S.cmdVariant s (toVar vid))
  let ts = fromJust $ M.lookup vn m
  vids <- forM ts $ const $ lift $ FS.getValueID
  Sum vn <$> zipWithM (pack fEx) ts (map Atom vids)
fEx _ vid = pure $ Atom $ Left vid

packAll :: Monad m => (TypeLayout -> a -> PackerT v m (Value b)) -> [(ExTypeID, Value a)] -> PackerT v m [Value b]
packAll f vs = forM vs $ uncurry $ pack f

load :: (S.Server, ServerInterface) -> [PrimitiveValue] -> [(ExTypeID, Value (Either ExValueID Int))] -> FutharkServerM [ExValueID]
load (s, i) ps vs = do
  vids <- forM [1..length ps] $ const FS.getValueID
  liftIO $ withSystemTempFile "futhark-call-load" $ \tmpf tmpf_h -> do
    forM_ ps $ BL.hPutStr tmpf_h . encodePrimitive
    hClose tmpf_h
    void $ S.cmdRestore s tmpf $ zip (map toVar vids) (map (T.pack . primitiveTypeName . primitiveType) ps)
  forM (map (\(tid, v) -> (tid, look tid, v)) vs) (load' vids)
  where
    encodePrimitive :: PrimitiveValue -> BL.ByteString
    encodePrimitive (Int8    v) = B.encode $ fromJust $ V.putValue v
    encodePrimitive (Int16   v) = B.encode $ fromJust $ V.putValue v
    encodePrimitive (Int32   v) = B.encode $ fromJust $ V.putValue v
    encodePrimitive (Int64   v) = B.encode $ fromJust $ V.putValue v
    encodePrimitive (UInt8   v) = B.encode $ fromJust $ V.putValue v
    encodePrimitive (UInt16  v) = B.encode $ fromJust $ V.putValue v
    encodePrimitive (UInt32  v) = B.encode $ fromJust $ V.putValue v
    encodePrimitive (UInt64  v) = B.encode $ fromJust $ V.putValue v
    encodePrimitive (Float16 v) = B.encode $ fromJust $ V.putValue v
    encodePrimitive (Float32 v) = B.encode $ fromJust $ V.putValue v
    encodePrimitive (Float64 v) = B.encode $ fromJust $ V.putValue v
    encodePrimitive (Bool    v) = B.encode $ fromJust $ V.putValue v

    load' :: [ExValueID] -> (ExTypeID, TypeLayout, Value (Either ExValueID Int)) -> FutharkServerM ExValueID
    load' _ (_, _, Atom (Left vid)) = pure vid
    load' vids (_, _, Atom (Right idx)) = pure $ vids !! idx
    load' vids (tid, TLRecord r, Record m) = do
      k <- forM r $ load' vids . \(n, t) -> (t, look t, fromJust $ M.lookup n m)
      o <- FS.getValueID
      void $ liftIO $ S.cmdNew s (toVar o) (fromJust $ BM.lookupLeft tid $ siType i) $ map toVar k
      pure o
    load' vids (tid, TLSum m, Sum vn vvs) = do
      k <- forM (zip (fromJust $ M.lookup vn m) vvs) $ load' vids . \(t, v) -> (t, look t, v)
      o <- FS.getValueID
      void $ liftIO $ S.cmdConstruct s (toVar o) (fromJust $ BM.lookupLeft tid $ siType i) vn $ map toVar k
      pure o
    load' _ _ = error "TODO (y8euiqdhjkanx)"

    look :: ExTypeID -> TypeLayout
    look tid = fromJust $ M.lookup tid $ siTypeLayout i

unload :: ServerInterface -> [(ExValueID, PrimitiveType)] -> [(ExTypeID, Value (Either ExValueID Int))] -> FutharkServerM [ExValue]
unload i vs k = do
  s <- FS.server
  liftIO $ withSystemTempFile "futhark-call-unload" $ \tmpf tmpf_h -> do
    hClose tmpf_h
    void $ S.cmdStore s tmpf $ map (toVar . fst) vs
    bs <- BL.readFile tmpf
    let vs' = case B.runGetOrFail (mapM (getPrimitive . snd) vs) bs of
          Left v -> error $ "TODO (u89riqojkms) " ++ show v
          Right (_, _, v) -> v
    pure $ map (unload' vs') (map (\(tid, v) -> (tid, look tid, v)) k)
  where
    getPrimitive :: PrimitiveType -> B.Get PrimitiveValue
    getPrimitive TInt8    = Int8    . fromJust . V.getValue <$> B.get
    getPrimitive TInt16   = Int16   . fromJust . V.getValue <$> B.get
    getPrimitive TInt32   = Int32   . fromJust . V.getValue <$> B.get
    getPrimitive TInt64   = Int64   . fromJust . V.getValue <$> B.get
    getPrimitive TUInt8   = UInt8   . fromJust . V.getValue <$> B.get
    getPrimitive TUInt16  = UInt16  . fromJust . V.getValue <$> B.get
    getPrimitive TUInt32  = UInt32  . fromJust . V.getValue <$> B.get
    getPrimitive TUInt64  = UInt64  . fromJust . V.getValue <$> B.get
    getPrimitive TFloat16 = Float16 . fromJust . V.getValue <$> B.get
    getPrimitive TFloat32 = Float32 . fromJust . V.getValue <$> B.get
    getPrimitive TFloat64 = Float64 . fromJust . V.getValue <$> B.get
    getPrimitive TBool    = Bool    . fromJust . V.getValue <$> B.get

    look :: ExTypeID -> TypeLayout
    look tid = fromJust $ M.lookup tid $ siTypeLayout i

    unload' :: [PrimitiveValue] -> (ExTypeID, TypeLayout, Value (Either ExValueID Int)) -> ExValue
    unload' pvs (_, TLPrimitive _, Atom (Right idx)) = Atom $ Right $ pvs !! idx
    unload' _ (_, TLOpaque, Atom (Left vid)) = Atom $ Left vid
    unload' pvs (_, TLRecord f, Record m) =
      Record $ M.fromList $ zip (map fst f) $ map (\(n, t) -> unload' pvs (t, look t, fromJust $ M.lookup n m)) f
    unload' pvs (_, TLSum m, Sum vn vvs) =
      Sum vn $ zipWith (\t v -> unload' pvs (t, look t, v)) (fromJust $ M.lookup vn m) vvs
    unload' _ _ = error "TODO (u8rqowijdalkcm)"

toVar :: ExValueID -> S.VarName
toVar v = "v" <> T.show (exValueID v)

call :: S.EntryName -> [ExValue] -> FutharkServerM ExValue
call n vs = do
  s <- FS.server
  si <- FS.interface

  -- Get entry info
  (Entry i o) <- getEntryPointID n >>= getEntryPoint

  -- Send inputs
  let (ivs, ps) = runPackerM (packAll fIn $ zip i vs) si
  ivs' <- load (s, si) ps $ zip i ivs

  -- Call
  o' <- forM [1..length o] $ const FS.getValueID
  let o'' = map toVar o'
  void $ liftIO $ S.cmdCall s n o'' $ map toVar ivs'

  -- Get outputs
  (ovs, oids) <- runPackerT (packAll fEx $ zip o $ map Atom o') si
  tuple' <$> unload si oids (zip o ovs)
  where
    getEntryPointID n' = do
      si <- FS.interface
      case BM.lookupRight n' $ siEntryPoint si of
        Just eid -> pure eid
        Nothing -> error $ "Entry point \"" ++ T.unpack n' ++ "\" not found"

    getEntryPoint eid = do
      si <- FS.interface
      case M.lookup eid $ siEntryPointInfo si of
        Just e -> pure e
        Nothing -> error $ "Impossible (3urq8wfijoalskm)" -- TODO

    tuple' :: [ExValue] -> ExValue
    tuple' [v] = v
    tuple' vs'' = toTuple vs''
