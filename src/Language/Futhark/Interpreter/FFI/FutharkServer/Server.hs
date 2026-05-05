module Language.Futhark.Interpreter.FFI.FutharkServer.Server
  ( FutharkServer (..),
    start',
    start,
  )
where

import Control.Arrow (Arrow (first))
import Control.Concurrent (yield)
import Control.Monad (zipWithM, forM, replicateM, when)
import Control.Monad.IO.Class (MonadIO (liftIO))
import Data.Array qualified as A
import Data.Binary qualified as B
import Data.Binary.Get qualified as B
import Data.Binary.Put qualified as B
import Data.ByteString.Lazy qualified as BL
import Data.Foldable (toList, forM_)
import Data.List (mapAccumL)
import Data.Map qualified as M
import Data.Maybe (fromJust)
import Data.Text qualified as T
import Futhark.Server qualified as S
import Futhark.Util (isEnvVarAtLeast)
import GHC.IO.Handle (hClose)
import Language.Futhark.Core (nameFromText, nameToText, Name)
import Language.Futhark.Interpreter (Value)
import Language.Futhark.Interpreter.FFI qualified as FFI
import Language.Futhark.Interpreter.FFI.FutharkServer.Explorer (exploreProgram, EntryMap)
import Language.Futhark.Interpreter.FFI.FutharkServer.TypeLayout (TypeLayout (..), typeName)
import Language.Futhark.Interpreter.FFI.FutharkServer.ValueHandle (ValueHandle (..), ValueReference (..), withValueHandle, mkValueHandle, mkValueHandles, withValueHandles, Direction (ArrayElement, RecordField, VariantField))
import Language.Futhark.Interpreter.FFI.UIDs (ValueUID, uid, MonadUIDSource, getUID, runUIDSourceT, UIDSource)
import Language.Futhark.Interpreter.FFI.Util.AtomicList (AtomicList, flush, new)
import Language.Futhark.Interpreter.FFI.Value qualified as FFI
import Language.Futhark.Interpreter.Values (Shape (..), ValueShape)
import System.IO.Temp (withSystemTempFile)
import System.Mem (performGC)

data FutharkServerI = FutharkServerI
  { server :: S.Server,
    entryMap :: EntryMap,
    gcList :: AtomicList ValueUID
  }

data FutharkServer = FutharkServer
  { efi :: FFI.EFI Value ValueReference
    --evi :: FFI.EVI S.TypeName ValueHandle
  }

start' :: FilePath -> IO FutharkServer
start' prog = fst <$> runUIDSourceT (start prog) (mempty :: UIDSource)

start :: (MonadIO m, MonadUIDSource m) => FilePath -> m FutharkServer
start prog = liftIO (S.startServer $ futharkServerCfg prog []) >>= fromServer

toVarName :: ValueUID -> S.VarName
toVarName i = "v" <> T.show (uid i)

fromServer :: (MonadIO m, MonadUIDSource m) => S.Server -> m FutharkServer
fromServer s = do
  m <- liftIO $ exploreProgram s
  iid <- getUID
  si <- liftIO $ FutharkServerI s m <$> new
  let efi = FFI.EFI iid (map nameFromText $ M.keys m) (pushRef si) (receiveRef si) (callRef si)
      --evi = undefined
  pure $ FutharkServer efi --evi

index :: (MonadIO m, MonadUIDSource m) => FutharkServerI -> [Int] -> ValueHandle -> m ValueHandle
index s is h | (TLArray _ _ e) <- typeLayout h =
  withValueHandle h $ \sid -> mkValueHandle (gcList s) $ \did -> do
    _ <- S.cmdIndex (server s) (toVarName did) (toVarName sid) is
    pure e
index _ _ _ = error "TODO (798quowdj): Not an array"

project :: (MonadIO m, MonadUIDSource m) => FutharkServerI -> S.FieldName -> ValueHandle -> m ValueHandle
project s f h | (TLRecord _ fs) <- typeLayout h =
  withValueHandle h $ \sid -> mkValueHandle (gcList s) $ \did -> do
    _ <- liftIO $ S.cmdProject (server s) (toVarName did) (toVarName sid) f
    pure $ fromJust $ lookup f fs
project _ _ _ = error "TODO (798quowdj): Not a record"

variant :: MonadIO m => FutharkServerI -> ValueHandle -> m S.VariantName
variant s h | (TLSum _ _) <- typeLayout h =
  withValueHandle h $ \vid ->
    either (error "TODO (yqrihwf)") id <$> liftIO (S.cmdVariant (server s) (toVarName vid))
variant _ _ = error "TODO (798quowdj): Not a sum"

destruct :: (MonadIO m, MonadUIDSource m) => FutharkServerI -> ValueHandle -> m [ValueHandle]
destruct s h | (TLSum _ m) <- typeLayout h = do
  v <- variant s h
  let ts = fromJust $ M.lookup v m
  withValueHandle h $ \sid -> mkValueHandles (gcList s) (fromIntegral $ length ts) $ \dids -> do
    -- TODO: Handle errors
    _ <- liftIO $ S.cmdDestruct (server s) (toVarName sid) (map toVarName dids)
    pure ts
destruct _ _ = error "TODO (798quowdj): Not a sum"

newArray :: (MonadIO m, MonadUIDSource m) => FutharkServerI -> TypeLayout -> [Int] -> [ValueHandle] -> m ValueHandle
newArray s t@(TLArray tn _ _) ds hs =
  withValueHandles hs $ \sids -> mkValueHandle (gcList s) $ \did -> do
    -- TODO: Handle errors
    _ <- liftIO $ S.cmdNewArray (server s) (toVarName did) tn ds $ map toVarName sids
    pure t
newArray _ _ _ _ = error "TODO (3riowda): Not an array"

newRecord :: (MonadIO m, MonadUIDSource m) => FutharkServerI -> TypeLayout -> M.Map Name ValueHandle -> m ValueHandle
newRecord s t@(TLRecord tn fs) m =
  let fs' = map (\(n, _) -> (nameFromText n, m M.! nameFromText n)) fs
  in withValueHandles (map snd fs') $ \sids -> mkValueHandle (gcList s) $ \did -> do
    -- TODO: Handle errors
    _ <- liftIO $ S.cmdNew (server s) (toVarName did) tn $ map toVarName sids
    pure t
newRecord _ _ _ = error "TODO (u8rqowfj): Not a record"

construct :: (MonadIO m, MonadUIDSource m) => FutharkServerI -> TypeLayout -> Name -> [ValueHandle] -> m ValueHandle
construct s t@(TLSum tn _) vn fs =
  withValueHandles fs $ \sids -> mkValueHandle (gcList s) $ \did -> do
    -- TODO: Handle errors
    _ <- liftIO $ S.cmdConstruct (server s) (toVarName did) tn (nameToText vn) $ map toVarName sids
    pure t
construct _ _ _ _ = error "TODO (u8rqowfj): Not a sum"

callRef :: (MonadIO m, MonadUIDSource m) => FutharkServerI -> Name -> [Either Value ValueReference] -> m ValueReference
callRef i n ps = do
  ps' <- forM ps $ either (pure . Left) $ fmap Right . push i
  o <- call i n ps'
  pure $ ValueReference o []

call :: (MonadIO m, MonadUIDSource m) => FutharkServerI -> Name -> [Either Value ValueHandle] -> m ValueHandle
call s n ps = call'
  where
    (ti, to) = entryMap s M.! nameToText n

    call' = do
      ps' <- zipWithM (\t -> either (send s t) pure) ti ps
      withValueHandles ps' $ \sids -> mkValueHandle (gcList s) $ \did -> do
        _ <- S.cmdCall (server s) (nameToText n) (toVarName did) $ map toVarName sids
        pure to

forceGC :: IO ()
forceGC = performGC >> yield

garbageCollect :: FutharkServerI -> IO ()
garbageCollect s = do
  forceGC -- TODO: This shouldn't be here - It's only here for demonstration purposes
  vids <- map toVarName <$> flush (gcList s)
  putStrLn $ "Garbage collecting " ++ show vids
  -- TODO: Handle errors
  _ <- S.cmdFree (server s) vids
  pure ()

typeValue :: TypeLayout -> FFI.Value FFI.Primitive -> FFI.Value TypeLayout
typeValue t@(TLPrimitive {}) (FFI.Leaf {}) = FFI.Leaf t
typeValue t@(TLArray {}) (FFI.Leaf {}) = FFI.Leaf t
typeValue (TLArray _ r e) (FFI.Array p) | r == 1 = FFI.Array $ fmap (typeValue e) p
typeValue (TLArray n r e) (FFI.Array p) | r > 1 = FFI.Array $ fmap (typeValue $ TLArray n (r - 1) e) p
typeValue (TLRecord _ fs) (FFI.Record m) = FFI.Record $ M.mapWithKey (\k -> typeValue $ fromJust $ lookup (nameToText k) fs) m
typeValue (TLSum _ m) (FFI.Variant vn fs) = FFI.Variant vn $ zipWith typeValue (m M.! nameToText vn) fs
typeValue t v' = error $ "TypeLayout " ++ show t ++ " doesn't match value " ++ show v'

push :: (MonadIO m, MonadUIDSource m) => FutharkServerI -> ValueReference -> m ValueHandle
push i r = do
  ValueReference h ds <- pushRef i r
  if null ds then pure h
             else error $ "TODO (89u1joiqd): Expected fully resolved value, but got directions " ++ show ds

pushRef :: (MonadIO m, MonadUIDSource m) => FutharkServerI -> ValueReference -> m ValueReference
pushRef i (ValueReference h' ds') = pushRef' (reverse ds') h'
  where
    pushRef' :: (MonadIO m, MonadUIDSource m) => [Direction] -> ValueHandle -> m ValueReference
    pushRef' [] h = pure $ ValueReference h []
    pushRef' ds@(ArrayElement is : _) h
      | (TLArray _ r _) <- typeLayout h
      , length is /= fromIntegral r
      = pure $ ValueReference h $ reverse ds
    pushRef' (ArrayElement is : ds) h = index i is h >>= pushRef' ds
    pushRef' (RecordField f : ds) h = project i (nameToText f) h >>= pushRef' ds
    pushRef' (VariantField f : ds) h = ((!!f) <$> destruct i h) >>= pushRef' ds

send :: (MonadIO m, MonadUIDSource m) => FutharkServerI -> TypeLayout -> Value -> m ValueHandle
send s t v = liftIO (garbageCollect s) >> send' >>= build t
  where
    value = FFI.flattenPrims $ FFI.pack v
    typedValue = typeValue t value

    send' = do
      let ps = toList value
          ts = toList typedValue
      hv <- traverse (\t' -> mkValueHandle (gcList s) $ const $ pure t') typedValue
      let hs = toList hv

      liftIO $ withValueHandles hs $ \sids -> withSystemTempFile "futhark-call-restore" $ \tmpf tmpf_h -> do
        BL.hPutStr tmpf_h $ B.runPut $ forM_ ps B.put
        hClose tmpf_h
        -- TODO: Handle errors
        _ <- S.cmdRestore (server s) tmpf $ zip (map toVarName sids) $ map typeName ts
        pure hv

    build :: (MonadIO m, MonadUIDSource m) => TypeLayout -> FFI.Value ValueHandle -> m ValueHandle
    build _ (FFI.Leaf p) = pure p
    build t'@(TLArray _ _ et) (FFI.Array a) = do
      let (ds, vs) = FFI.flatten a
      hs <- mapM (build et) vs
      newArray s t' ds hs
    build t'@(TLRecord _ fs) (FFI.Record m) = do
      m' <- M.fromList <$> mapM (\(n, v') -> (n,) <$> build (fromJust $ lookup (nameToText n) fs) v') (M.toList m)
      newRecord s t' m'
    build t'@(TLSum _ tm) (FFI.Variant vn vs) = do
      fs' <- zipWithM build (tm M.! nameToText vn) vs
      construct s t' vn fs'
    build _ _ = error "TODO: Impossible (y912qiuhwd)" -- TODO

receiveRef :: (MonadIO m, MonadUIDSource m) => FutharkServerI -> ValueReference -> m Value
receiveRef i r = push i r >>= fetch i

fetch :: (MonadIO m, MonadUIDSource m) => FutharkServerI -> ValueHandle -> m Value
fetch s h = do
  k <- FFI.unflattenPrims <$> (destroy h >>= fetch')
  pure $ FFI.unpack (shape (typeLayout h) k) k
  where
    -- TODO: Okay, might've made a mistake - maybe the shape is all about type, and not about actual value?
    -- I'm a little confused about ShapeDim. Perhaps the size of the array is part of the type? That would
    -- make sense! In that case, I need to extract it from the type system somehow. Yikes
    shape :: TypeLayout -> FFI.Value FFI.PrimValue -> ValueShape
    shape (TLOpaque {}) (FFI.Leaf _) = ShapeLeaf
    shape (TLPrimitive {}) (FFI.Leaf _) = ShapeLeaf
    -- TODO: All the array stuff is absolutely horrible, but I'm tired! I think I misunderstoof ShapeDim too, oops
    shape (TLArray {}) (FFI.Leaf _) = ShapeLeaf -- TODO: Not right
    shape (TLArray _ r e) (FFI.Array a) | r <= 1 = ShapeDim (fromIntegral $ snd (A.bounds a) + 1) $ shape e $ a A.! 0
    shape (TLArray _ r e) (FFI.Array a) | otherwise = ShapeDim (fromIntegral $ snd (A.bounds a) + 1) $ shape (TLArray undefined (r - 1) e) $ a A.! 0
    shape (TLRecord _ tm) (FFI.Record m) = ShapeRecord $ M.intersectionWith shape (M.fromList $ map (first nameFromText) tm) m
    -- TODO: Likely wrong as well
    shape (TLSum _ tm) (FFI.Variant vn fs) = ShapeSum $ M.singleton vn $ zipWith shape (tm M.! nameToText vn) fs
    shape t v = error $ "TODO (9r2quwdioaj) " ++ show t ++ " | " ++ show v

    destroy :: (MonadIO m, MonadUIDSource m) => ValueHandle -> m (FFI.Value ValueHandle)
    destroy h' | (TLOpaque {}) <- typeLayout h' = pure $ FFI.Leaf h'
    destroy h' | (TLPrimitive {}) <- typeLayout h' = pure $ FFI.Leaf h'
    destroy h' | (TLArray _ _ (TLPrimitive _)) <- typeLayout h' = pure $ FFI.Leaf h'
    destroy h' | (TLArray {}) <- typeLayout h' = error "Opaque arrays are TODO!!"
    destroy h' | (TLRecord _ fs) <- typeLayout h' =
      FFI.Record . M.fromList <$> forM fs (\(n, _) -> (nameFromText n,) <$> (project s n h' >>= destroy))
    destroy h' | (TLSum {}) <- typeLayout h' = do
      v <- variant s h'
      FFI.Variant (nameFromText v) <$> (destruct s h' >>= mapM destroy)
    destroy _ = error "TODO (yr2iquh)"

    isPrimitive :: TypeLayout -> Bool
    isPrimitive (TLPrimitive {}) = True
    isPrimitive (TLArray _ _ e) = isPrimitive e
    isPrimitive _ = False

    fetch' :: MonadIO m => FFI.Value ValueHandle -> m (FFI.Value FFI.Primitive)
    fetch' v = do
      let hs = toList v
      when (any (not . isPrimitive . typeLayout) hs) $ error "Trying to fetch opaque value"
      
      liftIO $ withValueHandles hs $ \sids -> withSystemTempFile "futhark-call-store" $ \tmpf tmpf_h -> do
        hClose tmpf_h
        -- TODO: Handle errors
        _ <- S.cmdStore (server s) tmpf (map toVarName sids)
        bs <- BL.readFile tmpf
        let vs' = case B.runGetOrFail (replicateM (length hs) B.get) bs of
              Left err -> error $ "TODO (u89riqojkms) " ++ show err
              Right (_, _, v') -> v'
        pure $ replace vs' v

    replace :: Traversable t => [b] -> t a -> t b
    replace vs t = snd $ mapAccumL step vs t
      where
        step (v' : vs') _ = (vs', v')
        step [] _ = error "TODO (89uoqwd)"

futharkServerCfg :: FilePath -> [String] -> S.ServerCfg
futharkServerCfg prog opts =
  (S.newServerCfg prog opts)
    { S.cfgDebug = isEnvVarAtLeast "FUTHARK_COMPILER_DEBUGGING" 1
    }
