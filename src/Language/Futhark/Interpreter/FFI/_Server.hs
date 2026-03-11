module Language.Futhark.Interpreter.FFI.Server
  ( Server,
    startServer,
    getInterface,
    call,
  )
where

import Control.Arrow (Arrow (second, first))
import Control.Monad (forM_, void, forM)
import Data.Binary qualified as B
import Data.ByteString.Lazy qualified as BL
import Data.Foldable (foldlM)
import Data.Map qualified as M
import Data.Maybe (fromJust)
import Data.Text qualified as T
import Futhark.Server qualified as S
import Futhark.Test.Values qualified as V
import GHC.IO.Handle (hClose)
import Language.Futhark.Core (Name, nameToText, nameFromText)
import Language.Futhark.Interpreter.FFI
import Language.Futhark.Interpreter.FFI.Values
import Prelude hiding (init)
import System.IO.Temp (withSystemTempFile)
import Control.Monad.State (State, modify, get, runState)

data SrvType = SrvType
  { srvTypeName :: S.TypeName,
    srvTypeLayout :: TypeLayout
  }
  deriving (Show, Eq, Ord)

data TypeLayout
  = TLPrimitive PrimitiveType
  | TLArray Int SrvType
  | TLRecord [(S.FieldName, SrvType)]
  | TLSum [(S.VariantName, [SrvType])]
  | TLOpaque
  deriving (Show, Eq, Ord)

toType :: SrvType -> ExType
toType (SrvType n TLOpaque) = TAtom $ Left n
toType (SrvType _ (TLArray r t)) = TArray r $ toType t
toType (SrvType _ (TLPrimitive p)) = TAtom $ Right p
--toType (SrvType _ (TLRecord m)) = TRecord $ M.fromList $ map (second toType) m
--toType (SrvType _ (TLSum m)) = TSum $ M.fromList $ map (second $ map toType) m
toType _ = error "TODO (ru89q2jodwlakm)"

data ConstructionPlan
  = Reference ExID
  | MkRecord ExID T.Text [ExID]
  | MkVariant ExID T.Text T.Text [ExID]
  deriving (Show, Eq)

type Planner a = State (ExID, [(PrimitiveValue, ExID)]) a
type Plan = Planner [ConstructionPlan]

getID :: Planner ExID
getID = fst <$> get <* modify (first nextID)

toName :: ExID -> T.Text
toName i = "v" <> T.show (exID i)

loadPrimitive :: PrimitiveValue -> Planner ExID
loadPrimitive v = do
  vid <- getID
  modify $ second ((v, vid):)
  pure vid

mkRecord :: T.Text -> [ExID] -> Plan
mkRecord t vs = do
  vid <- getID
  pure [MkRecord vid t vs]

mkVariant :: T.Text -> T.Text -> [ExID] -> Plan
mkVariant t v vs = do
  vid <- getID
  pure [MkVariant vid t v vs]

outID :: [ConstructionPlan] -> ExID
outID = outID' . last
  where
    outID' (Reference i) = i
    outID' (MkRecord i _ _) = i
    outID' (MkVariant i _ _ _) = i

pack :: SrvType -> ExValue -> Plan
--pack (SrvType _ TLOpaque) (Atom (Left i)) = pure [Reference i]
--pack (SrvType _ (TLPrimitive _)) (Atom (Right v)) = do
--  vid <- loadPrimitive v
--  pure [Reference vid]
--pack (SrvType _ (TLRecord l)) (Record m) = (forM l (\(n, t) -> outID <$> pack t (fromJust $ M.lookup n m))) >>= mkRecord "rec"
pack a b = error $ "Type layout does not match value " ++ show a ++ " " ++ show b

unpack :: S.Server -> ConstructionPlan -> IO ExID
unpack _ (Reference i) = pure i
unpack s (MkRecord oid t vs) = do
  print "new"
  out <- S.cmdNew s (toName oid) t (map toName vs)
  print out
  pure oid
unpack s (MkVariant oid t v vs) = do
  print "con"
  void $ S.cmdConstruct s (toName oid) t v (map toName vs)
  pure oid

runPlan :: S.Server -> Plan -> IO ExID
runPlan s p = do
  let (plan, (_, load)) = runState p (initID, [])
  withSystemTempFile "futhark-call-inputs" $ \tmpf tmpf_h -> do
    forM_ (map (encodePrimitive . fst) load) $ BL.hPutStr tmpf_h
    hClose tmpf_h

    -- Load values
    let vnames = map (toName . snd) load
        tnames = map (T.pack . primitiveTypeName . primitiveType . fst) load
    void $ S.cmdRestore s tmpf $ zip vnames tnames

    -- Build
    last <$> forM plan (unpack s)

handleServerError :: Either S.CmdFailure a -> a
handleServerError = either (\e -> error $ "TODO (89utqojfials) " ++ show e) id

data EntryPoint
  = EntryPoint [S.TypeName] [S.TypeName]
  deriving (Show, Eq)

data ServerInterface
  = ServerInterface (M.Map S.TypeName SrvType) (M.Map S.EntryName EntryPoint)

getServerInterface :: S.Server -> IO ServerInterface
getServerInterface s = do
  t <- parseTypes
  e <- parseEntryPoints
  pure $ ServerInterface t e
  where
    parseTypes :: IO (M.Map S.TypeName TypeLayout)
    parseTypes = handleServerError <$> S.cmdTypes s >>= foldlM parseTypes' M.empty

    parseTypes' :: M.Map S.TypeName TypeLayout -> S.TypeName -> IO (M.Map S.TypeName TypeLayout)
    parseTypes' m n = (\t -> M.insert n t m) <$> parseType m n

    parseType :: M.Map S.TypeName TypeLayout -> S.TypeName -> IO TypeLayout
    parseType _ "i8"   = pure $ TLPrimitive TInt8
    parseType _ "i16"  = pure $ TLPrimitive TInt16
    parseType _ "i32"  = pure $ TLPrimitive TInt32
    parseType _ "i64"  = pure $ TLPrimitive TInt64
    parseType _ "u8"   = pure $ TLPrimitive TUInt8
    parseType _ "u16"  = pure $ TLPrimitive TUInt16
    parseType _ "u32"  = pure $ TLPrimitive TUInt32
    parseType _ "u64"  = pure $ TLPrimitive TUInt64
    parseType _ "f16"  = pure $ TLPrimitive TFloat16
    parseType _ "f32"  = pure $ TLPrimitive TFloat32
    parseType _ "f64"  = pure $ TLPrimitive TFloat64
    parseType _ "bool" = pure $ TLPrimitive TBool
    parseType m n = do
      k <- handleServerError <$> S.cmdKind s n
      case k of
        S.Primitive -> error "Impossible (9y38qwfiuhoajl)" -- TODO
        S.Array -> error "TODO (qowufaji2783qiw)"
        S.Record -> do
          f <- handleServerError <$> S.cmdFields s n
          TLRecord <$> mapM (parseField m) f
        S.Sum    -> do
          f <- handleServerError <$> S.cmdVariants s n
          TLSum <$> mapM (parseVariant m) f
        S.Opaque -> pure $ TLOpaque $ T.unpack n

    parseField :: M.Map S.TypeName TypeLayout -> S.Field -> IO (Name, TypeLayout)
    parseField m f = (nameFromText $ S.fieldName f,) <$> parseType m (S.fieldType f)

    parseVariant :: M.Map S.TypeName TypeLayout -> S.Variant -> IO (Name, [TypeLayout])
    parseVariant m v = (nameFromText $ S.variantName v,) <$> mapM (parseType m) (S.variantTypes v)

    parseEntryPoints :: IO (M.Map S.EntryName EntryPoint)
    parseEntryPoints = do
      e <- handleServerError <$> S.cmdEntryPoints s
      M.fromList . zip e <$> mapM parseEntryPoint e

    parseEntryPoint :: S.EntryName -> IO EntryPoint
    parseEntryPoint n = do
      i <- map S.inputType  . handleServerError <$> S.cmdInputs  s n
      o <- map S.outputType . handleServerError <$> S.cmdOutputs s n
      pure $ EntryPoint i o

toInterface :: ServerInterface -> ExInterface
toInterface (ServerInterface t e) = Interface $ M.mapKeys nameFromText $ M.map (toFunction t) e

toFunction :: M.Map S.TypeName TypeLayout -> EntryPoint -> ExFunction
toFunction m (EntryPoint i o) = Function (map getType i) $ toTupleType $ map getType o
  where
    getType :: S.TypeName -> ExType
    getType n = toType $ fromJust $ M.lookup n m

data Server = Server S.Server ServerInterface

init :: S.Server -> IO Server
init s = Server s <$> getServerInterface s

getInterface :: Server -> ExInterface
getInterface (Server _ i) = toInterface i

startServer :: FilePath -> IO Server
startServer p = S.startServer (S.newServerCfg p []) >>= init

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

toEx :: InValue -> ExValue
toEx (Atom v) = Atom $ Right v
toEx (Array s vs) = Array s $ map toEx vs
toEx (Record m) = Record $ M.map toEx m
toEx (Sum v vs) = Sum v $ map toEx vs

call :: Server -> Name -> [InValue] -> IO InValue
call (Server s (ServerInterface t e)) n v =
  let (EntryPoint i o) = fromJust $ M.lookup (nameToText n) e
      i' = map getTypeLayout i
      o' = map getTypeLayout o
   in do
    eid <- runPlan s $ pack (head i') (toEx $ head v)
    print eid
    out <- S.cmdKind s $ toName eid
    print out
    error "TODO (129ey8uihjqknd)"
  where
    getTypeLayout :: S.TypeName -> TypeLayout
    getTypeLayout n' = fromJust $ M.lookup n' t

--  let n' = nameToText n
--   in fmap tuple' . call' n' (fromJust $ M.lookup n' e)
--  where
--    call' :: S.EntryName -> EntryPoint -> [ExValue] -> IO [ExValue]
--    call' n' (EntryPoint i o) p = do
--      let i' = take (length i) $ map (T.pack . ("i"++) . show) ([0..] :: [Int])
--      let o' = take (length o) $ map (T.pack . ("o"++) . show) ([0..] :: [Int])
--
--      withSystemTempFile "futhark-call-inputs" $ \tmpf tmpf_h -> do
--        forM_ (zipWith (encode . getTypeLayout) i p) $ BL.hPutStr tmpf_h
--        hClose tmpf_h
--        handleServerError <$> S.restore s tmpf (zip i' i)
--      
--      _ <- handleServerError <$> S.call s n' o' i'
--
--      withSystemTempFile "futhark-call-outputs" $ \tmpf tmpf_h -> do
--        hClose tmpf_h
--        void $ S.store s tmpf o'
--        bs <- BL.readFile tmpf
--        case decodeAllOrFail (map getTypeLayout o) bs of
--          Left v -> error $ "TODO (u89riqojkms) " ++ show v
--          Right (_, _, v) -> pure v
--
--    getTypeLayout :: S.TypeName -> TypeLayout
--    getTypeLayout n' = fromJust $ M.lookup n' t
--
--    tuple' :: [ExValue] -> ExValue
--    tuple' [v] = v
--    tuple' vs = tuple vs
