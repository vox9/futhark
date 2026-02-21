module Language.Futhark.Interpreter.FFI.Server
  ( Server,
    startServer,
    getInterface,
    call
  )
where

import Control.Arrow (Arrow(second))
import Control.Monad (forM_, void)
import Data.Binary qualified as B
import Data.Binary.Get qualified as B
import Data.ByteString.Lazy qualified as BL
import Data.Foldable (foldlM)
import Data.Map qualified as M
import Data.Maybe (fromJust)
import Data.Text qualified as T
import Futhark.Server qualified as S
import Futhark.Server.Parsed qualified as S
import Futhark.Test.Values qualified as V
import GHC.IO.Handle (hClose)
import Language.Futhark.Core (Name, nameToText, nameFromText, nameFromString)
import Language.Futhark.Interpreter.FFI
import Language.Futhark.Interpreter.FFI.Values
import Prelude hiding (init)
import System.IO.Temp (withSystemTempFile)
import Data.List (findIndex)

data TypeLayout
  = TLPrimitive PrimitiveType
  | TLRecord [(Name, TypeLayout)]
  | TLSum [(Name, [TypeLayout])]
  deriving (Show, Eq)

toType :: TypeLayout -> Type
toType (TLPrimitive p) = TPrimitive p
toType (TLRecord m) = TRecord $ M.fromList $ map (second toType) m
toType (TLSum m) = TSum $ M.fromList $ map (second $ map toType) m

tupleTypeLayout :: [TypeLayout] -> TypeLayout
tupleTypeLayout ts = TLRecord $ zip (map (nameFromString . show) ([0..] :: [Int])) ts

handleServerError :: Either S.CmdFailure a -> a
handleServerError = either (\e -> error $ "TODO (89utqojfials) " ++ show e) id

data EntryPoint
  = EntryPoint [S.TypeName] [S.TypeName]
  deriving (Show, Eq)

data ServerInterface
  = ServerInterface (M.Map S.TypeName TypeLayout) (M.Map S.EntryName EntryPoint)

getServerInterface :: S.Server -> IO ServerInterface
getServerInterface s = do
  t <- parseTypes
  e <- parseEntryPoints
  pure $ ServerInterface t e
  where
    parseTypes :: IO (M.Map S.TypeName TypeLayout)
    parseTypes = handleServerError <$> S.types s >>= foldlM parseTypes' M.empty

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
      k <- handleServerError <$> S.kind s n
      case k of
        S.Scalar -> error "Impossible (9y38qwfiuhoajl)" -- TODO
        S.Record -> do
          f <- handleServerError <$> S.fields s n
          TLRecord <$> mapM (parseField m) f
        S.Sum    -> do
          f <- handleServerError <$> S.variants s n
          TLSum <$> mapM (parseVariant m) f

    parseField :: M.Map S.TypeName TypeLayout -> S.Field -> IO (Name, TypeLayout)
    parseField m f = (nameFromText $ S.fieldName f,) <$> parseType m (S.fieldType f)

    parseVariant :: M.Map S.TypeName TypeLayout -> S.Variant -> IO (Name, [TypeLayout])
    parseVariant m v = (nameFromText $ S.variantName v,) <$> mapM (parseType m) (S.variantTypes v)

    parseEntryPoints :: IO (M.Map S.EntryName EntryPoint)
    parseEntryPoints = do
      e <- handleServerError <$> S.entryPoints s
      M.fromList . zip e <$> mapM parseEntryPoint e

    parseEntryPoint :: S.EntryName -> IO EntryPoint
    parseEntryPoint n = do
      i <- map S.inputType  . handleServerError <$> S.inputs  s n
      o <- map S.outputType . handleServerError <$> S.outputs s n
      pure $ EntryPoint i o

toInterface :: ServerInterface -> Interface
toInterface (ServerInterface t e) = Interface $ M.mapKeys nameFromText $ M.map (toFunction t) e

toFunction :: M.Map S.TypeName TypeLayout -> EntryPoint -> Function
toFunction m (EntryPoint i o) = Function (map getType i) $ tupleType $ map getType o
  where
    getType :: S.TypeName -> Type
    getType n = toType $ fromJust $ M.lookup n m

data Server = Server S.Server ServerInterface

init :: S.Server -> IO Server
init s = Server s <$> getServerInterface s

getInterface :: Server -> Interface
getInterface (Server _ i) = toInterface i

startServer :: FilePath -> IO Server
startServer p = S.startServer (S.newServerCfg p []) >>= init

encode :: TypeLayout -> Value -> BL.ByteString
encode _ (Primitive v) = encodePrimitive v
encode (TLRecord tl) (Record vm) = foldl1 (<>) $ map (encodeField vm) tl
  where
    encodeField :: M.Map Name Value -> (Name, TypeLayout) -> BL.ByteString
    encodeField m (n, t) = encode t $ fromJust $ M.lookup n m
-- The sum code almost definitely doesn't work for every case
encode (TLSum vs) (Sum n v) =
  let i = fromJust $ findIndex ((==n) . fst) vs
   in    encodePrimitive (Int8 $ fromIntegral i)
      <> foldl1 (<>) (map (encodeVariant v) vs)
  where
    encodeVariant :: [Value] -> (Name, [TypeLayout]) -> BL.ByteString
    encodeVariant v' (n', t) = foldl1 (<>) $ if n == n' then zipWith encode t v' else zipWith encode t $ map (zero . toType) t
encode _ _ = error "TODO (1y7r38iwhuqfkn)"

zero :: Type -> Value
zero (TPrimitive t) = Primitive $ zeroPrim t
zero (TRecord m) = Record $ M.map zero m
zero (TSum m) =
  let (n, t) = head $ M.toList m
   in Sum n $ map zero t

zeroPrim :: PrimitiveType -> PrimitiveValue
zeroPrim t = primInt t 0

primInt :: PrimitiveType -> Int -> PrimitiveValue
primInt TInt8    = Int8    . fromIntegral
primInt TInt16   = Int16   . fromIntegral
primInt TInt32   = Int32   . fromIntegral
primInt TInt64   = Int64   . fromIntegral
primInt TUInt8   = UInt8   . fromIntegral
primInt TUInt16  = UInt16  . fromIntegral
primInt TUInt32  = UInt32  . fromIntegral
primInt TUInt64  = UInt64  . fromIntegral
primInt TFloat16 = Float16 . fromIntegral
primInt TFloat32 = Float32 . fromIntegral
primInt TFloat64 = Float64 . fromIntegral
primInt TBool    = Bool    . (/=0)

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

decodeAllOrFail ::
  [TypeLayout] ->
  BL.ByteString ->
  Either (BL.ByteString, B.ByteOffset, String)
         (BL.ByteString, B.ByteOffset, [Value])
decodeAllOrFail t = B.runGetOrFail $ sequence $ map getValue t

getValue :: TypeLayout -> B.Get Value
getValue (TLPrimitive t) = Primitive <$> getPrimitive t
getValue (TLRecord tl) = Record . M.fromList <$> mapM (sequence . second getValue) tl
getValue (TLSum vs) = do
  (Int64 t) <- getPrimitive TInt64
  let (n, tl) = vs !! (fromIntegral t)
  Sum n <$> mapM getValue tl

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

call :: Server -> Name -> [Value] -> IO Value
call (Server s (ServerInterface t e)) n =
  let n' = nameToText n
   in fmap tuple' . call' n' (fromJust $ M.lookup n' e)
  where
    call' :: S.EntryName -> EntryPoint -> [Value] -> IO [Value]
    call' n' (EntryPoint i o) p = do
      let i' = take (length i) $ map (T.pack . ("i"++) . show) ([0..] :: [Int])
      let o' = take (length o) $ map (T.pack . ("o"++) . show) ([0..] :: [Int])

      withSystemTempFile "futhark-call-inputs" $ \tmpf tmpf_h -> do
        forM_ (zipWith (encode . getTypeLayout) i p) $ BL.hPutStr tmpf_h
        hClose tmpf_h
        handleServerError <$> S.restore s tmpf (zip i' i) >>= print
      
      _ <- handleServerError <$> S.call s n' o' i'

      withSystemTempFile "futhark-call-outputs" $ \tmpf tmpf_h -> do
        hClose tmpf_h
        void $ S.store_ s tmpf o'
        bs <- BL.readFile tmpf
        case decodeAllOrFail (map getTypeLayout o) bs of
          Left v -> error $ "TODO (u89riqojkms) " ++ show v
          Right (_, _, v) -> pure v

    getTypeLayout :: S.TypeName -> TypeLayout
    getTypeLayout n' = fromJust $ M.lookup n' t

    tuple' :: [Value] -> Value
    tuple' [v] = v
    tuple' vs = tuple vs
