{-# OPTIONS_GHC -Wno-orphans #-}

module Language.Futhark.Interpreter.FFI.Value
  ( Primitive,
    typeName,
    Value (..),
    PrimValue,
    flatten,
    flattenPrims,
    unflattenPrims',
    unflattenPrims,
    pack,
    unpack,
  )
where

import Data.Array qualified as A
import Data.Map qualified as M
import Data.Vector.Storable qualified as V
import Data.Word
import Futhark.Data qualified as D
import Language.Futhark.Core (Name, nameFromString)
import Language.Futhark.Interpreter.Values qualified as I
import Language.Futhark.Syntax (PrimValue (..), IntValue (..), FloatValue (..))

type Primitive = D.Value

typeName :: Primitive -> Name
typeName = nameFromString . typeName'
  where
    brackets s = concatMap (const "[]") $ V.toList s
    
    typeName' (D.I8Value   s _) = "i8" ++ brackets s
    typeName' (D.I16Value  s _) = "i16" ++ brackets s
    typeName' (D.I32Value  s _) = "i32" ++ brackets s
    typeName' (D.I64Value  s _) = "i64" ++ brackets s
    typeName' (D.U8Value   s _) = "u8" ++ brackets s
    typeName' (D.U16Value  s _) = "u16" ++ brackets s
    typeName' (D.U32Value  s _) = "u32" ++ brackets s
    typeName' (D.U64Value  s _) = "u64" ++ brackets s
    typeName' (D.F16Value  s _) = "f16" ++ brackets s
    typeName' (D.F32Value  s _) = "f32" ++ brackets s
    typeName' (D.F64Value  s _) = "f64" ++ brackets s
    typeName' (D.BoolValue s _) = "b" ++ brackets s

data Value p
  = Leaf p
  | Array (A.Array Int (Value p))
  | Record (M.Map Name (Value p))
  | Variant Name [Value p]
  deriving (Show, Eq, Ord, Functor, Foldable, Traversable)

instance D.PutValue1 PrimValue where
  putValue1 (SignedValue (Int8Value i)) = D.putValue1 i
  putValue1 (SignedValue (Int16Value i)) = D.putValue1 i
  putValue1 (SignedValue (Int32Value i)) = D.putValue1 i
  putValue1 (SignedValue (Int64Value i)) = D.putValue1 i
  putValue1 (UnsignedValue (Int8Value i)) = D.putValue1 (fromIntegral i :: Word8)
  putValue1 (UnsignedValue (Int16Value i)) = D.putValue1 (fromIntegral i :: Word16)
  putValue1 (UnsignedValue (Int32Value i)) = D.putValue1 (fromIntegral i :: Word32)
  putValue1 (UnsignedValue (Int64Value i)) = D.putValue1 (fromIntegral i :: Word64)
  putValue1 (FloatValue (Float16Value f)) = D.putValue1 f
  putValue1 (FloatValue (Float32Value f)) = D.putValue1 f
  putValue1 (FloatValue (Float64Value f)) = D.putValue1 f
  putValue1 (BoolValue b) = D.putValue1 b

flatten :: A.Array Int (Value p) -> ([Int], [Value p])
flatten = flatten' . Array
  where
    flatten' (Array a) =
      let (r, p) = unzip $ map flatten' $ A.elems a
      in (length (A.elems a) : r !! 0, foldl (++) [] p)
    flatten' v = ([], [v])

flattenPrims' :: Value PrimValue -> Maybe Primitive
flattenPrims' (Leaf p) = Just $ D.putValue1 p
flattenPrims' (Array a) = mapM flattenPrims' (A.elems a) >>= D.putValue
flattenPrims' _ = Nothing

flattenPrims :: Value PrimValue -> Value Primitive
flattenPrims (Leaf p) = Leaf $ D.putValue1 p
flattenPrims v@(Array _) | Just v' <- flattenPrims' v = Leaf $ v'
flattenPrims (Array a) = Array $ fmap flattenPrims a
flattenPrims (Record m) = Record $ M.map flattenPrims m
flattenPrims (Variant v fs) = Variant v $ map flattenPrims fs

getValue1 :: Primitive -> PrimValue
getValue1 (D.I8Value _ vs) = SignedValue $ Int8Value $ vs V.! 0
getValue1 (D.I16Value _ vs) = SignedValue $ Int16Value $ vs V.! 0
getValue1 (D.I32Value _ vs) = SignedValue $ Int32Value $ vs V.! 0
getValue1 (D.I64Value _ vs) = SignedValue $ Int64Value $ vs V.! 0
getValue1 (D.U8Value _ vs) = UnsignedValue $ Int8Value $ fromIntegral $ vs V.! 0
getValue1 (D.U16Value _ vs) = UnsignedValue $ Int16Value $ fromIntegral $ vs V.! 0
getValue1 (D.U32Value _ vs) = UnsignedValue $ Int32Value $ fromIntegral $ vs V.! 0
getValue1 (D.U64Value _ vs) = UnsignedValue $ Int64Value $ fromIntegral $ vs V.! 0
getValue1 (D.F16Value _ vs) = FloatValue $ Float16Value $ vs V.! 0
getValue1 (D.F32Value _ vs) = FloatValue $ Float32Value $ vs V.! 0
getValue1 (D.F64Value _ vs) = FloatValue $ Float64Value $ vs V.! 0
getValue1 (D.BoolValue _ vs) = BoolValue $ vs V.! 0

unflattenPrims' :: Primitive -> Value PrimValue
unflattenPrims' p | [] <- D.valueElems p = Leaf $ getValue1 p
unflattenPrims' p | vs <- D.valueElems p = Array $ A.array (0, length vs - 1) $ zip [0..] $ map unflattenPrims' vs

unflattenPrims :: Value Primitive -> Value PrimValue
unflattenPrims (Leaf p) = unflattenPrims' p
unflattenPrims (Array a) = Array $ fmap unflattenPrims a
unflattenPrims (Record m) = Record $ M.map unflattenPrims m
unflattenPrims (Variant v fs) = Variant v $ map unflattenPrims fs

pack :: I.Value m -> Value PrimValue
pack (I.ValuePrim p) = Leaf p
pack (I.ValueArray _ a) = Array $ pack <$> a
pack (I.ValueRecord m) = Record $ M.map pack m
pack (I.ValueSum _ v fs) = Variant v $ map pack fs
pack v = error $ "Unable to pack " ++ show v

unpack :: I.ValueShape -> Value PrimValue -> I.Value m
unpack I.ShapeLeaf (Leaf p) = I.ValuePrim p
unpack s@(I.ShapeDim _ es) (Array a) = I.ValueArray s $ unpack es <$> a
unpack (I.ShapeRecord sm) (Record m) = I.ValueRecord $ M.intersectionWith unpack sm m -- TODO: Validate the shape?
unpack s@(I.ShapeSum sm) (Variant v fs) = I.ValueSum s v $ zipWith unpack (sm M.! v) fs -- TODO: Validate the shape? (Only best-effort)
unpack s v = error $ "Shape " ++ show s ++ " doesn't fit value " ++ show v
