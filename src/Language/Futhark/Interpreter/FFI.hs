module Language.Futhark.Interpreter.FFI
  ( ExTypeAtom,
    ExType,
    ExValueAtom,
    ExValue,
    Function (..),
    Interface (..),
    InFunction,
    InInterface,
    ExFunction,
    ExInterface,
    fromInterpreterValue,
    toInterpreterValue
  )
where

import Data.Map qualified as M
import Language.Futhark.Core (Name, nameToText, nameFromText)
import Language.Futhark.Interpreter.FFI.Values
import Language.Futhark.Interpreter.FFI.ExID
import Language.Futhark.Interpreter.Values qualified as I

data Function a
  = Function [Type a] (Type a)
  deriving (Show, Eq)

newtype Interface a
  = Interface (M.Map Name (Function a))
  deriving Show

type InFunction = Function PrimitiveType
type InInterface = Interface PrimitiveType

type ExTypeAtom = Either ExTypeID PrimitiveType
type ExValueAtom = Either ExValueID PrimitiveValue

type ExType = Type ExTypeAtom
type ExValue = Value ExValueAtom

type ExFunction = Function ExTypeAtom
type ExInterface = Interface ExTypeAtom

fromInterpreterValue :: I.Value m -> ExValue
fromInterpreterValue (I.ValuePrim v) = Atom $ Right $ fromPrimValue v
fromInterpreterValue (I.ValueArray _ a) = Array $ fmap fromInterpreterValue a
fromInterpreterValue (I.ValueRecord m) = Record $ M.map fromInterpreterValue $ M.mapKeys nameToText m
fromInterpreterValue (I.ValueSum _ n v) = Sum (nameToText n) $ map fromInterpreterValue v
fromInterpreterValue (I.ValueExt vid) = Atom $ Left vid
fromInterpreterValue _ = error "TODO (qu9wdaoijlm)"

toInterpreterValue :: ExValue -> I.Value m
toInterpreterValue (Atom (Right v)) = I.ValuePrim $ toPrimValue v
-- TODO: Add shape?
toInterpreterValue (Array a) = I.ValueArray (I.ShapeDim 0 I.ShapeLeaf) $ fmap toInterpreterValue a
toInterpreterValue (Record m) = I.ValueRecord $ M.map toInterpreterValue $ M.mapKeys nameFromText m
-- TODO: Add shape?
toInterpreterValue (Sum n v) = I.ValueSum (I.ShapeSum M.empty) (nameFromText n) $ map toInterpreterValue v
toInterpreterValue (Atom (Left vid)) = I.ValueExt vid
