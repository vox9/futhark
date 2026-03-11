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
  )
where

import Data.Map qualified as M
import Language.Futhark.Core (Name)
import Language.Futhark.Interpreter.FFI.Values
import Control.Arrow (Arrow (first))
import Control.Monad.Trans.Class
import Control.Monad.State (StateT (runStateT), MonadState (state, get, put), MonadIO)
import Language.Futhark.Interpreter.FFI.ExID

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
