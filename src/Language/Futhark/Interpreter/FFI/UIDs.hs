module Language.Futhark.Interpreter.FFI.UIDs
  ( EntryPointUID,
    TypeUID,
    ValueUID,
    UID.uid,
    UIDSource,
    UIDSourceT,
    UID.runUIDSourceT,
    UIDSourceM,
    UID.runUIDSourceM,
    MonadUIDSource,
    UID.getUID,
    UID.getUIDs,
  )
where

import Language.Futhark.Interpreter.FFI.Util.UID qualified as UID

data Entry

data Type

data Value

type EntryPointUID = UID.UID Entry Word

type TypeUID = UID.UID Type Word

type ValueUID = UID.UID Value Word

type UIDSource = UID.UIDSource Word

type UIDSourceT = UID.UIDSourceT Word

type UIDSourceM = UID.UIDSourceM Word

type MonadUIDSource = UID.MonadUIDSource Word
