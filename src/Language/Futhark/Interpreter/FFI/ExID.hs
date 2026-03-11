{-# LANGUAGE UndecidableInstances #-}
module Language.Futhark.Interpreter.FFI.ExID
  ( -- External IDs
    ExID,
    ExEntryID,
    exEntryID,
    ExTypeID,
    exTypeID,
    ExValueID,
    exValueID,

    -- External ID source
    ExIDSrc,
    initIDSrc,
    entryID,
    typeID,
    valueID,

    -- External ID source monad transformer
    ExIDSrcT,
    runExIDSrcT,
    ExIDSrcM,
    runExIDSrcM,
    getEntryID,
    getTypeID,
    getValueID
  )
where

import Control.Arrow (Arrow (first))
import Control.Monad.Trans.Class
import Control.Monad.State (StateT (runStateT), MonadState (state, get, put), MonadIO)
import Data.Functor.Identity (Identity (runIdentity))
import Control.Monad.RWS (MonadReader (ask, local))

-- External IDs
newtype ExID = ExID Int
  deriving (Show, Eq, Ord)

newtype ExEntryID = ExEntryID ExID
  deriving (Show, Eq, Ord)

exEntryID :: ExEntryID -> Int
exEntryID (ExEntryID (ExID i)) = i

newtype ExTypeID = ExTypeID ExID
  deriving (Show, Eq, Ord)

exTypeID :: ExTypeID -> Int
exTypeID (ExTypeID (ExID i)) = i

newtype ExValueID = ExValueID ExID
  deriving (Show, Eq, Ord)

exValueID :: ExValueID -> Int
exValueID (ExValueID (ExID i)) = i

-- External ID source
newtype ExIDSrc = ExIDSrc ExID

initIDSrc :: ExIDSrc
initIDSrc = ExIDSrc $ ExID 0

nextID :: ExIDSrc -> (ExID, ExIDSrc)
nextID (ExIDSrc (ExID i)) =
  let eid = ExID $ i + 1
   in (eid, ExIDSrc eid)

entryID :: ExIDSrc -> (ExEntryID, ExIDSrc)
entryID = first ExEntryID . nextID

typeID :: ExIDSrc -> (ExTypeID, ExIDSrc)
typeID = first ExTypeID . nextID

valueID :: ExIDSrc -> (ExValueID, ExIDSrc)
valueID = first ExValueID . nextID

-- External ID source monad transformer
newtype ExIDSrcT m a = ExIDSrcT (StateT ExIDSrc m a)
  deriving (Functor, Applicative, Monad, MonadTrans, MonadIO)

type ExIDSrcM = ExIDSrcT Identity

instance MonadState s m => MonadState s (ExIDSrcT m) where
  get = lift get
  put = lift . put
  state = lift . state

instance MonadReader r m => MonadReader r (ExIDSrcT m) where
  ask = lift ask
  local f (ExIDSrcT m) = ExIDSrcT (local f m)

runExIDSrcT :: ExIDSrcT m a -> ExIDSrc -> m (a, ExIDSrc)
runExIDSrcT (ExIDSrcT m) s = runStateT m s

runExIDSrcM :: ExIDSrcT Identity a -> ExIDSrc -> (a, ExIDSrc)
runExIDSrcM m = runIdentity . runExIDSrcT m

getEntryID :: Monad m => ExIDSrcT m ExEntryID
getEntryID = ExIDSrcT $ state entryID

getTypeID :: Monad m => ExIDSrcT m ExTypeID
getTypeID = ExIDSrcT $ state typeID

getValueID :: Monad m => ExIDSrcT m ExValueID
getValueID = ExIDSrcT $ state valueID
