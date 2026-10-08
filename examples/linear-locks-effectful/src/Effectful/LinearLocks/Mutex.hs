{-# LANGUAGE RequiredTypeArguments #-}

module Effectful.LinearLocks.Mutex
  ( -- * Mutex
    Mutex.Mutex,
    new,
    acquire,

    -- * Mutex guards
    Mutex.MutexGuard,
    read,
    write,
    release,
  )
where

import Data.Unrestricted.Linear (Ur)
import Effectful
import Effectful.Dispatch.Static (unsafeEff_)
import Effectful.Linear (LEff)
import Effectful.Linear.Resource (WithResource)
import Effectful.Linear.Resource qualified as LER
import Effectful.LinearLocks.Internal
import Effectful.LinearLocks.Utils.RedundantConstraint qualified as Utils
import GHC.TypeLits (Nat)
import LinearLocks.Mutex (Mutex, MutexGuard)
import LinearLocks.Mutex qualified as Mutex
import Prelude hiding (read)

-- | Creates a new mutex with the given initial value.
--
-- The @lvl@ parameter determines the order in which this mutex can be acquired relative to other mutexes.
--
-- It does not have to be unique, multiple mutexes can have the same level.
-- Mutexes with the same level can be added to a t`LinearLocks.LockSet` and acquired with 'LinearLocks.acquireMany'.
new :: forall a es. forall (lvl :: Nat) -> a -> Eff es (Mutex lvl a)
new lvl a = do
  unsafeEff_ $
    Mutex.new lvl a

read ::
  forall a es.
  (Locks :> es, WithResource :> es) =>
  MutexGuard a %1 -> LEff es (Ur a, MutexGuard a)
read mg =
  Utils.redundantConstraint @(Locks :> es) Control.do
    LER.fromRIO
      (Mutex.read mg)

-- | Writes a new value to the mutex, which will be committed when the guard is released.
--
-- If an exception is thrown after `write` but before `release`,
-- the mutex will be rolled back to its original state.
write :: forall a es. (Locks :> es, WithResource :> es) => MutexGuard a %1 -> a -> LEff es (MutexGuard a)
write mg newValue =
  Utils.redundantConstraint @(Locks :> es) Control.do
    LER.fromRIO
      (Mutex.write mg newValue)

-- | Releases the mutex and commits the latest value set by `write`.
release :: forall a es. (Locks :> es, WithResource :> es) => MutexGuard a %1 -> LEff es ()
release mg =
  Utils.redundantConstraint @(Locks :> es) Control.do
    LER.fromRIO
      (Mutex.release mg)
