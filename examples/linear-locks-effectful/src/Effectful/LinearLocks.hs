module Effectful.LinearLocks
  ( -- * Effect
    Locks,
    runLocks,

    -- * Lock scope
    LL.LockKey,
    lockScope,
    lockScope',
    dropKey,
    dropKeyAndReturn,
    LL.NestedLocksScopeException (..),

    -- * Lock sets
    LL.LockSet,
    LL.IsLockSet (), -- Note: do not export the typeclass members
    newLockSet,
    acquireMany,
  )
where

import Data.Unrestricted.Linear (Ur)
import Effectful
import Effectful.Dispatch.Static (unsafeEff, unsafeEff_)
import Effectful.Linear (LEff, liftEff)
import Effectful.Linear.Resource (WithResource)
import Effectful.Linear.Resource qualified as LER
import Effectful.LinearLocks.Internal
import Effectful.LinearLocks.Utils.RedundantConstraint qualified as Utils
import Effectful.WithIO (WithIO)
import GHC.TypeLits (type (+), type (<=))
import LinearLocks (IsLockSet, LockKey, LockSet)
import LinearLocks qualified as LL
import LinearLocks.Internal.LockSet qualified as LL.Internal

----------------------------------------------------------------------------
-- Lifted functions
----------------------------------------------------------------------------

-- | Creates a new lock scope with a key of level 0, and runs the given function with it.
--  The key can be used to acquire locks with `acquire` and `LinearLocks.acquireMany`.
--
-- After acquiring all the necessary locks, the key must be dropped with
-- `dropKey` or `dropKeyAndReturn`.
--
-- Will throw a t`NestedLocksScopeException` if a nested `lockScope` is created at runtime.
lockScope ::
  forall a es.
  (Locks :> es, WithIO :> es) =>
  (LockKey 0 %1 -> LEff (WithResource ': es) (Ur a)) ->
  Eff es a
lockScope run =
  Utils.redundantConstraint @(Locks :> es) Control.do
    unsafeEff \env ->
      LL.lockScope @a \key -> LER.toRIO env (run key)

-- Like 'lockScope' but returns in 'LEff'
lockScope' ::
  forall a es.
  (Locks :> es, WithIO :> es) =>
  (LockKey 0 %1 -> LEff (WithResource ': es) (Ur a)) ->
  LEff es a
lockScope' =
  liftEff . lockScope

-- | Discard a key. Should be used after acquiring all the necessary locks in a lock scope.
dropKey :: forall lvl es. (Locks :> es, WithResource :> es) => LockKey lvl %1 -> LEff es ()
dropKey key =
  Utils.redundantConstraint @(Locks :> es) Control.do
    LER.fromRIO (LL.dropKey key)

-- | Discard a key. Should be used after acquiring all the necessary locks in a lock scope.
dropKeyAndReturn :: forall a lvl es. (Locks :> es, WithResource :> es) => LockKey lvl %1 -> a -> LEff es (Ur a)
dropKeyAndReturn key a =
  Utils.redundantConstraint @(Locks :> es) Control.do
    LER.fromRIO (LL.dropKeyAndReturn key a)

newLockSet :: forall es set. (IsLockSet set, Locks :> es) => set -> Eff es (LockSet set)
newLockSet set =
  Utils.redundantConstraint @(Locks :> es) Control.do
    unsafeEff_ $
      LL.newLockSet set

acquireMany ::
  forall keyLvl lockLvl set es.
  (IsLockSet set, lockLvl ~ LL.Internal.LockSetLevel set, keyLvl <= lockLvl, Locks :> es, WithResource :> es) =>
  LockKey keyLvl %1 ->
  LockSet set ->
  LEff es (LL.Internal.LockSetGuard set, LockKey (lockLvl + 1))
acquireMany key lockSet =
  Utils.redundantConstraint @(Locks :> es) Control.do
    LER.fromRIO (LL.acquireMany key lockSet)
