module Effectful.LinearLocks
  ( -- * Effect
    Locks,
    runLocks,

    -- * Lock scope
    LL.LockKey,
    lockScope,
    dropKey,
    LL.NestedLocksScopeException (..),

    -- * Lock sets
    LL.LockSet,
    LL.IsLockSet (), -- Note: do not export the typeclass members
    newLockSet,
    acquireMany,
  )
where

import Control.Monad.IO.Class.Linear qualified as Linear
import Data.Unrestricted.Linear (Ur)
import Effectful
import Effectful.Dispatch.Static (unsafeEff_)
import Effectful.Linear (LEff)
import Effectful.Linear qualified as LE
import Effectful.Linear.Resource (WithResource)
import Effectful.Linear.Resource qualified as LER
import Effectful.LinearLocks.Internal
import Effectful.LinearLocks.Utils.RedundantConstraint qualified as Utils
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
  (Locks :> es, IOE :> es) =>
  (LockKey 0 %1 -> LEff (WithResource ': es) (Ur a)) ->
  LEff es a
lockScope run =
  Utils.redundantConstraint @(Locks :> es) Control.do
    LE.unsafeLEff \env ->
      Linear.liftSystemIO (LL.lockScope @a \key -> LER.toRIO env (run key))

-- | Discard a key. Should be used after acquiring all the necessary locks in a lock scope.
dropKey :: forall lvl es. (Locks :> es, WithResource :> es) => LockKey lvl %1 -> LEff es ()
dropKey key =
  Utils.redundantConstraint @(Locks :> es) Control.do
    LER.fromRIO (LL.dropKey key)

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
