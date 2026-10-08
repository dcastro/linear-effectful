module Effectful.LinearLocks.Internal where

import Effectful
import Effectful.Dispatch.Static (SideEffects (..), StaticRep)
import Effectful.Dispatch.Static qualified as Static
import Effectful.Linear (LEff)
import Effectful.Linear.Resource (WithResource)
import Effectful.Linear.Resource qualified as LER
import Effectful.LinearLocks.Utils.RedundantConstraint qualified as Utils
import GHC.TypeLits (type (+), type (<=))
import LinearLocks (LockKey)
import LinearLocks.Internal qualified as LL.Internal

----------------------------------------------------------------------------
-- Effect
----------------------------------------------------------------------------
data Locks :: Effect

type instance DispatchOf Locks = 'Static 'WithSideEffects

data instance StaticRep Locks = Locks

runLocks :: (IOE :> es) => Eff (Locks : es) a -> Eff es a
runLocks = Static.evalStaticRep Locks

----------------------------------------------------------------------------
-- Lifted functions
----------------------------------------------------------------------------

-- | Acquires a lock.
-- Consumes the key and return a new key (with an increased level).
acquire ::
  forall keyLvl acquirable es.
  (LL.Internal.Acquirable acquirable) =>
  (keyLvl <= LL.Internal.Level acquirable) =>
  (Locks :> es, WithResource :> es) =>
  LockKey keyLvl %1 ->
  acquirable ->
  LEff es (LL.Internal.Guard acquirable, LockKey (LL.Internal.Level acquirable + 1))
acquire key m = L.do
  Utils.redundantConstraint @(Locks :> es) Control.do
    LER.fromRIO (LL.Internal.acquire key m)
