module Effectful.Linear.Dispatch.Static where

import Control.Monad.IO.Class.Linear qualified as Linear
import Data.Unrestricted.Linear (Ur)
import Effectful
import Effectful.Dispatch.Static (HasCallStack, StaticRep)
import Effectful.Dispatch.Static.Primitive qualified as Static
import Effectful.Linear.Internal

-- | Fetch the current representation of the effect.
getStaticRep ::
  (HasCallStack, DispatchOf e ~ 'Static sideEffects, e :> es) =>
  LEff es (Ur (StaticRep e))
getStaticRep =
  unsafeLEff \es ->
    Linear.liftSystemIOU (Static.getEnv es)
