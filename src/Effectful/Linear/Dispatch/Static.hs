module Effectful.Linear.Dispatch.Static where

import Control.Monad.IO.Class.Linear qualified as Linear
import Data.Unrestricted.Linear (Ur)
import Effectful
import Effectful.Dispatch.Static (HasCallStack, StaticRep)
import Effectful.Dispatch.Static.Primitive (Env)
import Effectful.Dispatch.Static.Primitive qualified as Static
import Effectful.Linear.Internal
import Effectful.Linear.Utils qualified as Utils
import Effectful.Linear.Utils.RedundantConstraint (redundantConstraint)
import Effectful.WithIO (MaybeWithIO)

-- | Fetch the current representation of the effect.
getStaticRep ::
  (HasCallStack, DispatchOf e ~ 'Static sideEffects, e :> es) =>
  LEff es (Ur (StaticRep e))
getStaticRep =
  unsafeLEff \es ->
    Linear.liftSystemIOU (Static.getEnv es)

-- | Run a statically dispatched effect with the given initial representation
-- and return the final value, discarding the final representation.
evalStaticRep ::
  forall e sideEffects es a.
  (HasCallStack, DispatchOf e ~ Static sideEffects, MaybeWithIO sideEffects es) =>
  StaticRep e ->
  LEff (e : es) a %1 ->
  LEff es a
evalStaticRep staticRep (LEff act) = Linear.do
  -- NOTE: When this static effect `e` has side effects,
  -- this constraint forces the user to use `runWithIO` at the edge of the program,
  -- which in turn forces them to use `runEff` and forbids `runPureEff`.
  redundantConstraint @(MaybeWithIO sideEffects es) Linear.do
    unsafeLEff \env ->
      Utils.linearBracket @(Env (e : es)) @() @a
        (Linear.liftSystemIOU (Static.consEnv @e staticRep Static.dummyRelinker env))
        (\env -> Linear.liftSystemIO (Static.unconsEnv env))
        ( \env -> Linear.do
            act env
        )
