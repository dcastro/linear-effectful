{-# OPTIONS_HADDOCK not-home #-}

module Effectful.Linear.Resource.Internal where

import Control.Functor.Linear qualified as Linear
import Control.Monad.IO.Class.Linear qualified as Linear
import Data.IORef (IORef)
import Data.Unrestricted.Linear (Ur (..))
import Effectful
import Effectful.Dispatch.Static (SideEffects (..), StaticRep)
import Effectful.Dispatch.Static.Primitive (Env)
import Effectful.Linear.Dispatch.Static qualified as LinearStatic
import Effectful.Linear.Internal
import System.IO.Linear qualified as Linear
import System.IO.Resource.Linear (RIO)
import System.IO.Resource.Linear qualified as RIO
import System.IO.Resource.Linear.Internal qualified as Internal
import System.IO.Resource.Linear.Internal qualified as RIO.Internal

-- | An effect that allows safely acquiring and releasing resources in a linear monad.
-- See: "System.IO.Resource.Linear"
data WithResource :: Effect

type instance DispatchOf WithResource = 'Static 'WithSideEffects

newtype instance StaticRep WithResource = WithResource (IORef Internal.ReleaseMap)

runResource ::
  forall a es.
  (IOE :> es) =>
  -- NOTE: the `Ur a` prevents linear variables from *escaping* this scope,
  -- and the non-linear arrow prevents linear variables from *entering* this scope.
  LEff (WithResource : es) (Ur a) -> LEff es a
runResource action =
  unsafeLEff \env -> do
    Linear.liftSystemIO
      (RIO.run (toRIO env action))

----------------------------------------------------------------------------
-- Creating new types of resources
----------------------------------------------------------------------------

-- | Given a resource in the "System.IO.Linear.IO" monad, and
-- given a function to release that resource, provides that resource in
-- the @RIO@ monad. For example, releasing a @Handle@ from "System.IO"
-- would be done with @fromSystemIO hClose@. Because this release function
-- is an input, and could be wrong, this function is unsafe.
unsafeAcquire :: (WithResource :> es) => Linear.IO (Ur a) -> (a -> Linear.IO ()) -> LEff es (RIO.Resource a)
unsafeAcquire acquire release = fromRIO (RIO.unsafeAcquire acquire release)

-- | @'release' r@ calls the release function provided when @r@ was acquired.
release :: (WithResource :> es) => RIO.Resource a %1 -> LEff es ()
release res = fromRIO (RIO.release res)

----------------------------------------------------------------------------
-- RIO Compatibility
----------------------------------------------------------------------------

toRIO :: forall a es. (IOE :> es) => Env es -> LEff (WithResource : es) a %1 -> RIO a
toRIO env action =
  -- Create a RIO action that takes its internal "ReleaseMap",
  -- uses it to register a static effect in the Env,
  -- runs the given effectful action,
  -- and then removes the static effect from the Env.
  Linear.do
    Ur rm <- getReleaseMap
    Linear.liftIO
      ( unLEff
          (LinearStatic.evalStaticRep (WithResource rm) action)
          env
      )
  where
    getReleaseMap :: RIO (Ur (IORef RIO.Internal.ReleaseMap))
    getReleaseMap = RIO.Internal.RIO \rm -> Linear.pure (Ur rm)

fromRIO :: (WithResource :> es) => RIO a %1 -> LEff es a
fromRIO (RIO.Internal.RIO rio) =
  -- NOTE: even though this function can be used to introduce arbitrary "IO" actions,
  -- it cannot do so in *pure* effectful computations,
  -- because the `WithResource` effect is marked with `WithSideEffects`.
  -- Therefore, its name is not prefixed with "unsafe".
  Linear.do
    Ur (WithResource rm) <- LinearStatic.getStaticRep @WithResource
    unsafeLEff_ (rio rm)
