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
import System.IO.Resource.Linear (RIO)
import System.IO.Resource.Linear.Internal qualified as Internal
import System.IO.Resource.Linear.Internal qualified as RIO.Internal

-- | An effect that allows safely acquiring and releasing resources in a linear monad.
-- See: "System.IO.Resource.Linear"
data WithResource :: Effect

type instance DispatchOf WithResource = 'Static 'WithSideEffects

newtype instance StaticRep WithResource = WithResource (IORef Internal.ReleaseMap)

-- | This function is unsafe because it can be used to introduce arbitrary "IO" actions into pure `Eff` computations.
unsafeResourceLEff_ :: (WithResource :> es) => RIO a %1 -> LEff es a
unsafeResourceLEff_ (Internal.RIO rio) =
  Linear.do
    Ur (WithResource releaseMap) <- LinearStatic.getStaticRep @WithResource
    unsafeLEff_ (rio releaseMap)

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
