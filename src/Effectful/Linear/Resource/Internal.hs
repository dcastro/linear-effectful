{-# OPTIONS_HADDOCK not-home #-}

module Effectful.Linear.Resource.Internal where

import Control.Functor.Linear qualified as Linear
import Data.IORef (IORef)
import Data.Unrestricted.Linear (Ur (..))
import Effectful
import Effectful.Dispatch.Static (SideEffects (..), StaticRep)
import Effectful.Linear.Dispatch.Static qualified as LinearStatic
import Effectful.Linear.Internal
import System.IO.Resource.Linear (RIO)
import System.IO.Resource.Linear.Internal qualified as Internal

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
