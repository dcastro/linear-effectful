module Effectful.Linear.Resource
  ( WithResource,

    -- * Creating new types of resources
    RIO.Resource,

    -- * RIO Compatibility
    toRIO,
    fromRIO,

    -- * Unsafe
    unsafeResourceLEff_,
  )
where

import Effectful.Linear.Resource.Internal
import System.IO.Resource.Linear qualified as RIO
