module Effectful.Linear.Resource
  ( WithResource,
    runResource,

    -- * Creating new types of resources
    RIO.Resource,
    unsafeAcquire,
    release,

    -- * RIO Compatibility
    toRIO,
    unsafeFromRIO,
  )
where

import Effectful.Linear.Resource.Internal
import System.IO.Resource.Linear qualified as RIO
