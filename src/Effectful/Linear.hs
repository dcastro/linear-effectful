module Effectful.Linear
  ( LEff,
    runLEff,
    unLEff,
    liftEff,
    liftEffU,

    -- * Unsafe
    unsafeLEff,
    unsafeLEff_,
  )
where

import Effectful.Linear.Internal
