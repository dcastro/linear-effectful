{-# LANGUAGE AllowAmbiguousTypes #-}
{-# OPTIONS_GHC -Wno-redundant-constraints #-}

module Effectful.LinearLocks.Utils.RedundantConstraint where

import Data.Kind (Constraint)
import Prelude.Linear qualified as PL

-- | Ignore a constraint without having to set `-Wno-redundant-constraints` on the entire module.
{-# INLINE redundantConstraint #-}
redundantConstraint :: forall (c :: Constraint) a. (c) => a %1 -> a
redundantConstraint = PL.id
