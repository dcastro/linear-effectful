module Effectful.Linear.Utils where

import Control.Exception qualified as Exception
import Control.Functor.Linear qualified as Linear
import Data.Unrestricted.Linear (Consumable, Ur (..), lseq)
import GHC.IO qualified as System
import System.IO.Linear qualified as Linear
import Unsafe.Linear qualified as Unsafe

-- TODO: open a PR in `linear-base` with this.
linearBracket ::
  forall a b c.
  (Consumable b) =>
  -- The resource `a` must be wrapped in `Ur`: it may be used more than once.
  -- The arrow is linear: `acquire` will be used once (assuming `bracket` doesn't throw).
  Linear.IO (Ur a) %1 ->
  -- The value of the `release` function will be discarded, so it has to be `Consumable`
  -- The arrow is linear: `release` will be used once (assuming `bracket` doesn't throw).
  (a -> Linear.IO b) %1 ->
  -- The arrow is linear: `use` will be used once (assuming `bracket` doesn't throw).
  (a -> Linear.IO c) %1 ->
  Linear.IO c
linearBracket = Unsafe.toLinear3 bracket'
  where
    bracket' ::
      Linear.IO (Ur a) ->
      (a -> Linear.IO b) ->
      (a -> Linear.IO c) ->
      Linear.IO c
    bracket' acquire release use = Linear.do
      let acquire' = unsafeToSystemIO acquire
      let release' (Ur a) = unsafeToSystemIO Linear.do
            b <- release a
            -- Consume `b`
            b `lseq` Linear.pure ()
      let use' (Ur a) = unsafeToSystemIO (use a)
      Linear.fromSystemIO
        ( Exception.bracket @(Ur a) @() @c
            acquire'
            release'
            use'
        )

-- | This function is marked as "unsafe" because it discards the linear constraint on `a`.
unsafeToSystemIO :: Linear.IO a %1 -> System.IO a
unsafeToSystemIO (Linear.IO m) =
  System.IO (\s -> m s)
