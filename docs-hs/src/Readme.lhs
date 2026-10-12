\begin{code}%hidden
{-# LANGUAGE GHC2024 #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QualifiedDo #-}

module Readme where

import Control.Functor.Linear qualified as Linear
import Effectful
import Prelude.Linear (Ur (..))
import Effectful.LinearLocks
import Effectful.LinearLocks.Mutex qualified as Mutex
import Effectful.WithIO (WithIO)
\end{code}


`linear-effectful`
===

This repository is (for now) a proof of concept for using [`effectful`'s effect system][effectful] with
[linear `IO` from `linear-base`][linear-io].


It defines a `LEff` monad (read: "linear effect").
You can embed `LEff` programs in `Eff` programs using `runLEff`, and vice-versa using `liftEff`.

It also defines a `WithResource` effect that can only be run in `LEff`, isomorphic to [`linear-base`'s `RIO`][rio], with the following guarantees:

* Resources must always be released.
* Resources cannot be released twice.
* Resources cannot be used after being released.
* Resources cannot escape the scope where they were created.

A quick demo of a `FileSystem` effect built on top of `LEff` + `WithResource` can be found in
[test/Example/Effectful/Linear/Demo.hs](test/Example/Effectful/Linear/Demo.hs)


This repo also contains a `LEff` "adapter" for [`linear-locks`][linear-locks] in [./examples/linear-locks-effectful/](./examples/linear-locks-effectful/).
When/if `linear-effectful` leaves the "proof of concept" stage, `linear-locks-effectful` will be moved to its own repo.

\begin{code}

app2 :: (Locks :> es, WithIO :> es) => Eff es String
app2 = do
  mutex <- Mutex.new @String 0 "hello"
  lockScope \key -> Linear.do
    (mg, key) <- Mutex.acquire key mutex
    (Ur str, mg) <- Mutex.read mg
    Mutex.release mg
    dropKeyAndReturn key str
\end{code}

Known quirks
---

In `effectful`, static effects can signal that they use `IO` by setting `DispatchOf` to `WithSideEffects`.
`evalStaticRep` + `MaybeIOE` will then ensure that the effect is interpreted into `IOE` at "the edge of the program" (e.g. in `main`).

However, given that (1) `LEff` programs are meant to be embedded in larger `Eff` programs, and (2) the static effect `WithResource` has to be interpreted _within_ a `LEff` program,
that means that `WithResource` will **NOT** be handled at "the edge of the program".

And interpreting an effect into `IOE` in the middle of a program is not safe, as it introduces the ability to run arbitrary IO.

For this reason, in this package, static effects are not interpreted into `IOE`; instead, they're interpreted into `WithIO`.

* `WithIO` is similar to `IOE` in that it forbids the usage of `runPureEff`.
* But unlike `IOE`, it does not have `MonadIO` / `MonadUnliftIO` instances.

Thus having it in scope is safe, as it does not allow running arbitrary IO.



  [effectful]: https://hackage.haskell.org/package/effectful
  [linear-io]: https://hackage-content.haskell.org/package/linear-base/docs/System-IO-Linear.html
  [rio]:       https://hackage-content.haskell.org/package/linear-base/docs/System-IO-Resource-Linear.html
  [linear-locks]: https://hackage.haskell.org/package/linear-locks
