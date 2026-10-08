{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE NoFieldSelectors #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Test.Effectful.LinearLocks.MutexSpec where

import Control.Functor.Linear qualified as L
import Control.Monad (void)
import Data.Function ((&))
import Effectful
import Effectful.Concurrent (Concurrent, ThreadId, myThreadId, runConcurrent)
import Effectful.Concurrent.MVar qualified as Eff
import Effectful.Concurrent.STM (atomically)
import Effectful.Exception (SomeException, throwIO, try)
import Effectful.Fail (Fail, runFailIO)
import Effectful.Linear
import Effectful.LinearLocks
import Effectful.LinearLocks.Mutex qualified as Mutex
import LinearLocks.Internal qualified as Internal
import LinearLocks.Internal.Mutex qualified as Internal
import ListT qualified
import Prelude.Linear (Ur (..))
import Prelude.Linear qualified as L hiding (IO)
import StmContainers.Set qualified as StmSet
import Test.Effectful.LinearLocks.Utils
import Test.Syd

type TestEffects = '[Locks, Fail, Concurrent, IOE]

type EffTest_ = EffTest ()

type EffTest = Eff TestEffects

instance IsTest EffTest_ where
  type Arg1 EffTest_ = Arg1 (IO ())
  type Arg2 EffTest_ = Arg2 (IO ())
  runTest eff = runTest $ runEffTest eff

runEffTest :: EffTest_ -> IO ()
runEffTest eff =
  eff
    & runLocks
    & runFailIO
    & runConcurrent
    & runEff

-- | Doctests
--
-- >>> :{
-- >>> unit_mutexes_cannot_be_locked_in_wrong_order :: Locks :> es => Eff es ()
-- >>> unit_mutexes_cannot_be_locked_in_wrong_order = do
-- >>>   m1 <- Mutex.new 2 "hello"
-- >>>   m2 <- Mutex.new 4 "world"
-- >>>   runLEff L.do
-- >>>     lockScope \key -> L.do
-- >>>       (mg2, key) <- Mutex.acquire key m2
-- >>>       (mg1, key) <- Mutex.acquire key m1
-- >>>       Mutex.release mg1
-- >>>       Mutex.release mg2
-- >>>       dropKey key
-- >>>       L.pure (Ur (Ur ()))
-- >>> :}
-- ...
-- ... • Cannot satisfy: 5 <= 2
-- ... • In a stmt of a 'do' block: (mg1, key) <- Mutex.acquire key m1
-- ...
spec :: Spec
spec = describe "Mutex" do
  it @_ @_ @EffTest_ "read mutex" do
    mutex <- Mutex.new @String 0 "hello"
    str <- runLEff L.do
      lockScope \key -> L.do
        (mg, key) <- Mutex.acquire key mutex
        (Ur str, mg) <- Mutex.read mg
        Mutex.release mg
        dropKey key
        L.pure (Ur (Ur str))
    liftIO $ str `shouldBe` "hello"
    pure ()

  it @_ @_ @EffTest_ "write mutex" do
    mutex <- Mutex.new @String 0 "hello"
    runLEff L.do
      lockScope \key -> L.do
        (mg, key) <- Mutex.acquire key mutex
        mg <- Mutex.write mg "world"
        Mutex.release mg
        dropKey key
        L.pure (Ur (Ur ()))

    str <- runLEff L.do
      lockScope \key -> L.do
        (mg, key) <- Mutex.acquire key mutex
        (Ur str, mg) <- Mutex.read mg
        Mutex.release mg
        dropKey key
        L.pure (Ur (Ur str))

    liftIO $ str `shouldBe` "world"

    str <- Eff.readMVar mutex.var
    liftIO $ str `shouldBe` "world"

  it @_ @_ @EffTest_ "realeases mvar" do
    mutex <- Mutex.new @String 0 "hello"
    runLEff L.do
      lockScope \key -> L.do
        (mg, key) <- Mutex.acquire key mutex

        liftEff do
          isEmpty <- Eff.isEmptyMVar mutex.var
          liftIO $ isEmpty `shouldBe` True

        Mutex.release mg

        liftEff do
          isEmpty <- Eff.isEmptyMVar mutex.var
          liftIO $ isEmpty `shouldBe` False

        dropKey key
        L.pure (Ur (Ur ()))

    isEmpty <- Eff.isEmptyMVar mutex.var
    liftIO $ isEmpty `shouldBe` False

  it @_ @_ @EffTest_ "can't nest lock scopes" do
    let run =
          runLEff L.do
            lockScope \key -> L.do
              liftEff do
                runLEff L.do
                  lockScope \key -> L.do
                    dropKey key
                    L.pure (Ur (Ur ()))
              dropKey key
              L.pure (Ur (Ur ()))

    liftIO $ runEffTest run `shouldThrow` \(_ :: NestedLocksScopeException) -> True

  it @_ @_ @EffTest_ "updates thread ids" do
    let getThreadIds :: (Concurrent :> es) => Eff es [ThreadId]
        getThreadIds =
          Internal.lockScopes & StmSet.listT & ListT.toList & atomically
    tid <- myThreadId

    getThreadIds >>= \tids -> liftIO $ tids `shouldNotContain` [tid]
    runLEff L.do
      lockScope \key -> L.do
        liftEff L.$ getThreadIds >>= \tids -> liftIO $ tids `shouldContain` [tid]
        dropKey key
        L.pure (Ur (Ur ()))
    getThreadIds >>= \tids -> liftIO $ tids `shouldNotContain` [tid]

    -- Check that the thread ID is removed even if an exception is thrown.
    let run =
          runLEff L.do
            lockScope \key -> L.do
              liftEff L.$ getThreadIds >>= \tids -> liftIO $ tids `shouldContain` [tid]
              liftEff L.$ throwIO (userError "oops")
              dropKey key
              L.pure (Ur (Ur ()))
    liftIO $ runEffTest run `shouldThrow` anyIOException
    getThreadIds >>= \tids -> liftIO $ tids `shouldNotContain` [tid]

    -- Check that the thread ID is removed even if when a nested lock scope is attempted
    let run =
          runLEff L.do
            lockScope \key -> L.do
              liftEff L.$ getThreadIds >>= \tids -> liftIO $ tids `shouldContain` [tid]
              liftEff do
                runLEff L.do
                  lockScope \key -> L.do
                    dropKey key
                    L.pure (Ur (Ur ()))
              dropKey key
              L.pure (Ur (Ur ()))
    liftIO $ runEffTest run `shouldThrow` \(_ :: NestedLocksScopeException) -> True
    getThreadIds >>= \tids -> liftIO $ tids `shouldNotContain` [tid]

    -- Check that the thread ID is NOT removed if a nested lock scope is caught
    runLEff L.do
      lockScope \key -> L.do
        liftEff L.$ getThreadIds >>= \tids -> liftIO $ tids `shouldContain` [tid]
        liftEff do
          Left _ <- try @SomeException $
            runLEff L.do
              lockScope \key -> L.do
                dropKey key
                L.pure (Ur (Ur ()))
          pure ()
        liftEff L.$ getThreadIds >>= \tids -> liftIO $ tids `shouldContain` [tid]
        dropKey key
        L.pure (Ur (Ur ()))
    getThreadIds >>= \tids -> liftIO $ tids `shouldNotContain` [tid]

  it @_ @_ @EffTest_ "rolls back on exception" do
    mutex <- Mutex.new @String 0 "hello"
    Left _ <- try @SomeException $
      runLEff L.do
        lockScope \key -> L.do
          (mg, key) <- Mutex.acquire key mutex
          mg <- Mutex.write mg "world"
          liftEff $ throwIO (userError "oops")
          Mutex.release mg
          dropKey key
          L.pure (Ur (Ur ()))

    -- The MVar should have been released, and the original value should have been put back into the MVar.
    mbResult <- Eff.tryTakeMVar mutex.var
    liftIO $ mbResult `shouldBe` Just "hello"

  it @_ @_ @EffTest_ "rolls back on imprecise exception" do
    mutex <- Mutex.new @String 0 "hello"
    Left _ <- try @SomeException do
      runLEff L.do
        lockScope \key -> L.do
          (mg, key) <- Mutex.acquire key mutex
          mg <- Mutex.write mg "world"
          error "err"
          Mutex.release mg
          dropKey key
          L.pure (Ur (Ur ()))

    -- The MVar should have been released, and the original value should have been put back into the MVar.
    mbResult <- Eff.tryTakeMVar mutex.var
    liftIO $ mbResult `shouldBe` Just "hello"

  it @_ @_ @EffTest_ "new doesn't evaluate value to normal form" do
    -- This should not throw, the "error" thunk should not be evaluated
    void $ Mutex.new @[Int] 0 [1, 2, error "oops", 4]

  it @_ @_ @EffTest_ "release doesn't evaluate value to normal form" do
    mutex <- Mutex.new @[Int] 0 [1]

    runLEff L.do
      lockScope \key -> L.do
        (mg, key) <- Mutex.acquire key mutex
        -- This should not throw, the "error" thunk should not be evaluated
        mg <- Mutex.write mg [1, 2, error "oops", 4]
        -- This should not throw
        Mutex.release mg
        dropKey key
        L.pure (Ur (Ur ()))
