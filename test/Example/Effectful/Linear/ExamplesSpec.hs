module Example.Effectful.Linear.ExamplesSpec where

import Control.Functor.Linear qualified as Linear
import Data.Text (Text)
import Data.Unrestricted.Linear (Ur (..))
import Effectful
import Effectful.Linear
import Effectful.Linear.Resource (WithResource)
import Effectful.Linear.Resource qualified as R
import Effectful.State.Static.Local (State, modify)
import Effectful.WithIO (WithIO)
import System.IO.Resource.Linear qualified as RIO
import Test.Syd

-- $setup
-- >>> import Effectful
-- >>> import Effectful.Linear
-- >>> import Effectful.Linear.Resource qualified as R
-- >>> import Effectful.WithIO (WithIO)

-- | Doctests
--
-- `runResource` doesn't force `IOE` into scope (which would allow arbitrary IO).
--
-- >>> :{
-- test :: (WithIO :> es) => Eff es Text
-- test = do
--   liftIO $ putStrLn "test"
--   runLEff (R.runResource test2)
-- :}
-- ...
-- ... • Could not deduce ‘IOE :> es’ arising from a use of ‘liftIO’
-- ...
spec :: Spec
spec =
  describe "example test" do
    it "example test" do
      pure @IO ()

test1 :: LEff es Int
test1 = Linear.pure 1

test2 :: (WithResource :> es) => LEff es (Ur (Ur Text))
test2 = Linear.do
  handle <- R.unsafeFromRIO (RIO.openFile "" RIO.ReadMode)
  (Ur line, handle) <- R.unsafeFromRIO (RIO.hGetLine handle)
  R.unsafeFromRIO (RIO.hClose handle)
  Linear.pure (Ur (Ur line))

-- NOTE: `runResource` is a static effect with side effects,
-- but it requires `WithIO` instead of `IOE`,
-- so it's safe to call `runResource` in the middle of a program,
-- without enabling arbitrary IO everywhere.
test3 :: (WithIO :> es, State Int :> es) => Eff es Text
test3 = do
  modify @Int (+ 1)
  line <- runLEff Linear.do
    R.runResource Linear.do
      liftEff (modify @Int (+ 1))
      test2
  modify @Int (+ 1)
  pure line
