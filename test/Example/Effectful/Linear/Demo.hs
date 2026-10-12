module Example.Effectful.Linear.Demo where

import Control.Functor.Linear qualified as Linear
import Data.Text (Text)
import Data.Unrestricted.Linear (Ur (..))
import Effectful
import Effectful.Dispatch.Static (HasCallStack, SideEffects (..), StaticRep, evalStaticRep)
import Effectful.Linear
import Effectful.Linear.Resource (WithResource)
import Effectful.Linear.Resource qualified as R
import Effectful.WithIO (WithIO)
import System.IO.Resource.Linear qualified as RIO
import Test.RedundantConstraint

----------------------------------------------------------------------------
-- Demo: linear filesystem effect
----------------------------------------------------------------------------

data LinearFileSystem :: Effect

type instance DispatchOf LinearFileSystem = Static WithSideEffects

data instance StaticRep LinearFileSystem = MkLinearFileSystem

runLinearFileSystem :: (HasCallStack, IOE :> es) => Eff (LinearFileSystem : es) a -> Eff es a
runLinearFileSystem = evalStaticRep MkLinearFileSystem

openFile ::
  forall es.
  (LinearFileSystem :> es, WithResource :> es) =>
  FilePath -> RIO.IOMode -> LEff es RIO.Handle
openFile handle line =
  redundantConstraint @(LinearFileSystem :> es) Linear.do
    R.unsafeFromRIO (RIO.openFile handle line)

hPutStrLn ::
  forall es.
  (LinearFileSystem :> es, WithResource :> es) =>
  RIO.Handle %1 -> Text -> LEff es RIO.Handle
hPutStrLn handle line =
  redundantConstraint @(LinearFileSystem :> es) Linear.do
    R.unsafeFromRIO (RIO.hPutStrLn handle line)

hClose ::
  forall es.
  (LinearFileSystem :> es, WithResource :> es) =>
  RIO.Handle %1 -> LEff es ()
hClose handle =
  redundantConstraint @(LinearFileSystem :> es) Linear.do
    R.unsafeFromRIO (RIO.hClose handle)

dumpLog :: (LinearFileSystem :> es, WithIO :> es) => FilePath -> [Text] -> Eff es ()
dumpLog fp logLines = do
  runLEff Linear.do
    R.runResource Linear.do
      handle <- openFile fp RIO.AppendMode
      handle <- go logLines handle
      hClose handle
      Linear.pure (Ur (Ur ()))
  where
    go :: (LinearFileSystem :> es, WithResource :> es) => [Text] -> RIO.Handle %1 -> LEff es RIO.Handle
    go [] handle = Linear.pure handle
    go (line : rest) handle = Linear.do
      handle <- hPutStrLn handle line
      go rest handle
