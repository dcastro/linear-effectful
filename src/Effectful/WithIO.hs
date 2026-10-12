module Effectful.WithIO
  ( WithIO,
    MaybeWithIO,
    runWithIO,
  )
where

import Data.Kind (Constraint)
import Effectful
import Effectful.Dispatch.Static (HasCallStack, SideEffects (..), StaticRep)
import Effectful.Dispatch.Static qualified as Static

-- $setup
-- >>> import Control.Functor.Linear qualified as Linear
-- >>> import Data.Unrestricted.Linear (Ur (..))
-- >>> import Effectful
-- >>> import Effectful.Linear
-- >>> import Effectful.Linear.Resource (WithResource)
-- >>> import Effectful.Linear.Resource qualified as R
-- >>> import Effectful.Dispatch.Static
-- >>> import System.IO.Resource.Linear qualified as RIO
-- >>> import Effectful.State.Static.Local (State, modify)
-- >>> import Data.Text (Text)

-- | This effect is similar to 'IOE', except it does not
-- give rise to 'MonadIO' or 'MonadUnliftIO' instances when in scope.
--
-- It simply signals that _some_ effects in the stack have side effects,
-- thus forbidding the use of 'runPureEff'.
--
-- This allows us to handle a static effect with side effects (e.g. 'Effectful.Linear.Resource.runResource')
-- in the middle of a program rather than at the edge (e.g. in @main@)
-- without having to enable arbitrary arbitrary IO everywhere.
--
-- E.g. in the following example, we can handle the 'Effectful.Linear.Resource.WithResource'
-- event from within @dumpLog@,
-- without infecting it (or its callers) with 'IOE'.
--
-- >>> :{
-- >>> data LinearFileSystem :: Effect
-- >>> type instance DispatchOf LinearFileSystem = Static WithSideEffects
-- >>> data instance StaticRep LinearFileSystem = MkLinearFileSystem
-- >>>
-- >>> runLinearFileSystem :: (HasCallStack, IOE :> es) => Eff (LinearFileSystem : es) a -> Eff es a
-- >>> runLinearFileSystem = evalStaticRep MkLinearFileSystem
-- >>>
-- >>> openFile  :: (LinearFileSystem :> es, WithResource :> es) => FilePath -> RIO.IOMode -> LEff es RIO.Handle
-- >>> hPutStrLn :: (LinearFileSystem :> es, WithResource :> es) => RIO.Handle %1 -> Text -> LEff es RIO.Handle
-- >>> hClose    :: (LinearFileSystem :> es, WithResource :> es) => RIO.Handle %1 -> LEff es ()
-- >>>
-- >>> openFile handle line = R.unsafeFromRIO (RIO.openFile handle line)
-- >>> hPutStrLn handle line = R.unsafeFromRIO (RIO.hPutStrLn handle line)
-- >>> hClose handle = R.unsafeFromRIO (RIO.hClose handle)
-- >>> :}
--
-- >>> :{
-- dumpLog :: (LinearFileSystem :> es, WithIO :> es) => FilePath -> [Text] -> Eff es ()
-- dumpLog fp logLines = do
--   runLEff Linear.do
--     R.runResource Linear.do
--       handle <- openFile fp RIO.AppendMode
--       handle <- go logLines handle
--       hClose handle
--       Linear.pure (Ur (Ur ()))
--   where
--     go :: (LinearFileSystem :> es, WithResource :> es) => [Text] -> RIO.Handle %1 -> LEff es RIO.Handle
--     go [] handle = Linear.pure handle
--     go (line : rest) handle = Linear.do
--       handle <- hPutStrLn handle line
--       go rest handle
-- :}
data WithIO :: Effect

type instance DispatchOf WithIO = 'Static 'WithSideEffects

data instance StaticRep WithIO = MkWithIO

type family MaybeWithIO (sideEffects :: SideEffects) (es :: [Effect]) :: Constraint where
  MaybeWithIO NoSideEffects _ = ()
  MaybeWithIO WithSideEffects es = WithIO :> es

runWithIO :: (HasCallStack, IOE :> es) => Eff (WithIO : es) a -> Eff es a
runWithIO = Static.evalStaticRep MkWithIO
