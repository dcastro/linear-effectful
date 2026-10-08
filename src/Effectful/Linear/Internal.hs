{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_HADDOCK not-home #-}

module Effectful.Linear.Internal where

import Control.Functor.Linear qualified as Control
import Control.Monad.IO.Class.Linear qualified as Linear
import Control.Monad.Trans.Reader (ReaderT (..))
import Data.Functor.Linear qualified as Data
import Data.Unrestricted.Linear (Ur)
import Effectful
import Effectful.Dispatch.Static qualified as Static
import Effectful.Dispatch.Static.Primitive (Env)
import Effectful.Linear.Utils.RedundantConstraint (redundantConstraint)
import System.IO.Linear qualified as Linear

-- | A linear "Eff" monad.
newtype LEff (es :: [Effect]) a = LEff (Env es -> Linear.IO a)
  deriving
    ( Data.Functor,
      Control.Functor,
      Data.Applicative,
      Control.Applicative,
      Control.Monad
    )
    via (ReaderT (Env es) Linear.IO)

-- We don't want `LEff es1 a` to be coercible to `LEff es2 a`.
type role LEff nominal representational

instance (IOE :> es) => Linear.MonadIO (LEff es) where
  liftIO =
    redundantConstraint @(IOE :> es) Linear.do
      unsafeLEff_

unLEff :: LEff es a %1 -> (Env es -> Linear.IO a)
unLEff (LEff f) = f

runLEff :: LEff es (Ur a) -> Eff es a
runLEff (LEff act) =
  Static.unsafeEff \env ->
    Linear.withLinearIO Linear.do
      act env

-- | This function is unsafe because it can be used to introduce arbitrary "IO" actions into pure `Eff` computations.
unsafeLEff :: (Env es -> Linear.IO a) %1 -> LEff es a
unsafeLEff = LEff

-- | This function is unsafe because it can be used to introduce arbitrary "IO" actions into pure `Eff` computations.
unsafeLEff_ :: Linear.IO a %1 -> LEff es a
unsafeLEff_ io = LEff \_ -> io

liftEff :: forall es a. Eff es a -> LEff es a
liftEff eff =
  LEff \env -> Linear.liftSystemIO (Static.unEff eff env)

liftEffU :: forall es a. Eff es a -> LEff es (Ur a)
liftEffU eff =
  LEff \env -> Linear.liftSystemIOU (Static.unEff eff env)
