{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

{- | 'AppM' is @'ReaderT' 'AppEnv' 'Handler'@; 'runApp' is the @AppM ~> Handler@
mapping passed to Servant's 'hoistServer'.
-}
module App.Env (
    AppEnv (..),
    AppM (..),
    runApp,
) where

import qualified Config
import Control.Monad.Catch (MonadCatch, MonadMask, MonadThrow)
import Control.Monad.Except (MonadError)
import Control.Monad.IO.Class (MonadIO)
import Control.Monad.Reader (MonadReader, ReaderT (..))
import Database.Manager (DatabaseManager)
import Servant (Handler, ServerError)

-- | Read-only application environment threaded through every request.
data AppEnv = AppEnv
    { aeDbManager :: !DatabaseManager
    , aeMaxTreeDepth :: !Int
    , aePassword :: !(Maybe String)
    , aeHostingConfig :: !(Maybe Config.HostingConfig)
    , aeClassificationPresets :: ![Config.ClassificationPreset]
    , aeDataVersion :: !(Maybe Config.DataVersion)
    }

newtype AppM a = AppM {unAppM :: ReaderT AppEnv Handler a}
    deriving newtype (Functor, Applicative, Monad, MonadIO, MonadReader AppEnv, MonadError ServerError, MonadThrow, MonadCatch, MonadMask)

runApp :: AppEnv -> AppM a -> Handler a
runApp env (AppM m) = runReaderT m env
