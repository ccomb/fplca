{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}

{- | The 'AppM' capability monad and the read-only environment that every
HTTP handler closes over.

The categorical structure: 'AppM' is a 'Reader' monad transformer over
Servant's 'Handler', so it lives in the Kleisli category of @Reader
AppEnv@ lifted into @Handler@'s Kleisli category. 'hoistServer' (used
in 'API.Routes.lcaServer') is the natural transformation
@forall a. AppM a -> Handler a@ that turns a @ServerT api AppM@ into a
@ServerT api Handler@ — Servant doesn't care about the monad, as long
as we can collapse it down to 'Handler' at the boundary.

The 'Has*' typeclasses are narrow capability witnesses: a function with
@(MonadReader r m, HasDatabaseManager r, MonadIO m) => m a@ declares
exactly the slice of the environment it needs, without committing to
the concrete monad. This is the "Has-pattern" of mtl-style: capability
classes are *projections* out of the environment object.
-}
module App.Env (
    -- * Environment
    AppEnv (..),
    mkAppEnv,

    -- * Monad
    AppM (..),
    runApp,

    -- * Capability classes (Has-pattern)
    HasDatabaseManager (..),
    HasMaxTreeDepth (..),
    HasPassword (..),
    HasHostingConfig (..),
    HasClassificationPresets (..),
) where

import Control.Monad.Except (MonadError)
import Control.Monad.IO.Class (MonadIO)
import Control.Monad.Reader (MonadReader, ReaderT (..))
import qualified Config
import Database.Manager (DatabaseManager)
import Servant (Handler, ServerError)

-- | Read-only application environment threaded through every request.
data AppEnv = AppEnv
    { aeDbManager :: !DatabaseManager
    , aeMaxTreeDepth :: !Int
    , aePassword :: !(Maybe String)
    , aeHostingConfig :: !(Maybe Config.HostingConfig)
    , aeClassificationPresets :: ![Config.ClassificationPreset]
    }

-- | Smart constructor — keeps callers from positionally swapping fields.
mkAppEnv
    :: DatabaseManager
    -> Int
    -> Maybe String
    -> Maybe Config.HostingConfig
    -> [Config.ClassificationPreset]
    -> AppEnv
mkAppEnv = AppEnv

{- | Servant 'Handler' threaded with a read-only 'AppEnv'. Deriving the
@MonadReader@ / @MonadIO@ / @MonadError ServerError@ instances via
@GeneralizedNewtypeDeriving@ keeps the wrapper free at runtime: 'AppM'
is representationally a function @AppEnv -> IO (Either ServerError a)@
under the hood, identical to 'Handler' modulo the @AppEnv@ argument.
-}
newtype AppM a = AppM {unAppM :: ReaderT AppEnv Handler a}
    deriving newtype (Functor, Applicative, Monad, MonadIO, MonadReader AppEnv, MonadError ServerError)

{- | Discharge an 'AppM' computation against a concrete environment,
producing a plain Servant 'Handler'. Use in 'API.Routes.lcaServer' as
@hoistServer lcaAPI (runApp env) handlers@.

This is the natural transformation @AppM ~> Handler@ that Servant's
@hoistServer@ requires; it lifts the entire @ServerT api AppM@ into
@ServerT api Handler@ point-free.
-}
runApp :: AppEnv -> AppM a -> Handler a
runApp env (AppM m) = runReaderT m env

-- ---------------------------------------------------------------------------
-- Has-pattern: narrow capability witnesses
-- ---------------------------------------------------------------------------

{- | Witness that the environment exposes a 'DatabaseManager'. Handlers
that need DB access should constrain on @HasDatabaseManager r@ rather
than the concrete 'AppEnv', so the same code can run in tests with a
narrower env.
-}
class HasDatabaseManager r where
    getDatabaseManager :: r -> DatabaseManager

instance HasDatabaseManager AppEnv where
    getDatabaseManager = aeDbManager

-- | Max tree depth limit (anti-DoS guard for /tree and /graph endpoints).
class HasMaxTreeDepth r where
    getMaxTreeDepth :: r -> Int

instance HasMaxTreeDepth AppEnv where
    getMaxTreeDepth = aeMaxTreeDepth

-- | Optional admin password gating @POST /auth@.
class HasPassword r where
    getPassword :: r -> Maybe String

instance HasPassword AppEnv where
    getPassword = aePassword

-- | Hosting configuration consumed by @GET /hosting@.
class HasHostingConfig r where
    getHostingConfig :: r -> Maybe Config.HostingConfig

instance HasHostingConfig AppEnv where
    getHostingConfig = aeHostingConfig

-- | Classification presets used in @/aggregate@ and @/supply-chain@ filters.
class HasClassificationPresets r where
    getClassificationPresets :: r -> [Config.ClassificationPreset]

instance HasClassificationPresets AppEnv where
    getClassificationPresets = aeClassificationPresets
