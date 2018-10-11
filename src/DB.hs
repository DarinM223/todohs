{-# LANGUAGE UndecidableInstances #-}

module DB where

import Config (HasPool (..))
import Control.Monad.Reader
import Data.Text (Text)
import Database.Beam
import Database.Beam.Postgres (Pg)
import Database.Beam.Sqlite (SqliteM)
import Models
import Migrations
import Pool
import Types (PublicUser, fromUser)

import qualified Database.PostgreSQL.Simple as P
import qualified Database.SQLite.Simple as S

class (Monad m) => MonadUsers m where
    -- | Looks up user by email.
    --
    -- Only to be used for authentication, since it returns a user with the
    -- hashed password.
    lookupByEmail :: Text -> m (Maybe User)

    -- | Looks up user by id.
    lookupByID :: Int -> m (Maybe PublicUser)

class (Monad m) => MonadTodoLists m where
    getList :: Int -> m (Maybe TodoList)

newtype MonadDBSqlite m a = MonadDBSqlite (m a)
    deriving (Functor, Applicative, Monad, MonadIO, MonadReader r)
newtype MonadDBPostgres m a = MonadDBPostgres (m a)
    deriving (Functor, Applicative, Monad, MonadIO, MonadReader r)

type SqConstr c m = (MonadIO m, MonadReader c m, HasPool S.Connection c)
type PgConstr c m = (MonadIO m, MonadReader c m, HasPool P.Connection c)

instance (PgConstr c m) => MonadUsers (MonadDBPostgres m) where
    lookupByEmail email
        = liftIO
        . flip runPool (lookupByEmail' pgDb email :: Pg (Maybe User))
      =<< asks getPool
    lookupByID id
        = fmap (fmap fromUser)
        . liftIO
        . flip runPool (lookupByID' pgDb id :: Pg (Maybe User))
      =<< asks getPool

instance (SqConstr c m) => MonadUsers (MonadDBSqlite m) where
    lookupByEmail email
        = liftIO
        . flip runPool (lookupByEmail' pgDb email :: SqliteM (Maybe User))
      =<< asks getPool
    lookupByID id
        = fmap (fmap fromUser)
        . liftIO
        . flip runPool (lookupByID' pgDb id :: SqliteM (Maybe User))
      =<< asks getPool

instance (PgConstr c m) => MonadTodoLists (MonadDBPostgres m) where
    getList id = asks getPool >>=
        liftIO . flip runPool (getList' pgDb id :: Pg (Maybe TodoList))

instance (SqConstr c m) => MonadTodoLists (MonadDBSqlite m) where
    getList id = asks getPool >>=
        liftIO . flip runPool (getList' sqDb id :: SqliteM (Maybe TodoList))

lookupByEmail' db email
    = runSelectReturningOne
    $ select
    $ filter_ (\user -> _userEmail user ==. val_ email)
    $ all_ (_todoListUsers db)

lookupByID' db id
    = runSelectReturningOne
    $ select
    $ filter_ (\user -> _userId user ==. val_ id)
    $ all_ (_todoListUsers db)

getList' db id
    = runSelectReturningOne
    $ select
    $ filter_ (\list -> _todoListId list ==. val_ id)
    $ all_ (_todoLists db)
