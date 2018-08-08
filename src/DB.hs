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

import qualified Database.PostgreSQL.Simple as P
import qualified Database.SQLite.Simple as S

class (Monad m) => MonadUsers m where
    lookupByEmail :: Text -> m (Maybe User)

class (Monad m) => MonadTodoLists m where
    getList :: Int -> m (Maybe TodoList)

lookupByEmailDB db email
    = runSelectReturningOne
    $ select
    $ filter_ (\user -> _userEmail user ==. val_ email)
    $ all_ (_todoListUsers db)

getListDB db id
    = runSelectReturningOne
    $ select
    $ filter_ (\list -> _todoListId list ==. val_ id)
    $ all_ (_todoLists db)

newtype MonadDBPostgres m a = MonadDBPostgres (m a)
    deriving (Functor, Applicative, Monad, MonadIO, MonadReader r)

type PgConstr c m = (MonadIO m, MonadReader c m, HasPool P.Connection c)

instance (PgConstr c m) => MonadUsers (MonadDBPostgres m) where
    lookupByEmail id = asks getPool >>=
        liftIO . flip runPool (lookupByEmailDB pgDb id :: Pg (Maybe User))
instance (PgConstr c m) => MonadTodoLists (MonadDBPostgres m) where
    getList id = asks getPool >>=
        liftIO . flip runPool (getListDB pgDb id :: Pg (Maybe TodoList))

newtype MonadDBSqlite m a = MonadDBSqlite (m a)
    deriving (Functor, Applicative, Monad, MonadIO, MonadReader r)

type SqConstr c m = (MonadIO m, MonadReader c m, HasPool S.Connection c)

instance (SqConstr c m) => MonadUsers (MonadDBSqlite m) where
    lookupByEmail id = asks getPool >>=
        liftIO . flip runPool (lookupByEmailDB sqDb id :: SqliteM (Maybe User))
instance (SqConstr c m) => MonadTodoLists (MonadDBSqlite m) where
    getList id = asks getPool >>=
        liftIO . flip runPool (getListDB sqDb id :: SqliteM (Maybe TodoList))
