{-# LANGUAGE UndecidableInstances #-}

module DB where

import Config (HasPool (..))
import Control.Monad.Reader
import Database.Beam
import Database.Beam.Postgres (Pg)
import Database.Beam.Sqlite (SqliteM)
import Models
import Migrations
import Pool

import qualified Database.PostgreSQL.Simple as P
import qualified Database.SQLite.Simple as S

class (Monad m) => MonadUsers m where
    getUser :: Int -> m (Maybe User)

class (Monad m) => MonadTodoLists m where
    getList :: Int -> m (Maybe TodoList)

getUserDB id
    = runSelectReturningOne
    $ select
    $ filter_ (\user -> _userId user ==. val_ id)
    $ all_ (_todoListUsers todoListDb)

getListDB id
    = runSelectReturningOne
    $ select
    $ filter_ (\list -> _todoListId list ==. val_ id)
    $ all_ (_todoLists todoListDb)

newtype MonadDBPostgres m a = MonadDBPostgres (m a)
    deriving (Functor, Applicative, Monad, MonadIO, MonadReader r)

instance (MonadIO m, MonadReader c m, HasPool P.Connection c)
    => MonadUsers (MonadDBPostgres m) where
    getUser id = asks getPool >>=
        liftIO . flip runPool (getUserDB id :: Pg (Maybe User))

instance (MonadIO m, MonadReader c m, HasPool P.Connection c)
    => MonadTodoLists (MonadDBPostgres m) where
    getList id = asks getPool >>=
        liftIO . flip runPool (getListDB id :: Pg (Maybe TodoList))

newtype MonadDBSqlite m a = MonadDBSqlite (m a)
    deriving (Functor, Applicative, Monad, MonadIO, MonadReader r)

instance (MonadIO m, MonadReader c m, HasPool S.Connection c)
    => MonadUsers (MonadDBSqlite m) where
    getUser id = asks getPool >>=
        liftIO . flip runPool (getUserDB id :: SqliteM (Maybe User))

instance (MonadIO m, MonadReader c m, HasPool S.Connection c)
    => MonadTodoLists (MonadDBSqlite m) where
    getList id = asks getPool >>=
        liftIO . flip runPool (getListDB id :: SqliteM (Maybe TodoList))
