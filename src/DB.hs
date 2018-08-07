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

getUserDB db id
    = runSelectReturningOne
    $ select
    $ filter_ (\user -> _userId user ==. val_ id)
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
    getUser id = asks getPool >>=
        liftIO . flip runPool (getUserDB pgDb id :: Pg (Maybe User))
instance (PgConstr c m) => MonadTodoLists (MonadDBPostgres m) where
    getList id = asks getPool >>=
        liftIO . flip runPool (getListDB pgDb id :: Pg (Maybe TodoList))

newtype MonadDBSqlite m a = MonadDBSqlite (m a)
    deriving (Functor, Applicative, Monad, MonadIO, MonadReader r)

type SqConstr c m = (MonadIO m, MonadReader c m, HasPool S.Connection c)

instance (SqConstr c m) => MonadUsers (MonadDBSqlite m) where
    getUser id = asks getPool >>=
        liftIO . flip runPool (getUserDB sqDb id :: SqliteM (Maybe User))
instance (SqConstr c m) => MonadTodoLists (MonadDBSqlite m) where
    getList id = asks getPool >>=
        liftIO . flip runPool (getListDB sqDb id :: SqliteM (Maybe TodoList))
