{-# LANGUAGE UndecidableInstances #-}

module DB where

import Config (HasPool (..))
import Control.Monad.Reader
import Data.Proxy (Proxy (Proxy))
import Data.Text (Text)
import Database.Beam
import Database.Beam.Postgres (Pg, PgCommandSyntax)
import Database.Beam.Sqlite (SqliteCommandSyntax, SqliteM)
import Models
import Migrations
import Pool
import Types (PublicUser, fromUser)

import qualified Database.PostgreSQL.Simple as P
import qualified Database.SQLite.Simple as S

class (Monad m) => MonadUsers m where
    type Syntax m     :: *
    type DBMonad m    :: * -> *
    type Connection m :: *

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

runInPool :: forall m c be r syntax
           . ( MonadIO m
             , MonadReader c m
             , MonadBeam syntax be (Connection m) (DBMonad m)
             , HasPool (Connection m) c
             , MigrateSyntax (Syntax m)
             )
          => (DatabaseSettings be TodoListDb -> (DBMonad m) (Maybe r))
          -> m (Maybe r)
runInPool m = do
    pool <- asks getPool
    let db' = db (Proxy :: Proxy (Syntax m))
    liftIO $ runPool pool (m db')

instance (PgConstr c m) => MonadUsers (MonadDBPostgres m) where
    type Syntax (MonadDBPostgres m)     = PgCommandSyntax
    type DBMonad (MonadDBPostgres m)    = Pg
    type Connection (MonadDBPostgres m) = P.Connection

    lookupByEmail = runInPool . flip lookupByEmail'
    lookupByID = fmap (fmap fromUser) . runInPool . flip lookupByID'

instance (SqConstr c m) => MonadUsers (MonadDBSqlite m) where
    type Syntax (MonadDBSqlite m)     = SqliteCommandSyntax
    type DBMonad (MonadDBSqlite m)    = SqliteM
    type Connection (MonadDBSqlite m) = S.Connection

    lookupByEmail = runInPool . flip lookupByEmail'
    lookupByID = fmap (fmap fromUser) . runInPool . flip lookupByID'

instance (PgConstr c m) => MonadTodoLists (MonadDBPostgres m) where
    getList = runInPool . flip getList'

instance (SqConstr c m) => MonadTodoLists (MonadDBSqlite m) where
    getList = runInPool . flip getList'

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
