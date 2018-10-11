{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UndecidableInstances #-}

module Monad where

import Config
import Control.Monad.Reader
import Data.Deriving.Via
import DB

import qualified Database.PostgreSQL.Simple as P
import qualified Database.SQLite.Simple as S

newtype ProdT conn m a = ProdT
    { unProdT :: ReaderT (Config conn (ProdT conn m)) m a }
    deriving ( Functor, Applicative, Monad, MonadIO
             , MonadReader (Config conn (ProdT conn m))
             )
$(deriveVia [t| forall m. (MonadIO m) =>
  MonadUsers (ProdT P.Connection m) `Via` MonadDBPostgres (ProdT P.Connection m) |])
$(deriveVia [t| forall m. (MonadIO m) =>
  MonadTodoLists (ProdT P.Connection m) `Via` MonadDBPostgres (ProdT P.Connection m) |])

runProdT :: ProdT conn m a -> Config conn (ProdT conn m) -> m a
runProdT = runReaderT . unProdT

newtype DevT conn m a = DevT
    { unDevT :: ReaderT (Config conn (DevT conn m)) m a }
    deriving ( Functor, Applicative, Monad, MonadIO
             , MonadReader (Config conn (DevT conn m))
             )
$(deriveVia [t| forall m. (MonadIO m) =>
  MonadUsers (DevT S.Connection m) `Via` MonadDBSqlite (DevT S.Connection m) |])
$(deriveVia [t| forall m. (MonadIO m) =>
  MonadTodoLists (DevT S.Connection m) `Via` MonadDBSqlite (DevT S.Connection m) |])

runDevT :: DevT conn m a -> Config conn (DevT conn m) -> m a
runDevT = runReaderT . unDevT

-- | Shared typeclass constraints for ProdT and DevT
type Constr conn m =
    ( MonadIO m
    , MonadReader (Config conn m) m
    , MonadUsers m
    , MonadTodoLists m
    )
