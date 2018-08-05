module Monad where

import Config
import Control.Monad.Reader

newtype ProdT conn m a = ProdT { unProdT :: ReaderT (Config conn) m a }
    deriving (Functor, Applicative, Monad, MonadIO, MonadReader (Config conn))

instance MonadTrans (ProdT conn) where
    lift = ProdT . lift

runProdT :: ProdT conn m a -> Config conn -> m a
runProdT = runReaderT . unProdT

newtype DevT conn m a = DevT { unDevT :: ReaderT (Config conn) m a }
    deriving (Functor, Applicative, Monad, MonadIO, MonadReader (Config conn))

instance MonadTrans (DevT conn) where
    lift = DevT . lift

runDevT :: DevT conn m a -> Config conn -> m a
runDevT = runReaderT . unDevT

-- | Shared typeclass constraints for ProdT and DevT
type Constr conn m = (MonadIO m, MonadReader (Config conn) m)
