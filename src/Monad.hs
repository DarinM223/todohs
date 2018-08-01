module Monad where

import Config
import Control.Monad.Reader

newtype AppT m a = AppT { unAppT :: ReaderT Config m a }
    deriving (Functor, Applicative, Monad, MonadIO, MonadReader Config)

instance MonadTrans AppT where
    lift = AppT . lift

runAppT :: AppT m a -> Config -> m a
runAppT = runReaderT . unAppT
