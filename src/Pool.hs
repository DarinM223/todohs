module Pool where

import Data.Pool
import Database.Beam
import Environment

import qualified Database.PostgreSQL.Simple as P

type DBPool = Pool P.Connection

mkPool :: Environment -> IO DBPool
mkPool env = createPool (connect env) P.close (numPools env) 1 2
  where
    numPools _ = 10
    connect _ = P.connectPostgreSQL ""

runPool :: (MonadBeam syntax be handle m) => Pool handle -> m a -> IO a
runPool pool m = withResource pool $ \handle ->
    withDatabaseDebug putStrLn handle m

freePool :: DBPool -> IO ()
freePool = destroyAllResources
