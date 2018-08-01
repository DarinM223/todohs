module Pool where

import Data.Pool
import Environment

import qualified Database.PostgreSQL.Simple as P

type DBPool = Pool P.Connection

mkPool :: Environment -> IO DBPool
mkPool env = createPool (connect env) P.close (numPools env) 1 2
  where
    numPools _ = 10
    connect _ = P.connectPostgreSQL ""

freePool :: DBPool -> IO ()
freePool = destroyAllResources
