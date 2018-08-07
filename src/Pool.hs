module Pool where

import Data.List (intercalate)
import Data.Maybe (catMaybes)
import Data.Pool
import Database.Beam
import Database.Beam.Migrate.Simple (runSimpleMigration, simpleMigration)
import Database.Beam.Postgres
import Database.Beam.Sqlite
import Environment
import Migrations

import qualified Data.ByteString.Char8 as C
import qualified Database.Beam.Sqlite.Migrate as SM
import qualified Database.Beam.Postgres.Migrate as PM
import qualified Database.PostgreSQL.Simple as P
import qualified Database.SQLite.Simple as S

runPool :: (MonadBeam syntax be handle m) => Pool handle -> m a -> IO a
runPool pool m = withResource pool $ \handle ->
    withDatabaseDebug putStrLn handle m

numPools :: Environment -> Int
numPools _ = 10

mkPoolPg :: Environment -> IO (Pool P.Connection)
mkPoolPg env = do
    pool <- createPool (connect env) P.close (numPools env) 1 2
    withResource pool $ \conn ->
        simpleMigration PM.migrationBackend conn pgCheckedDb >>=
            handleMigrations (runSimpleMigration @PgCommandSyntax
                                                 @Postgres @_ @Pg
                                                 conn)
    return pool
  where
    connect Test = P.connectPostgreSQL "host=localhost dbname=todohs_test"
    connect Development =
        P.connectPostgreSQL "host=localhost dbname=todohs_dev"
    connect Production = do
        host <- Just <$> lookupSetting "PGHOST" "localhost"
        port <- Just . show <$> lookupSetting "PGPORT" 5432
        user <- lookupEnv "PGUSER"
        password <- lookupEnv "PGPASSWORD"
        dbname <- lookupEnv "PGDATABASE"
        let params = ["host=", "port=", "user=", "password=", "dbname="]
            envs   = [host, port, user, password, dbname]
            conn   = C.pack
                   . intercalate " "
                   . catMaybes
                   . fmap toEnvString
                   . zip params
                   $ envs
        P.connectPostgreSQL conn
      where
        toEnvString (param, Just env) = Just $ param ++ env
        toEnvString _                 = Nothing

mkPoolSq :: Environment -> IO (Pool S.Connection)
mkPoolSq env = do
    pool <- createPool (S.open (filename env)) S.close (numPools env) 1 2
    withResource pool $ \conn ->
        simpleMigration SM.migrationBackend conn sqCheckedDb >>=
            handleMigrations (runSimpleMigration @SqliteCommandSyntax
                                                 @Sqlite @_ @SqliteM
                                                 conn)
    return pool
  where
    filename Test        = "test.db"
    filename Development = "dev.db"
    filename Production  = error "Sqlite should not be for production"

handleMigrations :: ([command] -> IO ()) -> Maybe [command] -> IO ()
handleMigrations fn = \case
    Nothing       -> fail "Something went wrong constructing migration"
    Just []       -> putStrLn "Already up to date"
    Just commands -> fn commands

freePool :: Pool a -> IO ()
freePool = destroyAllResources
