module Migrations where

import Database.Beam
import Database.Beam.Migrate
import Database.Beam.Postgres
import Database.Beam.Sqlite
import Models

-- :(
type MigrateSyntax syntax =
    ( IsSql92DdlCommandSyntax syntax
    , Sql92SerializableConstraintDefinitionSyntax
      (Sql92ColumnSchemaColumnConstraintDefinitionSyntax
      (Sql92CreateTableColumnSchemaSyntax
      (Sql92DdlCommandCreateTableSyntax syntax)))
    , Sql92SerializableDataTypeSyntax
      (Sql92ColumnSchemaColumnTypeSyntax
      (Sql92CreateTableColumnSchemaSyntax
      (Sql92DdlCommandCreateTableSyntax syntax)))
    )

migration :: forall be syntax. MigrateSyntax syntax
          => Migration syntax (CheckedDatabaseSettings be TodoListDb)
migration = do
    users <- createTable "users"
           $ User (field "id" int notNull)
                  (field "email" (varchar Nothing) notNull)
                  (field "password" (varchar Nothing) notNull)
    lists <- createTable "lists"
           $ TodoList (field "id" int notNull)
                      (field "name" (varchar Nothing) notNull)
                      (UserId (field "user" int notNull))
    todos <- createTable "todos"
           $ Todo (field "id" int notNull)
                  (field "text" (varchar Nothing) notNull)
                  (TodoListId (field "parent_list" int notNull))
    return $ TodoListDb users lists todos

type Migration' syntax be db = Migration syntax (CheckedDatabaseSettings be db)

migrationsSq :: MigrationSteps SqliteCommandSyntax
                               ()
                               (CheckedDatabaseSettings be TodoListDb)
migrationsSq = migrationStep "Initial commit" (const migration)

migrationsPg :: MigrationSteps PgCommandSyntax
                               ()
                               (CheckedDatabaseSettings be TodoListDb)
migrationsPg = migrationStep "Initial commit" (const migration)

pgCheckedDb :: CheckedDatabaseSettings be TodoListDb
pgCheckedDb = evaluateDatabase migrationsPg

sqCheckedDb :: CheckedDatabaseSettings be TodoListDb
sqCheckedDb = evaluateDatabase migrationsSq

pgDb :: DatabaseSettings be TodoListDb
pgDb = unCheckDatabase pgCheckedDb

sqDb :: DatabaseSettings be TodoListDb
sqDb = unCheckDatabase sqCheckedDb
