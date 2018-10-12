module Migrations where

import Data.Proxy (Proxy)
import Database.Beam
import Database.Beam.Migrate
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

migrations :: forall syntax be. (MigrateSyntax syntax)
           => MigrationSteps syntax
                             ()
                             (CheckedDatabaseSettings be TodoListDb)
migrations = migrationStep "Initial commit" (const migration)

checkedDb :: forall syntax be. (MigrateSyntax syntax)
          => Proxy syntax
          -> CheckedDatabaseSettings be TodoListDb
checkedDb _ = evaluateDatabase (migrations @syntax)

db :: forall syntax be. (MigrateSyntax syntax)
   => Proxy syntax
   -> DatabaseSettings be TodoListDb
db = unCheckDatabase . checkedDb
