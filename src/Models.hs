module Models where

import Data.Text (Text)
import Database.Beam

data UserT f = User
    { _userId       :: Columnar f Int
    , _userEmail    :: Columnar f Text
    , _userPassword :: Columnar f Text
    } deriving Generic
type User = UserT Identity
deriving instance Show User

instance Table UserT where
    data PrimaryKey UserT f = UserId (Columnar f Int)
        deriving Generic
    primaryKey = UserId . _userId

type UserId = PrimaryKey UserT Identity
deriving instance Show UserId

instance Beamable UserT
instance Beamable (PrimaryKey UserT)

data TodoListT f = TodoList
    { _todoListId   :: Columnar f Int
    , _todoListName :: Columnar f Text
    , _todoListUser :: PrimaryKey UserT f
    } deriving Generic
type TodoList = TodoListT Identity
deriving instance Show TodoList

instance Table TodoListT where
    data PrimaryKey TodoListT f = TodoListId (Columnar f Int)
        deriving Generic
    primaryKey = TodoListId . _todoListId

type TodoListId = PrimaryKey TodoListT Identity
deriving instance Show TodoListId

instance Beamable TodoListT
instance Beamable (PrimaryKey TodoListT)

data TodoT f = Todo
    { _todoId         :: Columnar f Int
    , _todoText       :: Columnar f Text
    , _todoParentList :: PrimaryKey TodoListT f
    } deriving Generic
type Todo = TodoT Identity
deriving instance Show Todo

instance Table TodoT where
    data PrimaryKey TodoT f = TodoId (Columnar f Int)
        deriving Generic
    primaryKey = TodoId . _todoId

type TodoId = PrimaryKey TodoT Identity
deriving instance Show TodoId

instance Beamable TodoT
instance Beamable (PrimaryKey TodoT)

data TodoListDb f = TodoListDb
    { _todoListUsers :: f (TableEntity UserT)
    , _todoLists     :: f (TableEntity TodoListT)
    , _todos         :: f (TableEntity TodoT)
    } deriving Generic

instance Database be TodoListDb

todoListDb :: DatabaseSettings be TodoListDb
todoListDb = defaultDbSettings `withDbModification` dbModification
    { _todoListUsers = modifyTable (const "users") tableModification
    , _todoLists     = modifyTable (const "lists") tableModification
    , _todos         = modifyTable (const "todos") tableModification
    }
