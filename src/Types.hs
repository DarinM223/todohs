module Types where

import Data.Aeson (FromJSON, ToJSON)
import Data.Text (Text)
import GHC.Generics (Generic)
import Models (User, UserT (..))
import Servant.Auth.Server (FromJWT, ToJWT)

data Login = Login
    { _email    :: Text
    , _password :: Text
    } deriving (Eq, Show, Read, Generic)
instance FromJSON Login

data PublicUser = PublicUser
    { _id    :: Int
    , _email :: Text
    } deriving (Show, Eq, Generic)
instance FromJSON PublicUser
instance ToJSON PublicUser
instance FromJWT PublicUser
instance ToJWT PublicUser

fromUser :: User -> PublicUser
fromUser user = PublicUser
    { _id    = _userId user
    , _email = _userEmail user
    }
