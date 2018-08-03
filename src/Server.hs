module Server where

import Config (Config (_cookie, _jwt))
import Control.Monad.IO.Class
import Data.Aeson (FromJSON)
import Data.Text (Text)
import GHC.Generics
import Models (TodoList, User, UserT (User))
import Servant
import Servant.Auth.Server

data Login = Login { _username :: Text, _password :: Text }
    deriving (Eq, Show, Read, Generic)
instance FromJSON Login

type Protected = "users" :> UsersAPI
            :<|> "lists" :> TodoListAPI

type UsersAPI = "get" :> Capture "id" Int :> Get '[JSON] (Maybe User)
type TodoListAPI = "get" :> Capture "id" Int :> Get '[JSON] (Maybe TodoList)

protected :: Config -> AuthResult User -> Server Protected
protected _ (Authenticated _) = undefined
protected _ _                 = throwAll err401

type Unprotected
    = "login" :> ReqBody '[JSON] Login
              :> PostNoContent '[JSON] (Headers '[ Header "Set-Cookie" SetCookie
                                                 , Header "Set-Cookie" SetCookie
                                                 ]
                                                 NoContent)
 :<|> Raw

unprotected :: Config -> Server Unprotected
unprotected config = checkCreds config :<|> serveDirectoryFileServer "/static"

type API auths = (Auth auths User :> Protected) :<|> Unprotected

checkCreds :: Config
           -> Login
           -> Handler (Headers '[ Header "Set-Cookie" SetCookie
                                , Header "Set-Cookie" SetCookie
                                ]
                                NoContent)
checkCreds cfg (Login "Ochako Uraraka" "Deku") = do
    -- TODO(DarinM223): lookup in database instead
    let usr = User 0 "Ochako Uraraka" "uravity@gmail.com" :: User
    liftIO (acceptLogin (_cookie cfg) (_jwt cfg) usr) >>= \case
        Nothing           -> throwError err401
        Just applyCookies -> return $ applyCookies NoContent
checkCreds _ _ = throwError err401

jwtApi :: Proxy (API '[JWT])
jwtApi = Proxy

cookieApi :: Proxy (API '[Cookie])
cookieApi = Proxy

server :: Config -> Server (API auths)
server config = protected config :<|> unprotected config
