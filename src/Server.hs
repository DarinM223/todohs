module Server where

import Config (Config (_cookie, _jwt), RunFn)
import Control.Monad.Except
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

protected :: Config conn -> RunFn conn m -> AuthResult User -> Server Protected
protected config run (Authenticated _) =
    hoistServer (Proxy :: Proxy Protected) (flip run config) server
  where
    server = undefined
protected _ _ _ = throwAll err401

type Unprotected
    = "login" :> ReqBody '[JSON] Login
              :> PostNoContent '[JSON] (Headers '[ Header "Set-Cookie" SetCookie
                                                 , Header "Set-Cookie" SetCookie
                                                 ]
                                                 NoContent)
 :<|> Raw

unprotected :: Config conn -> Server Unprotected
unprotected config = checkCreds config :<|> serveDirectoryFileServer "/static"

type API auths = (Auth auths User :> Protected) :<|> Unprotected

checkCreds :: Config conn
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

server :: RunFn conn m -> Config conn -> Server (API auths)
server run config = protected config run :<|> unprotected config
