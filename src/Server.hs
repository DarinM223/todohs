module Server where

import Config (Config (_cookie, _jwt, _runHandler))
import Control.Monad.Except
import Control.Monad.Trans.Maybe
import Crypto.BCrypt
import Data.Aeson (FromJSON)
import Data.Text (Text)
import Data.Text.Encoding (encodeUtf8)
import GHC.Generics
import Models (TodoList, User, UserT (_userPassword))
import Monad (Constr)
import Servant
import Servant.Auth.Server

import qualified DB as DB

data Login = Login { _email :: Text, _password :: Text }
    deriving (Eq, Show, Read, Generic)
instance FromJSON Login

type Protected = "users" :> UsersAPI
            :<|> "lists" :> TodoListAPI

type UsersAPI = "get" :> Capture "email" Text :> Get '[JSON] (Maybe User)
type TodoListAPI = "get" :> Capture "id" Int :> Get '[JSON] (Maybe TodoList)

protected :: (Constr conn m)
          => Config conn m
          -> AuthResult User
          -> Server Protected
protected config (Authenticated _) =
    hoistServer (Proxy :: Proxy Protected) (_runHandler config) server
  where
    userServer = DB.lookupByEmail
    listServer = DB.getList
    server = userServer :<|> listServer
protected _ _ = throwAll err401

type Unprotected
    = "login" :> ReqBody '[JSON] Login
              :> PostNoContent '[JSON] (Headers '[ Header "Set-Cookie" SetCookie
                                                 , Header "Set-Cookie" SetCookie
                                                 ]
                                                 NoContent)
 :<|> Raw

unprotected :: (Constr conn m) => Config conn m -> Server Unprotected
unprotected config = checkCreds config :<|> serveDirectoryFileServer "/static"

type API auths = (Auth auths User :> Protected) :<|> Unprotected

checkCreds :: (Constr conn m)
           => Config conn m
           -> Login
           -> Handler (Headers '[ Header "Set-Cookie" SetCookie
                                , Header "Set-Cookie" SetCookie
                                ]
                                NoContent)
checkCreds cfg (Login email password) =
    maybe (throwError err401) return =<< checkCreds
  where
    lookupEmail cfg = _runHandler cfg . DB.lookupByEmail
    checkLogin cfg = liftIO . acceptLogin (_cookie cfg) (_jwt cfg)
    validateUser user password = validatePassword
        (encodeUtf8 $ _userPassword user)
        (encodeUtf8 password)
    checkCreds = runMaybeT $ do
        user <- lift (lookupEmail cfg email) >>= MaybeT . pure
        guard (validateUser user password)
        lift (checkLogin cfg user) >>= lift . \case
            Nothing           -> throwError err401
            Just applyCookies -> return $ applyCookies NoContent

jwtApi :: Proxy (API '[JWT])
jwtApi = Proxy

cookieApi :: Proxy (API '[Cookie])
cookieApi = Proxy

server :: (Constr conn m) => Config conn m -> Server (API auths)
server config = protected config :<|> unprotected config
