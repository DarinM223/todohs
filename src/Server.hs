module Server where

import Config (Config (_cookie, _jwt, _runHandler))
import Control.Monad.Except
import Control.Monad.Trans.Maybe
import Crypto.BCrypt
import Data.Text.Encoding (encodeUtf8)
import Models (TodoList, User, UserT (_userPassword))
import Monad (Constr)
import Servant
import Servant.Auth.Server
import Types (Login (..), PublicUser)

import qualified DB as DB

type Protected = "users" :> UsersAPI
            :<|> "lists" :> TodoListAPI

type Unprotected
    = "login" :> ReqBody '[JSON] Login
              :> PostNoContent '[JSON] (Headers '[ Header "Set-Cookie" SetCookie
                                                 , Header "Set-Cookie" SetCookie
                                                 ]
                                                 NoContent)
 :<|> Raw

type API auths = (Auth auths User :> Protected) :<|> Unprotected

type UsersAPI = "get" :> Capture "id" Int :> Get '[JSON] (Maybe PublicUser)
type TodoListAPI = "get" :> Capture "id" Int :> Get '[JSON] (Maybe TodoList)

protected :: (Constr conn m)
          => Config conn m
          -> AuthResult User
          -> Server Protected
protected config (Authenticated _) =
    hoistServer (Proxy :: Proxy Protected) (_runHandler config) server
  where
    usersApi = DB.lookupByID
    listsApi = DB.getList
    server = usersApi :<|> listsApi
protected _ _ = throwAll err401

unprotected :: (Constr conn m) => Config conn m -> Server Unprotected
unprotected config = checkCreds config :<|> serveDirectoryFileServer "/static"

checkCreds :: (Constr conn m)
           => Config conn m
           -> Login
           -> Handler (Headers '[ Header "Set-Cookie" SetCookie
                                , Header "Set-Cookie" SetCookie
                                ]
                                NoContent)
checkCreds cfg (Login email password) =
    checkCreds >>= maybe (throwError err401) return
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

server :: (Constr conn m) => Config conn m -> Server (API auths)
server config = protected config :<|> unprotected config

jwtApi :: Proxy (API '[JWT])
jwtApi = Proxy

cookieApi :: Proxy (API '[Cookie])
cookieApi = Proxy
