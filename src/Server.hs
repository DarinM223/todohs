module Server where

import Control.Monad.IO.Class
import Data.Text (Text)
import GHC.Generics
import Models (User, UserT (User))
import Servant
import Servant.Auth.Server

data Login = Login { _username :: Text, _password :: Text }
    deriving (Eq, Show, Read, Generic)

type Protected = "users" :> UsersAPI
            :<|> "lists" :> TodoListAPI

type UsersAPI = Int
type TodoListAPI = Int

type Unprotected
    = "login" :> ReqBody '[JSON] Login
              :> PostNoContent '[JSON] (Headers '[ Header "Set-Cookie" SetCookie
                                                 , Header "Set-Cookie" SetCookie
                                                 ]
                                                 NoContent)
 :<|> Raw

unprotected :: CookieSettings -> JWTSettings -> Server Unprotected
unprotected cs jwts = checkCreds cs jwts
                 :<|> serveDirectoryFileServer "/static"

type API auths = (Auth auths User :> Protected) :<|> Unprotected

checkCreds :: CookieSettings
           -> JWTSettings
           -> Login
           -> Handler (Headers '[ Header "Set-Cookie" SetCookie
                                , Header "Set-Cookie" SetCookie
                                ]
                                NoContent)
checkCreds cookieSettings jwtSettings (Login "Ochako Uraraka" "Deku") = do
    -- TODO(DarinM223): lookup in database instead
    let usr = User 0 "Ochako Uraraka" "uravity@gmail.com" :: User
    liftIO (acceptLogin cookieSettings jwtSettings usr) >>= \case
        Nothing           -> throwError err401
        Just applyCookies -> return $ applyCookies NoContent
checkCreds _ _ _ = throwError err401
