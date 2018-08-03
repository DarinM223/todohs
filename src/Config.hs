module Config where

import Environment
import Pool
import Servant (Context (EmptyContext, (:.)))
import Servant.Auth.Server
    ( CookieSettings
    , JWTSettings
    , defaultCookieSettings
    , defaultJWTSettings
    , generateKey
    )

newtype Port = Port { unPort :: Int }
    deriving (Show, Eq, Num, Read)

data Config = Config
    { _port    :: Port
    , _env     :: Environment
    , _pool    :: DBPool
    , _jwt     :: JWTSettings
    , _cookie  :: CookieSettings
    , _context :: Context [CookieSettings, JWTSettings]
    }

mkConfig :: IO Config
mkConfig = do
    port <- lookupSetting "PORT" 8081
    env <- lookupSetting "ENV" Development
    pool <- mkPool env
    key <- generateKey
    let jwtSettings    = defaultJWTSettings key
        cookieSettings = defaultCookieSettings
        context        = cookieSettings :. jwtSettings :. EmptyContext
    return Config { _port    = port
                  , _env     = env
                  , _pool    = pool
                  , _jwt     = jwtSettings
                  , _cookie  = cookieSettings
                  , _context = context }

freeConfig :: Config -> IO ()
freeConfig = freePool . _pool
