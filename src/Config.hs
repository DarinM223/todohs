module Config where

import Data.Pool
import Environment
import Servant (Context (EmptyContext, (:.)), Handler)
import Servant.Auth.Server
    ( CookieSettings
    , JWTSettings
    , defaultCookieSettings
    , defaultJWTSettings
    , generateKey
    )

newtype Port = Port { unPort :: Int }
    deriving (Show, Eq, Num, Read)

type RunFn conn m = (forall a. m a -> Config conn -> Handler a)

data Config conn = Config
    { _port    :: Port
    , _env     :: Environment
    , _pool    :: Pool conn
    , _jwt     :: JWTSettings
    , _cookie  :: CookieSettings
    , _context :: Context [CookieSettings, JWTSettings]
    }

mkConfig :: Environment -> IO (Pool conn) -> IO (Config conn)
mkConfig env mkPool = do
    pool <- mkPool
    port <- lookupSetting "PORT" 8081
    key <- generateKey
    let jwtSettings    = defaultJWTSettings key
        cookieSettings = defaultCookieSettings
        context        = cookieSettings :. jwtSettings :. EmptyContext
    return Config { _port    = port
                  , _env     = env
                  , _pool    = pool
                  , _jwt     = jwtSettings
                  , _cookie  = cookieSettings
                  , _context = context
                  }

freeConfig :: Config conn -> IO ()
freeConfig = destroyAllResources . _pool
