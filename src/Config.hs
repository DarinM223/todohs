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

type RunFn conn m = forall a. m a -> Config conn m -> Handler a
type RunHandler m = forall a. m a -> Handler a

class HasPool conn c | c -> conn where
    getPool :: c -> Pool conn

data Config conn m = Config
    { _port       :: Port
    , _env        :: Environment
    , _pool       :: Pool conn
    , _jwt        :: JWTSettings
    , _cookie     :: CookieSettings
    , _context    :: Context [CookieSettings, JWTSettings]
    , _runHandler :: RunHandler m
    }

instance HasPool conn (Config conn m) where
    getPool = _pool

mkConfig :: Environment -> IO (Pool conn) -> RunFn conn m -> IO (Config conn m)
mkConfig env mkPool fn = do
    pool <- mkPool
    port <- lookupSetting "PORT" 8081
    key <- generateKey
    let jwtSettings    = defaultJWTSettings key
        cookieSettings = defaultCookieSettings
        context        = cookieSettings :. jwtSettings :. EmptyContext
    let config = Config { _port       = port
                        , _env        = env
                        , _pool       = pool
                        , _jwt        = jwtSettings
                        , _cookie     = cookieSettings
                        , _context    = context
                        , _runHandler = flip fn config
                        }
    return config

freeConfig :: Config conn m -> IO ()
freeConfig = destroyAllResources . _pool
