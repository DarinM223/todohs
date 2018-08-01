module Config where

import Environment
import Pool

newtype Port = Port { unPort :: Int }
    deriving (Show, Eq, Num, Read)

data Config = Config
    { _port :: Port
    , _env  :: Environment
    , _pool :: DBPool
    }

mkConfig :: IO Config
mkConfig = do
    port <- lookupSetting "PORT" 8081
    env <- lookupSetting "ENV" Development
    pool <- mkPool env
    return Config { _port = port
                  , _env  = env
                  , _pool = pool }

freeConfig :: Config -> IO ()
freeConfig = freePool . _pool
