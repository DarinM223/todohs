module Environment where

import System.Environment (lookupEnv)
import Text.Read (readMaybe)

data Environment = Development | Test | Production
    deriving (Show, Eq, Read)

lookupSetting :: (Read a) => String -> a -> IO a
lookupSetting key init = lookupEnv key >>= \case
    Just str -> case readMaybe str of
        Just v -> pure v
        _      -> error $ "Invalid setting for variable: " ++ key
    Nothing -> pure init
