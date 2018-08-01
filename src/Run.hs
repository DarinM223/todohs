module Run where

import Config
import Control.Exception

runApp :: IO ()
runApp = bracket mkConfig freeConfig runApp
  where
    runApp = undefined
