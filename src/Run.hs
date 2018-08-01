module Run where

import Config
import Control.Exception

import qualified Network.Wai.Handler.Warp as Warp

runApp :: IO ()
runApp = bracket mkConfig freeConfig runApp
  where
    app = undefined
    runApp = flip Warp.run app . unPort . _port
