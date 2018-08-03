module Lib where

import Config
import Control.Exception
import Control.Monad (void)
import Servant (serveWithContext)
import Server

import qualified Network.Wai.Handler.Warp as Warp

runApp :: IO ()
runApp = bracket mkConfig freeConfig runApp
  where
    app config = serveWithContext cookieApi (_context config) (server config)
    runApp config = void $ Warp.run (unPort $ _port config) (app config)
