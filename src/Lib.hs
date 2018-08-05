module Lib where

import Config
import Control.Exception (bracket)
import Control.Monad (void)
import Environment
import Monad
import Pool
import Servant (serveWithContext, Application)
import Server

import qualified Network.Wai.Handler.Warp as Warp

runApp :: IO ()
runApp = do
    env <- lookupSetting "ENV" Development
    case env of
        Production ->
            bracket (mkConfig env (mkPoolPg env) runProdT) freeConfig runApp
        _ ->
            bracket (mkConfig env (mkPoolSq env) runDevT) freeConfig runApp
  where
    app :: Config conn m -> Application
    app config =
        serveWithContext cookieApi (_context config) (server config)

    runApp :: Config conn m -> IO ()
    runApp config = void $ Warp.run (unPort $ _port config) (app config)
