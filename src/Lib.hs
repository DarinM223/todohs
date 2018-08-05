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
            bracket (mkConfig env (mkPoolPg env)) freeConfig (runApp runProdT)
        _ ->
            bracket (mkConfig env (mkPoolSq env)) freeConfig (runApp runDevT)
  where
    app :: RunFn conn m -> Config conn -> Application
    app run config =
        serveWithContext cookieApi (_context config) (server run config)

    runApp :: RunFn conn m -> Config conn -> IO ()
    runApp fn config = void $ Warp.run (unPort $ _port config) (app fn config)
