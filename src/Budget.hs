{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

module Budget where

import App
import Control.Applicative (Alternative ((<|>)))
import Control.Monad.Except
  ( ExceptT,
    MonadError (throwError),
  )
import Control.Monad.IO.Class (MonadIO)
import Control.Monad.Reader
  ( MonadReader,
    ReaderT,
    asks,
    runReaderT,
  )
import Control.Monad.Trans (MonadIO (liftIO))
import Data.Aeson
  ( FromJSON,
    ToJSON,
  )
import Data.Data (Proxy (Proxy))
import Data.Proxy (Proxy)
import Data.Text
import Database.Persist
import Database.Persist.Sqlite
import GHC.Generics (Generic)
import Model
import Network.HTTP.Types
import Network.Wai.Handler.Warp
import Servant
import Servant.API (ReqBody)
import Servant.Server (err400)
import User

type API = LoginAPI

serverAPI :: Proxy API
serverAPI = Proxy

server :: MonadIO m => ServerT API (AppT m)
server = userServer

mkServer :: Config -> Server API
mkServer config = hoistServer serverAPI f server
  where
    f :: AppT IO a -> Handler a
    f app = Handler $ runReaderT (runAppT @IO app) config

mkApplication :: Config -> Application
mkApplication config = serve serverAPI (mkServer config)

main :: IO ()
main = run 8080 (mkApplication mkConfig)
