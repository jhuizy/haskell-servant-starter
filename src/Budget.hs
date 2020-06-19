{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

module Budget where

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

checkLogin ::
  (MonadIO m, PersistQueryRead backend, BaseBackend backend ~ SqlBackend) =>
  Text ->
  Text ->
  ReaderT backend m Bool
checkLogin username pass = do
  mEntity <- selectFirst [UserName ==. username] [LimitTo 1]
  return $ case mEntity of
    Just (Entity _ user) -> userPassword user == pass
    Nothing -> False

data LoginRequest
  = LoginRequest
      { lrUserName :: Text,
        lrPassword :: Text
      }
  deriving (Show, Generic)

instance FromJSON LoginRequest

data Token
  = Token
      { tokenName :: Text,
        tokenExpiry :: Text
      }
  deriving (Show, Generic)

instance ToJSON Token

type LoginAPI =
  "login" :> ReqBody '[JSON] LoginRequest :> Post '[JSON] Token
    :<|> "register" :> ReqBody '[JSON] LoginRequest :> Post '[JSON] Token

type API = LoginAPI

data Config
  = Config
      { configConnection :: ConnectionPool,
        configPort :: Int
      }

newtype AppT m a
  = AppT
      { runAppT :: ReaderT Config (ExceptT ServerError m) a
      }
  deriving (Functor, Applicative, Monad, MonadReader Config, MonadIO, MonadError ServerError)

serverAPI :: Proxy API
serverAPI = Proxy

server :: MonadIO m => ServerT API (AppT m)
server = loginHandler :<|> registerHandler

mkServer :: Config -> Server API
mkServer config = hoistServer serverAPI f server
  where
    f :: AppT IO a -> Handler a
    f app = Handler $ runReaderT (runAppT @IO app) config

mkApplication :: Config -> Application
mkApplication config = serve serverAPI (mkServer config)

registerHandler :: MonadIO m => LoginRequest -> AppT m Token
registerHandler (LoginRequest user pass) = do
  conn <- asks configConnection
  mRecord <- liftIO $ flip runSqlPool conn $ insertUnique $ User user pass
  case mRecord of
    Just _ -> return $ Token "" ""
    Nothing -> throwError $ err400 {errBody = "Account already exists"}

loginHandler :: MonadIO m => LoginRequest -> AppT m Token
loginHandler (LoginRequest user pass) = do
  conn <- asks configConnection
  isOK <- liftIO $ runSqlPool (checkLogin user pass) conn
  if isOK then return (Token "" "") else throwError err403

mkConfig :: Config
mkConfig = Config conn port
  where
    conn = undefined
    port = 8080

main :: IO ()
main = run 8080 (mkApplication mkConfig)
