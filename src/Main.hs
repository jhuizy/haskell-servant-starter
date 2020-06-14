{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE TypeFamilies               #-}

module Main where

import           Model
import           Data.Text
import           Network.Wai.Handler.Warp
import           Network.HTTP.Types
import           Database.Persist
import           Database.Persist.Sqlite
import           Control.Monad.IO.Class         ( MonadIO )

import           Control.Monad.Reader           ( MonadReader
                                                , asks
                                                , ReaderT
                                                , runReaderT
                                                )
import Servant
import           Data.Proxy                     ( Proxy )
import           Data.Data                      ( Proxy(Proxy) )
import           Data.Aeson                     ( FromJSON
                                                , ToJSON
                                                )
import           GHC.Generics                   ( Generic )
import           Servant.API                    ( ReqBody )
import           Control.Monad.Except           ( ExceptT
                                                , MonadError(throwError)
                                                )
import           Control.Monad.Trans            ( MonadIO(liftIO) )
import Control.Applicative (Alternative((<|>)))
import Servant.Server (err400)


checkLogin
  :: (MonadIO m, PersistQueryRead backend, BaseBackend backend ~ SqlBackend)
  => Text
  -> Text
  -> ReaderT backend m Bool
checkLogin username pass = do
  mEntity <- selectFirst [UserName ==. username] [LimitTo 1]
  return $ case mEntity of
    Just (Entity _ user) -> userPassword user == pass
    Nothing              -> False

data LoginRequest = LoginRequest
  { lrUserName :: Text
  , lrPassword :: Text
  } deriving (Show, Generic)

instance FromJSON LoginRequest

data Token = Token
  { tokenName :: Text
  , tokenExpiry :: Text
  } deriving (Show, Generic)

instance ToJSON Token

type LoginAPI = "login" :> ReqBody '[JSON] LoginRequest :> Post '[JSON] Token
  :<|> "register" :> ReqBody '[JSON] LoginRequest :> Post '[JSON] Token
type API = LoginAPI

data Config = Config
  { configConnection :: ConnectionPool
  , configPort :: Int
  }

newtype AppT m a = AppT
  { runAppT :: ReaderT Config (ExceptT ServerError m) a
  } deriving (Functor, Applicative, Monad, MonadReader Config, MonadIO, MonadError ServerError)

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
    Nothing -> throwError $ err400 { errBody = "Account already exists" }

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
