{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

module User where

import App
import Control.Monad.IO.Class (MonadIO)
import Control.Monad.IO.Class (MonadIO (liftIO))
import Control.Monad.Reader (ReaderT, asks)
import Data.Aeson (ToJSON)
import Data.Aeson.Types (FromJSON)
import Data.Text (Text)
import Database.Persist (PersistQueryRead)
import Database.Persist (PersistQueryRead (selectFirst))
import Database.Persist
import Database.Persist.Sql (BaseBackend, SqlBackend, runSqlPool)
import Database.Persist.Sql (Entity (Entity))
import GHC.Generics (Generic)
import Model
import Servant

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

userServer :: MonadIO m => ServerT LoginAPI (AppT m)
userServer = loginHandler :<|> registerHandler

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
