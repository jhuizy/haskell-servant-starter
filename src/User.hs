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
import Database.Persist (PersistEntity (Key))
import Database.Persist.Sql (BaseBackend, SqlBackend, runSqlPool)
import Database.Persist.Sql (Entity (Entity))
import Database.Persist.Sql (PersistEntity (Key))
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

class MonadIO m => MonadUserService m where
  getUser :: Text -> m (Maybe User)
  putUser :: Text -> Text -> m (Maybe Text)

  validatePassword :: Text -> Text -> m Bool
  validatePassword username pass = do
    mUser <- getUser username
    return $ case mUser of
      Just (User _ pass') -> pass == pass'
      _ -> False

instance MonadIO m => MonadUserService (AppT m) where
  getUser username = do
    mEntity <- runDB $ selectFirst [UserName ==. username] [LimitTo 1]
    return $ case mEntity of
      Just (Entity _ user) -> Just user
      Nothing -> Nothing

  putUser username password = do
    mRecord <- runDB $ insertUnique $ User username password
    return $ f <$> mRecord
    where
      f (UserKey key) = key

type LoginAPI =
  "login" :> ReqBody '[JSON] LoginRequest :> Post '[JSON] Token
    :<|> "register" :> ReqBody '[JSON] LoginRequest :> Post '[JSON] Token

userServer :: MonadIO m => ServerT LoginAPI (AppT m)
userServer = loginHandler :<|> registerHandler

registerHandler :: MonadIO m => LoginRequest -> AppT m Token
registerHandler (LoginRequest user pass) = do
  mKey <- putUser user pass
  case mKey of
    Just _ -> return $ Token "" ""
    Nothing -> throwError $ err400 {errBody = "Account already exists"}

loginHandler :: MonadIO m => LoginRequest -> AppT m Token
loginHandler (LoginRequest user pass) = do
  isOK <- validatePassword user pass
  if isOK then return (Token "" "") else throwError err403
