{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Budget.App where

import Control.Monad.Except (ExceptT, MonadError)
import Control.Monad.IO.Class (MonadIO)
import Control.Monad.Reader (MonadReader, ReaderT)
import Database.Persist.Sql (ConnectionPool)
import Servant (ServerError)

data Config
  = Config
      { configConnection :: ConnectionPool,
        configPort :: Int
      }

mkConfig :: Config
mkConfig = Config conn port
  where
    conn = undefined
    port = 8080

newtype AppT m a
  = AppT
      { runAppT :: ReaderT Config (ExceptT ServerError m) a
      }
  deriving (Functor, Applicative, Monad, MonadReader Config, MonadIO, MonadError ServerError)
