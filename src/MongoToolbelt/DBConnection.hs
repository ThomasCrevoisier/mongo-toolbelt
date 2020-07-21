{-# LANGUAGE RecordWildCards #-}

module MongoToolbelt.DBConnection
  ( MongoConnectionOpts(..)
  , runMongoAction
  , readConnectionOptsFromEnv
  ) where

import           Control.Monad.IO.Class (MonadIO, liftIO)
import           Data.Text              (Text)
import qualified Data.Text              as T (pack)
import           Database.MongoDB       (Action, access, close, connect, host,
                                         slaveOk)
import           System.Environment     (getEnv)

runMongoAction :: MonadIO m => MongoConnectionOpts -> Action m a -> m a
runMongoAction MongoConnectionOpts{..} action = do
  pipe <- liftIO $ connect (host mongoHost)
  result <- access pipe slaveOk mongoDatabase action
  liftIO $ close pipe
  pure result

data MongoConnectionOpts
  = MongoConnectionOpts { mongoHost     :: String
                        , mongoDatabase :: Text
                        }

readConnectionOptsFromEnv :: IO MongoConnectionOpts
readConnectionOptsFromEnv = do
  mongoHost <- getEnv "MONGO_HOST"
  mongoDatabase <- T.pack <$> getEnv "MONGO_DATABASE"
  pure MongoConnectionOpts{..}
