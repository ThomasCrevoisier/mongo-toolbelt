{-# LANGUAGE OverloadedStrings #-}

module MongoToolbelt.Stream
  ( MongoStream
  , cursorToStream
  ) where

import           Control.Monad.IO.Class     (MonadIO, liftIO)
import           Control.Monad.Trans.Class  (lift)
import           Data.Bson                  (Document)
import           Database.MongoDB.Query     (Action, Cursor, nextBatch, find, select)
import           Streaming                  (Of, Stream)
import qualified Streaming.Prelude          as S (each, print, length_)

type MongoStream a m r = Stream a (Action m) r

cursorToStream :: MonadIO m => Cursor -> MongoStream (Of Document) m ()
cursorToStream cursor = do
  results <- lift $ nextBatch cursor
  if null results
    then pure ()
    else S.each results >> cursorToStream cursor
