{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module Commands.Search where

import           Control.Monad.IO.Class     (MonadIO, liftIO)
import           Data.Bson                  (ObjectId)
import           Data.Foldable              (traverse_)
import           Data.Monoid                (Sum (..))
import           Data.Text                  (Text)
import qualified Data.Text                  as T (pack, unpack)
import           Database.MongoDB           (Action, access, close, connect,
                                             host, slaveOk)
import           MongoToolbelt.Flattener    (AttrPath, FlatValue (..),
                                             toReadablePath)
import           System.Environment         (getEnv)

import           MongoToolbelt.DBConnection
import           MongoToolbelt.Search       (CollectionPerimeter (..),
                                             CollectionSearchResult (..),
                                             DocumentSearchResult (..),
                                             SearchOptions (..),
                                             searchInDatabase)

searchAndPrintIdRefs :: ObjectId -> IO ()
searchAndPrintIdRefs objId =
  let run action = do
        opts <- liftIO readConnectionOptsFromEnv
        runMongoAction opts action
      search = searchInDatabase (SearchOptions AllCollections) (== (ObjId objId))
      prettyPrint = traverse_ prettyPrintCounts
   in
     prettyPrint =<< run search

prettyPrintByCollection :: CollectionSearchResult -> IO ()
prettyPrintByCollection CollectionSearchResult{..} = do
  putStrLn $ "Collection: " <> T.unpack collection
  if null matchingDocuments
    then putStrLn "No result found"
    else traverse_ prettyPrintResults matchingDocuments

prettyPrintCounts :: CollectionSearchResult -> IO ()
prettyPrintCounts CollectionSearchResult{..} =
  let nbDocuments = length matchingDocuments
      nbReferences = getSum $ foldMap (Sum . length . matchingValues) matchingDocuments
    in do
      putStrLn $ T.unpack collection
      putStrLn $ "⮩ " <> show nbReferences <> " references found in " <> show nbDocuments <> " documents"
