{-# LANGUAGE OverloadedLists   #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TupleSections     #-}

module Main
  ( main
  ) where

import           Data.Bson               (Binary, Document, Field (..),
                                          Function, Javascript, MD5, MinMaxKey,
                                          MongoStamp, ObjectId, Regex, Symbol,
                                          UUID, UserDefined, Value)
import qualified Data.Bson               as B (Value (..), at)
import           Data.Foldable           (traverse_)
import           Data.Int                (Int32, Int64)
import           Data.List               as L (intersperse)
import           Data.Map                (Map)
import qualified Data.Map                as M
import           Data.Text               (Text)
import qualified Data.Text               as T (pack, unpack)
import           Data.Time               (UTCTime)
import           Database.MongoDB        (Action, access, allCollections, close,
                                          connect, find, host, rest, select,
                                          slaveOk)

import           MongoToolbelt.Flattener (AttrPath, FlatValue (..),
                                          flattenDocument, toReadablePath)
import           MongoToolbelt.Search    (CollectionPerimeter (..),
                                          CollectionSearchResult (..),
                                          DocumentSearchResult (..),
                                          SearchOptions (..), searchInDatabase)

import           Options.Applicative     (execParser)

import           CLI                     (Command (..), SearchIdOpts(..), opts)
import           Commands.Search         (searchAndPrintIdRefs)

main :: IO ()
main = run =<< execParser opts
  where
    run (SearchId SearchIdOpts{..}) = searchAndPrintIdRefs idToSearch
