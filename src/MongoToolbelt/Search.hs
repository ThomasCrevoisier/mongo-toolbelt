{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TupleSections     #-}

module MongoToolbelt.Search
  ( searchInAllDatabase
  , searchInDatabase
  , searchInCollection
  , CollectionSearchResult(..)
  , DocumentSearchResult(..)
  , SearchOptions(..)
  , CollectionPerimeter(..)
  ) where

import           Control.Monad.Trans.Class (lift)
import           Data.Bson                 (ObjectId)
import qualified Data.Bson                 as B (at)
import qualified Data.Map                  as M (filter, keys)
import           Data.Set                  (Set)
import qualified Data.Set                  as S (filter, fromList, notMember,
                                                 toList)
import           Data.Text                 (Text)
import           Database.MongoDB          (Action, allCollections, find, rest,
                                            select)
import           Streaming                 (Of)
import qualified Streaming.Prelude         as Str (filter, map, toList_)

import           MongoToolbelt.Flattener   (AttrPath, FlatValue,
                                            flattenDocument)
import           MongoToolbelt.Stream      (MongoStream, cursorToStream)

data CollectionSearchResult
  = CollectionSearchResult { collection        :: Text
                           , matchingDocuments :: [DocumentSearchResult]
                           }

searchInAllDatabase :: (FlatValue -> Bool) -> Action IO [CollectionSearchResult]
searchInAllDatabase = searchInDatabase (SearchOptions AllCollections)

searchInDatabase :: SearchOptions -> (FlatValue -> Bool) -> Action IO [CollectionSearchResult]
searchInDatabase SearchOptions{..} p = do
  cols <- applyPerimeter perimeter <$> listCollections
  traverse (\c -> CollectionSearchResult c <$> searchInCollection c p) (S.toList cols)

data SearchOptions
  = SearchOptions { perimeter :: CollectionPerimeter }

data CollectionPerimeter
  = AllCollections
  | RestrictedTo (Set Text)
  | Omit (Set Text)

applyPerimeter :: CollectionPerimeter -> Set Text -> Set Text
applyPerimeter AllCollections cols   = cols
applyPerimeter (RestrictedTo cols) _ = cols
applyPerimeter (Omit cols') cols     = S.filter (`S.notMember` cols') cols

listCollections :: Action IO (Set Text)
listCollections = S.fromList <$> allCollections

data DocumentSearchResult
  = DocumentSearchResult { documentId     :: ObjectId
                         , matchingValues :: [AttrPath]
                         }

searchInCollection' :: Text -> (FlatValue -> Bool) -> MongoStream (Of DocumentSearchResult) IO ()
searchInCollection' col p =
  let findAll = lift $ find (select [] col)
      documents = cursorToStream =<< findAll
      keepMatchingValues doc =
        DocumentSearchResult { documentId = B.at "_id" doc
                             , matchingValues = M.keys $ M.filter p (flattenDocument doc)
                             }
   in Str.filter (not . null . matchingValues) $ Str.map keepMatchingValues documents

searchInCollection :: Text -> (FlatValue -> Bool) -> Action IO [DocumentSearchResult]
searchInCollection col p = Str.toList_ $ searchInCollection' col p
