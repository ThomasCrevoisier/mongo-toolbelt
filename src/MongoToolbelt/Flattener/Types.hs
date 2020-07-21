{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}

module MongoToolbelt.Flattener.Types
  ( AttrPath(..)
  , toTextIndex
  , toReadablePath
  , FlatValue(..)
  ) where

import           Data.Bson (Binary, Document, Field (..), Function, Javascript,
                            MD5, MinMaxKey, MongoStamp, ObjectId, Regex, Symbol,
                            UUID, UserDefined, Value)
import qualified Data.Bson as B (Value (..), at)
import           Data.Int  (Int32, Int64)
import qualified Data.List as L (intersperse)
import           Data.Text (Text)
import qualified Data.Text as T (pack)
import           Data.Time (UTCTime)

newtype AttrPath
  = AttrPath [Text]
  deriving newtype (Eq, Ord, Show, Semigroup, Monoid)

toTextIndex :: Int -> AttrPath
toTextIndex = AttrPath . pure . T.pack . show

toReadablePath :: AttrPath -> Text
toReadablePath (AttrPath p) = mconcat $ L.intersperse "." p

data FlatValue
  = Float Double
  | String Text
  | Bin Binary
  | Fun Function
  | Uuid UUID
  | Md5 MD5
  | UserDef UserDefined
  | ObjId ObjectId
  | Bool Bool
  | UTC UTCTime
  | Null
  | RegEx Regex
  | JavaScr Javascript
  | Sym Symbol
  | Int32 Int32
  | Int64 Int64
  | Stamp MongoStamp
  | MinMax MinMaxKey
  deriving (Show, Eq, Ord)
