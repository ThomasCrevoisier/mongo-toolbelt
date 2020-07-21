module MongoToolbelt.Flattener
  ( flattenDocument
  , flattenField
  , flattenValue
  , module T
  ) where

import           Data.Bson                     (Binary, Document, Field (..),
                                                Function, Javascript, MD5,
                                                MinMaxKey, MongoStamp, ObjectId,
                                                Regex, Symbol, UUID,
                                                UserDefined, Value)
import qualified Data.Bson                     as B (Value (..), at)
import           Data.Map                      (Map)
import qualified Data.Map                      as M (mapKeys, singleton, unions)

import           MongoToolbelt.Flattener.Types as T

flattenDocument :: Document -> Map AttrPath FlatValue
flattenDocument = M.unions . fmap flattenField

flattenField :: Field -> Map AttrPath FlatValue
flattenField (l := v) = M.mapKeys ((AttrPath $ pure l) <>) $ flattenValue v

flattenValue :: Value -> Map AttrPath FlatValue
flattenValue (B.Float f)     = M.singleton mempty (Float f)
flattenValue (B.String txt)  = M.singleton mempty (String txt)
flattenValue (B.Bin bin)     = M.singleton mempty (Bin bin)
flattenValue (B.Fun fn)      = M.singleton mempty (Fun fn)
flattenValue (B.Uuid uuid)   = M.singleton mempty (Uuid uuid)
flattenValue (B.Md5 md5)     = M.singleton mempty (Md5 md5)
flattenValue (B.UserDef def) = M.singleton mempty (UserDef def)
flattenValue (B.ObjId oId)   = M.singleton mempty (ObjId oId)
flattenValue (B.Bool b)      = M.singleton mempty (Bool b)
flattenValue (B.UTC t)       = M.singleton mempty (UTC t)
flattenValue B.Null          = M.singleton mempty Null
flattenValue (B.RegEx rex)   = M.singleton mempty (RegEx rex)
flattenValue (B.JavaScr js)  = M.singleton mempty (JavaScr js)
flattenValue (B.Sym symb)    = M.singleton mempty (Sym symb)
flattenValue (B.Int32 i)     = M.singleton mempty (Int32 i)
flattenValue (B.Int64 i)     = M.singleton mempty (Int64 i)
flattenValue (B.Stamp t)     = M.singleton mempty (Stamp t)
flattenValue (B.MinMax k)    = M.singleton mempty (MinMax k)
flattenValue (B.Doc doc)     = flattenDocument doc
flattenValue (B.Array values) = M.unions $
  zipWith (\idx -> M.mapKeys (idx <>)) (toTextIndex <$> [0..]) (flattenValue <$> values)
