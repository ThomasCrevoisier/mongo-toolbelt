module CLI
  ( opts
  , Command(..)
  , SearchIdOpts(..)
  ) where

import           Data.Bson           (ObjectId)
import           Options.Applicative
import           Safe                (readMay)

opts :: ParserInfo Command
opts = info (actionParser <**> helper)
        ( fullDesc
        <> progDesc "Set of helpers to keep your sanity when using MongoDB"
        )

actionParser :: Parser Command
actionParser =
  let parser = SearchId . SearchIdOpts <$> argument (maybeReader readMay) idm
      desc = progDesc "Search usage of an ID across the database"
   in subparser (command "search-id" $ info parser desc)

data Command
  = SearchId SearchIdOpts

data SearchIdOpts
  = SearchIdOpts { idToSearch :: ObjectId
                 }
