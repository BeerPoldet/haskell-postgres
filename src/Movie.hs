{-# LANGUAGE DeriveGeneric     #-}

module Movie
  ( Movie (..)
  , CreateMovie (..)
  )
  where

import           Data.Aeson                         (FromJSON, ToJSON)
import           Data.Int                           (Int64)
import           Data.Time                          (LocalTime)
import           Database.PostgreSQL.Simple.FromRow (FromRow, field, fromRow)
import           Database.PostgreSQL.Simple.ToRow   (ToRow)
import           GHC.Generics                       (Generic)

data Movie = Movie
  { movieId          :: Int64
  , movieName        :: String
  , movieDescription :: String
  , movieCreatedAt   :: LocalTime
  }
  deriving (Show, Eq, Generic)

instance FromRow Movie where
  fromRow = Movie <$> field <*> field <*> field <*> field

instance FromJSON Movie
instance ToJSON  Movie

data CreateMovie = CreateMovie
  { createMovieName        :: String
  , createMovieDescription :: String
  , createMovieCreatedAt   :: LocalTime
  }
  deriving (Show, Eq, Generic)

instance FromJSON CreateMovie
instance ToRow CreateMovie
