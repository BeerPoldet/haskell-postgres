{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE QuasiQuotes                #-}
{-# LANGUAGE RecordWildCards            #-}

module Movie.Handler where

import           Control.Monad.IO.Class           (MonadIO, liftIO)
import           Control.Monad.Reader             (MonadReader, ReaderT, ask)
import           Data.Pool                        (Pool, withResource)
import           Database.PostgreSQL.Simple       (Connection)
import           Movie.DB                            (MovieDB(..), queryMovies', createMovie')

newtype MovieHandler m a 
  = MovieDBLive 
  { unMovieHandler :: ReaderT (Pool Connection) m a
  }
  deriving ( Functor
           , Applicative
           , Monad
           , MonadReader (Pool Connection)
           , MonadIO
           )

instance MonadIO m => MovieDB (MovieHandler m) where
  queryMovies = do
    pool <- ask
    liftIO $ withResource pool queryMovies'

  createMovie movie = do
    pool <- ask
    liftIO $ withResource pool $ createMovie' movie
