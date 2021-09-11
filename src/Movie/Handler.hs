{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE InstanceSigs               #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE QuasiQuotes                #-}
{-# LANGUAGE RecordWildCards            #-}

module Movie.Handler where

import           Control.Monad.IO.Class     (MonadIO, liftIO)
import           Control.Monad.Reader       (MonadReader, ReaderT, ask)
import           Data.Pool                  (Pool, withResource)
import           Database.PostgreSQL.Simple (Connection)
import           Movie                      (CreateMovie, Movie)
import           Movie.DB                   (MovieDB (..), createMovie',
                                             queryMovies')

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
  queryMovies :: MovieHandler m [Movie]
  queryMovies = do
    pool <- ask
    liftIO $ withResource pool queryMovies'

  createMovie :: CreateMovie -> MovieHandler m Movie
  createMovie movie = do
    pool <- ask
    liftIO $ withResource pool $ createMovie' movie
