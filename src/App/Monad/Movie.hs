{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE InstanceSigs               #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RecordWildCards            #-}

module App.Monad.Movie where

import           App.Env                    (Env (envPool))
import           App.Monad.AppM             (AppM (AppM))
import           Control.Monad.IO.Class     (MonadIO (liftIO))
import           Control.Monad.Reader       (MonadReader, ReaderT (ReaderT),
                                             ask, withReaderT)
import           Data.Pool                  (Pool, withResource)
import           Database.PostgreSQL.Simple (Connection)
import           Movie                      (Movie (..))
import           Movie.DB                   (createMovie', queryMovies')
import           Movie.Handler              (MonadCreateMovieDB (_createMovie),
                                             MonadMovieListDB (_listMovies))

newtype MovieListDB m a
  = MovieListDB
  { unMovieListDB :: ReaderT (Pool Connection) m a
  }
  deriving ( Functor
           , Applicative
           , Monad
           , MonadReader (Pool Connection)
           , MonadIO
           )

instance MonadIO m => MonadMovieListDB (MovieListDB m) where
  _listMovies :: MovieListDB m [Movie]
  _listMovies = do
    pool <- ask
    liftIO $ withResource pool queryMovies'

fromMovieListDB :: MovieListDB m a -> AppM m a
fromMovieListDB m = AppM $ withReaderT envPool (unMovieListDB m)

newtype MovieCreateDB m a
  = MovieCreateDB
  { unMovieCreateDB :: ReaderT (Pool Connection) m a
  }
  deriving ( Functor
           , Applicative
           , Monad
           , MonadReader (Pool Connection)
           , MonadIO
           )

instance MonadIO m => MonadCreateMovieDB (MovieCreateDB m) where
  _createMovie movie = do
    pool <- ask
    liftIO $ withResource pool $ createMovie' movie

fromMovieCreateDB :: MovieCreateDB m a -> AppM m a
fromMovieCreateDB m = AppM $ withReaderT envPool (unMovieCreateDB m)

