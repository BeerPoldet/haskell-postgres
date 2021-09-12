{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE InstanceSigs               #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE QuasiQuotes                #-}
{-# LANGUAGE RecordWildCards            #-}

module Movie.Handler where

import           Movie                      (CreateMovie, Movie)

listMovies :: (MonadMovieListDB m) => m [Movie]
listMovies = do
  _listMovies

class Monad m => MonadMovieListDB m where
  _listMovies :: m [Movie]

createMovie :: (MonadCreateMovieDB m) => CreateMovie -> m Movie
createMovie = _createMovie

class Monad m => MonadCreateMovieDB m where
  _createMovie :: CreateMovie -> m Movie
