{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE QuasiQuotes                #-}
{-# LANGUAGE RecordWildCards            #-}

module Movie.DB where

import           Database.PostgreSQL.Simple       (Connection, query_, execute)
import           Database.PostgreSQL.Simple.SqlQQ (sql)
import           Movie                            (Movie(..), CreateMovie(..))

class Monad m => MovieDB m where
  queryMovies :: m [Movie]
  createMovie :: CreateMovie -> m Movie

queryMovies' :: Connection -> IO [Movie]
queryMovies' c = query_ c [sql|select * from movies|]

createMovie' :: CreateMovie -> Connection -> IO Movie
createMovie' movie@CreateMovie {..} c =
  execute 
    c 
    [sql|insert into movies (name, description, created_at) values (?, ?, ?) |]
    movie 
  >>= \mId -> return $ Movie mId createMovieName createMovieDescription createMovieCreatedAt

