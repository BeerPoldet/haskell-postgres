{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE InstanceSigs               #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE QuasiQuotes                #-}
{-# LANGUAGE RecordWildCards            #-}

module Movie.DB where

import           Database.PostgreSQL.Simple       (Connection, execute, query_)
import           Database.PostgreSQL.Simple.SqlQQ (sql)
import           Movie                            (CreateMovie (..), Movie (..))

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

