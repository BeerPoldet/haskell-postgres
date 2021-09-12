module App.Server where

import           App.API         (API)
import           App.Monad.AppM  (AppM)
import           App.Monad.Movie (fromMovieCreateDB, fromMovieListDB)
import           Movie.Handler   (createMovie, listMovies)
import           Servant         (ServerT, (:<|>) (..))
import           Servant.API     (NoContent (NoContent))
import           Servant.Server  (Handler)

server :: ServerT API (AppM Handler)
server
  = pure NoContent
  :<|> fromMovieListDB listMovies
  :<|> fromMovieCreateDB . createMovie
