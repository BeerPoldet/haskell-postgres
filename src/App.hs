{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE KindSignatures             #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE TypeApplications           #-}
{-# LANGUAGE TypeOperators              #-}

module App 
  ( start
  )
  where

import           Control.Monad.IO.Class     (liftIO, MonadIO)
import           Control.Monad.Reader       (ReaderT, runReaderT, withReaderT)
import           Control.Monad.Except       (ExceptT (ExceptT), lift)
import           Data.Pool                  (Pool, createPool, withResource)
import           Data.Text
import           Database.PostgreSQL.Simple (Connection, ResultError, close)
import           DB                         (createConnection)
import           GHC.Generics               (Generic)
import           DB.Migration                  (migrate)
import           Movie                      (Movie)
import           Movie.Handler                      (unMovieHandler, MovieHandler)
import           Movie.DB                      (queryMovies, createMovie)
import           Network.Wai                (Application)
import           Network.Wai.Handler.Warp   (runSettings)
import           Network.Wai.Handler.Warp.Internal   (Settings (..))
import           DB.ResultError ()
import           Servant                    ((:<|>) (..))
import           Servant.Server             (Handler (Handler), ServerT, hoistServer,
                                             serve)
import Data.Time.Clock (NominalDiffTime)
import App.API (API, api)
import App.Config (config, configWarp, configPostgres, PostgresConfig(..), PostgresPoolConfig(..) , PostgresConnectConfig(..), Config)
import App.Error (onExceptionResponse, catchException)
import Control.Exception (SomeException)
import Control.Monad.Catch (catch, MonadCatch)

data Env = Env
  { envConfig :: Config
  , envPool   :: Pool Connection
  }
  deriving (Show)

newtype AppM m a
  = AppM
  { unApp :: ReaderT Env m a
  }
  deriving (Functor, Applicative, Monad)

server :: ServerT API (AppM Handler)
server = fromMovieDB queryMovies
  :<|> fromMovieDB . createMovie
-- Handler = ExceptT ServerError IO, ExceptT is instance of MonadIO, 
-- which I implemented as Instance of MovieDBLive.
-- On top of that, I used withReaderT to turn ReaderT (Pool Connection, Int) 
-- to ReaderT (Pool Connection) that works with MovieDBLive.

fromMovieDB :: MovieHandler m a -> AppM m a
fromMovieDB m = AppM $ withReaderT envPool (unMovieHandler m)

fromAppM :: Env -> AppM Handler a -> Handler a
fromAppM env a = catch (runReaderT (unApp a) env) catchException

-- Turn this to abstract MTL for easy mocking test
app :: Env -> Application
app env = serve api $ hoistServer api (fromAppM env) server

instance AppServer IO where
  mkConfig = 
    App.Config.config 
    >>= \cfg -> putStrLn "Running application with config:"
    >> print cfg
    >> pure cfg

  mkPool (PostgresConfig PostgresPoolConfig {..} postgresConnectConfig) = 
    createPool 
      (createConnection postgresConnectConfig)
      close 
      postgresPoolStrip
      (fromInt postgresPoolKeepOpenTime) 
      postgresPoolSize
    where fromInt :: Int -> NominalDiffTime
          fromInt = fromInteger . fromIntegral

  migrate pool = withResource pool DB.Migration.migrate

  runApp appEnv = do
    cfg <- config
    runSettings (wrapSettings cfg) $ app appEnv
    where wrapSettings cfg
            = (configWarp cfg)
            -- { settingsOnExceptionResponse = onExceptionResponse 
            -- }

class Monad m => AppServer m where
  mkConfig :: m Config
  mkPool :: PostgresConfig -> m (Pool Connection)
  migrate :: Pool Connection -> m ()
  runApp :: Env -> m ()

start :: AppServer m => m ()
start = mkConfig
  >>= \cfg -> mkPool (configPostgres cfg)
  >>= \pool -> App.migrate pool
  >> runApp (Env cfg pool)
