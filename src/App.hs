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

import           App.API                           (api)
import           App.Config                        (Config, PostgresConfig (..),
                                                    PostgresPoolConfig (..),
                                                    config, configPostgres,
                                                    configWarp)
import           App.Env                           (Env (Env))
import           App.Error                         (catchException,
                                                    onExceptionResponse)
import           App.Monad.AppM                    (AppM (unApp))
import           App.Server                        (server)
import           Control.Monad.Catch               (catch)
import           Control.Monad.Reader              (runReaderT)
import           DB                                (createConnection)
import           DB.Migration                      (migrate)
import           DB.ResultError                    ()
import           Data.Pool                         (Pool, createPool,
                                                    withResource)
import           Data.Time.Clock                   (NominalDiffTime)
import           Database.PostgreSQL.Simple        (Connection, close)
import           Network.Wai                       (Application)
import           Network.Wai.Handler.Warp          (runSettings)
import           Network.Wai.Handler.Warp.Internal (Settings (..))
import           Servant                           (Handler)
import           Servant.Server                    (hoistServer, serve)

-- Turn this to abstract MTL for easy mocking test
app :: Env -> Application
app env = serve api $ hoistServer api (fromAppM env) server

fromAppM :: Env -> AppM Handler a -> Handler a
fromAppM env a = catch (runReaderT (unApp a) env) catchException

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
      postgresPoolConfigStrip
      (fromInt postgresPoolConfigKeepOpenTime)
      postgresPoolConfigSize
    where fromInt :: Int -> NominalDiffTime
          fromInt = fromInteger . fromIntegral

  migrate pool = withResource pool DB.Migration.migrate

  runApp appEnv = do
    cfg <- config
    runSettings (wrapSettings cfg) $ app appEnv
    where wrapSettings cfg
            = (configWarp cfg)
            { settingsOnExceptionResponse = onExceptionResponse
            }

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
