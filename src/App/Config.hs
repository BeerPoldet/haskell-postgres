{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}

module App.Config where

import qualified Conferer
import           Conferer.FromConfig.Warp          ()
import           Data.Text                         ()
import           GHC.Generics                      (Generic)
import           Network.Wai.Handler.Warp.Internal (Settings (Settings, settingsHost, settingsPort))

config :: IO Config
config =
  Conferer.mkConfig "movi"
  >>= Conferer.fetch

data Config
  = Config
  { configWarp     :: Settings
  , configPostgres :: PostgresConfig
  }
  deriving (Generic, Show)

instance Show Settings where
  show Settings { settingsPort = port, settingsHost = host } =
    "Settings " ++
    "{ settingsPort = " ++ show port ++
    ", settingsHost = " ++  show host ++
    "}"

instance Conferer.FromConfig Config

instance Conferer.DefaultConfig Config where
  configDef
    = Config
    { configWarp = Conferer.configDef
    , configPostgres = Conferer.configDef
    }

data PostgresConfig
  = PostgresConfig
  { postgresConfigPool    :: PostgresPoolConfig
  , postgresConfigConnect :: PostgresConnectConfig
  }
  deriving (Generic, Show)

instance Conferer.FromConfig PostgresConfig

instance Conferer.DefaultConfig PostgresConfig where
  configDef
    = PostgresConfig
    { postgresConfigPool = Conferer.configDef
    , postgresConfigConnect = Conferer.configDef
    }

data PostgresPoolConfig
  = PostgresPoolConfig
  { postgresPoolConfigSize         :: Int
  , postgresPoolConfigStrip        :: Int
  , postgresPoolConfigKeepOpenTime :: Int
  }
  deriving (Generic, Show)

instance Conferer.DefaultConfig PostgresPoolConfig where
  configDef
    = PostgresPoolConfig
    { postgresPoolConfigSize = 10
    , postgresPoolConfigStrip = 1
    , postgresPoolConfigKeepOpenTime = 10
    }

instance Conferer.FromConfig PostgresPoolConfig

data PostgresConnectConfig
  = PostgresConnectConfig
  { postgresConnectConfigHost     :: String
  , postgresConnectConfigPort     :: Int
  , postgresConnectConfigUser     :: String
  , postgresConnectConfigPassword :: String
  , postgresConnectConfigDatabase :: String
  }
  deriving (Generic, Show)

instance Conferer.FromConfig PostgresConnectConfig

instance Conferer.DefaultConfig PostgresConnectConfig where
  configDef
    = PostgresConnectConfig
    { postgresConnectConfigHost = "localhost"
    , postgresConnectConfigPort = 5432
    , postgresConnectConfigUser = "postgres"
    , postgresConnectConfigPassword = "reallysecret"
    , postgresConnectConfigDatabase = "movie_store"
    }
