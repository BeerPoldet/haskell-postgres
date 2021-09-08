{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE OverloadedStrings          #-}

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
  { configWarp :: Settings
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
  { postgresPool :: PostgresPoolConfig
  , postgresConnect :: PostgresConnectConfig
  }
  deriving (Generic, Show)

instance Conferer.DefaultConfig PostgresConfig where
  configDef
    = PostgresConfig
    { postgresPool = Conferer.configDef
    , postgresConnect = Conferer.configDef
    }

data PostgresPoolConfig
  = PostgresPoolConfig
  { postgresPoolSize :: Int
  , postgresPoolStrip :: Int
  , postgresPoolKeepOpenTime :: Int
  }
  deriving (Generic, Show)

instance Conferer.DefaultConfig PostgresPoolConfig where
  configDef
    = PostgresPoolConfig
    { postgresPoolSize = 10
    , postgresPoolStrip = 1
    , postgresPoolKeepOpenTime = 10
    }

data PostgresConnectConfig
  = PostgresConnectConfig
  { postgresConnectHost :: String
  , postgresConnectPort :: Int
  , postgresConnectUser :: String
  , postgresConnectPassword :: String
  , postgresConnectDatabase :: String
  }
  deriving (Generic, Show)

instance Conferer.DefaultConfig PostgresConnectConfig where
  configDef
    = PostgresConnectConfig
    { postgresConnectHost = "localhost"
    , postgresConnectPort = 5432
    , postgresConnectUser = "postgres"
    , postgresConnectPassword = "reallysecret"
    , postgresConnectDatabase = "movie_store"
    }
