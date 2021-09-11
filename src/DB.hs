{-# LANGUAGE RecordWildCards #-}

module DB where

import           Database.PostgreSQL.Simple (ConnectInfo (..), Connection,
                                             connect)
import App.Config (PostgresConnectConfig (..))

createConnection :: PostgresConnectConfig -> IO Connection
createConnection PostgresConnectConfig {..} = connect
  $ ConnectInfo
    { connectHost = postgresConnectConfigHost
    , connectPort = fromIntegral postgresConnectConfigPort
    , connectUser = postgresConnectConfigUser
    , connectPassword = postgresConnectConfigPassword
    , connectDatabase = postgresConnectConfigDatabase
    }
