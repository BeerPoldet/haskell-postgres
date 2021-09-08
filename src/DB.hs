{-# LANGUAGE RecordWildCards #-}

module DB where

import           Database.PostgreSQL.Simple (ConnectInfo (..), Connection,
                                             connect)
import App.Config (PostgresConnectConfig (..))

createConnection :: PostgresConnectConfig -> IO Connection
createConnection PostgresConnectConfig {..} = connect
  $ ConnectInfo
    { connectHost = postgresConnectHost
    , connectPort = fromIntegral postgresConnectPort
    , connectUser = postgresConnectUser
    , connectPassword = postgresConnectPassword
    , connectDatabase = postgresConnectDatabase
    }
