module App.Env where

import           App.Config                 (Config)
import           Data.Pool                  (Pool)
import           Database.PostgreSQL.Simple (Connection)

data Env = Env
  { envConfig :: Config
  , envPool   :: Pool Connection
  }
  deriving (Show)


