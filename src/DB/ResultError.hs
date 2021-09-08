{-# LANGUAGE OverloadedStrings #-}

module DB.ResultError where

import           Data.Aeson                 (ToJSON, Value (String), toJSON)
import           Data.Text                  (pack)
import           Database.PostgreSQL.Simple (ResultError)

instance ToJSON ResultError where
  toJSON err = String $ pack $ show err
