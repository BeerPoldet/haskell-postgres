{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators     #-}

module App.API where

import           Movie       (CreateMovie, Movie)
import           Servant     (Get, JSON, PostCreated, Proxy (Proxy), ReqBody,
                              (:<|>) (..), (:>))
import           Servant.API (NoContent)

type API
  = Get '[JSON] NoContent
  :<|> "movies"
  :> ( Get '[JSON] [Movie]
     :<|> ReqBody '[JSON] CreateMovie :> PostCreated '[JSON] Movie
     )

api :: Proxy API
api = Proxy
