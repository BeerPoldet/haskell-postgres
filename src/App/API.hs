{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE OverloadedStrings                  #-}
{-# LANGUAGE TypeOperators                  #-}

module App.API where

import           Movie   (Movie, CreateMovie)
import           Servant ((:>), (:<|>) (..), Get, JSON, PostCreated, Proxy (Proxy), ReqBody)

type API = "movies" 
  :> ( Get '[JSON] [Movie]
     :<|> ReqBody '[JSON] CreateMovie :> PostCreated '[JSON] Movie
     )

api :: Proxy API
api = Proxy
