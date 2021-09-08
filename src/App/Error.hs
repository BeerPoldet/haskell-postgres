{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}

module App.Error where

import           Control.Exception        (SomeException, fromException)
import           Data.ByteString     (ByteString)
import           Data.ByteString.Lazy     (fromStrict, toStrict)
import qualified Network.HTTP.Types       as H
import           Network.HTTP2            (ErrorCodeId (..), HTTP2Error (..))
import           Network.Wai              (Response, responseLBS)
import           Network.Wai.Handler.Warp (InvalidRequest, defaultShouldDisplayException)
import Data.Aeson (toJSON, object, encode, (.=))
import           Servant.Server             (Handler (Handler))
import           Servant                    (ServerError (..), err400, err413, err500, throwError)
import Control.Monad.Except (ExceptT(ExceptT))
import qualified Data.Text.IO as TIO
import Data.Text (Text, pack)
import Data.Text.Encoding (encodeUtf8)
import Control.Monad (when)
import Control.Monad.IO.Class (liftIO)
import System.IO (stderr)
import Data.UUID (toText, toLazyASCIIBytes)
import Data.UUID.V4 (nextRandom)

-- Cannot figure out approach of printing json body error
onExceptionResponse :: SomeException -> Response
onExceptionResponse e
  | Just (_ :: InvalidRequest) <-
    fromException e = responseLBS H.badRequest400
                                [(H.hContentType, "text/plain; charset=utf-8")]
                                 "Bad Request"
  | Just (ConnectionError (UnknownErrorCode 413) t) <-
    fromException e = responseLBS H.status413
                                [(H.hContentType, "text/plain; charset=utf-8")]
                                 (fromStrict t)
  | Just (ConnectionError (UnknownErrorCode 431) t) <-
    fromException e = responseLBS H.status431
                                [(H.hContentType, "text/plain; charset=utf-8")]
                                 (fromStrict t)
  | otherwise       = responseLBS H.internalServerError500
                                [(H.hContentType, "application/json; charset=utf-8")]
                                $ encode $ object 
                                  [ "status" .= (500 :: Int)
                                  , "error"  .= ("Something went wrong" :: String)
                                  ]
      
catchException :: SomeException -> Handler a
catchException e = do
  uuid <- liftIO nextRandom
  liftIO $ printServerError e $ toText uuid
  throwServerError e $ toStrict $ toLazyASCIIBytes uuid

printServerError :: SomeException -> Text -> IO ()
printServerError e uuid =
    when (defaultShouldDisplayException e)
    $ TIO.hPutStrLn stderr 
    $ (("[Error: " <> uuid <> "] ") <>)
    $ pack 
    $ show e

throwServerError :: SomeException -> ByteString -> Handler a
throwServerError e uuid
  | Just (_ :: InvalidRequest) <-
    fromException e = throwError err400
  | Just (ConnectionError (UnknownErrorCode 413) t) <-
    fromException e = throwError err413
                        { errBody = fromStrict t }
  | Just (ConnectionError (UnknownErrorCode 431) t) <-
    fromException e = throwError err431
                        { errBody = fromStrict t }
  | otherwise       = throwError err500 
                        { errHeaders = [("Error-Code", uuid)] 
                        , errBody = fromStrict $ encodeUtf8 $ pack $ show e
                        }

err431 :: ServerError
err431 
  = ServerError
  { errHTTPCode = 431
  , errReasonPhrase = "Request Header Fields Too Large"
  , errBody = ""
  , errHeaders = []
  }
