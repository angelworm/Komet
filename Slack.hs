module Slack where

import Control.Exception
import Control.Monad
import Data.Aeson
import Data.Text
import Data.Text.Encoding
import Prelude
import Yesod.Auth
import Yesod.Auth.OAuth2
    
--import Network.HTTP.Conduit (Manager)

newtype ResultCred m  = ResultCred (AccessToken->Creds m)

instance FromJSON (ResultCred m) where
    parseJSON (Object o) = do
      ok <- o .:  "ok"
      when (not ok) $ (o .: "error") >>= fail
      return $ ResultCred $ (\x -> Creds "Slack" (decodeUtf8 $ accessToken x) undefined)

authSlack:: YesodAuth m => Text -> Text -> AuthPlugin m
authSlack authid secret = authOAuth2 service oauth callback
    where
      service = "Slack"
      oauth   = OAuth2
                { oauthClientId            = encodeUtf8 authid
                , oauthClientSecret        = encodeUtf8 secret
                , oauthOAuthorizeEndpoint  = "https://slack.com/oauth/authorize"
                , oauthAccessTokenEndpoint = "https://slack.com/api/oauth.access"
                , oauthCallback = Nothing
                }
      callback::AccessToken -> IO (Creds m)
      callback accesstoken = do
        result <- authGetJSON accesstoken "https://slack.com/api/users.info"
        case result of
          Right (ResultCred cred) -> return $ cred accesstoken
          Left err -> throwIO $ InvalidProfileResponse "Slack" err
