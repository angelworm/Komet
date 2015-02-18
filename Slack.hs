module Slack where

import Control.Exception
import Control.Monad
import Data.Aeson
import Data.Text
import Data.Text.Encoding
import Data.Maybe
import Prelude
import Yesod.Auth
import Yesod.Auth.OAuth2
    
import Network.HTTP.Conduit (Manager)

newtype ResultCred = ResultCred [(Text, Text)]

instance FromJSON ResultCred where
    parseJSON (Object o) = do
      ok <- o .:  "ok"
      when (not ok) $ (o .: "error") >>= fail
      extra <- (o .: "user") >>= \x -> do
               uid  <- x .: "id"
               name <- x .: "name"
               real <- x .: "real_name"
               return [ ("uid",  uid)
                      , ("name", name)
                      , ("real", real)]
      return $ ResultCred $ extra

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
      callback::Manager->AccessToken -> IO (Creds m)
      callback manager accesstoken = do
        result <- authGetJSON manager accesstoken "https://slack.com/api/users.info"
        case result of
          Right (ResultCred cred) -> return $ mkCreds accesstoken cred
          Left err -> throwIO $ InvalidProfileResponse "Slack" err

      mkCreds::AccessToken->[(Text, Text)]->Creds m
      mkCreds at extra = Creds "Slack" ident extra'
          where
            ident  = fromMaybe "NOTFOUND" $ lookup "uid" extra
            extra' = ("token", decodeUtf8 $ accessToken at) : extra
