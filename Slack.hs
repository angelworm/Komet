module Slack where

import Control.Exception
import Control.Monad
import Data.Aeson
import Data.Text
import Data.Text.Encoding
import Data.Maybe
import Prelude
import Yesod.Auth
import Yesod.Auth.OAuth2 hiding(authGetJSON)
import Network.HTTP.Conduit
import qualified Data.ByteString.Char8 as BS

import Network.HTTP.Conduit (Manager)
newtype ResultCred = ResultCred [(Text, Text)]

instance FromJSON ResultCred where
    parseJSON (Object o) = do
      ok <- o .:  "ok"
      when (not ok) $ (o .: "error") >>= fail
      uid  <- o .: "user_id"
      tid  <- o .: "team_id"
      name <- o .: "user"
      team <- o .: "team"
      return $ ResultCred $ [ ("uid",  uid)
                            , ("tid",  tid)
                            , ("name", name)
                            , ("team", team)]

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
        putStrLn $ show accesstoken
        result <- authGetJSON manager (decodeUtf8 $ accessToken accesstoken) "https://slack.com/api/auth.test" []
        case result of
          Right (ResultCred cred) -> return $ mkCreds accesstoken cred
          Left err -> throwIO $ InvalidProfileResponse "Slack" err

      mkCreds::AccessToken->[(Text, Text)]->Creds m
      mkCreds at extra = Creds "Slack" ident extra'
          where
            ident  = fromJust $ lookup "uid" extra
            extra' = ("token", decodeUtf8 $ accessToken at) : extra


authGetJSON:: FromJSON a => Manager-> Text -> URI -> QueryParams -> IO (OAuth2Result a)
authGetJSON manager token uri params = liftM parseResponseJSON $ liftM handleResponse $ do
  req <- parseUrl $ BS.unpack $ appendQueryParam uri $ ("token", encodeUtf8 token):params
  httpLbs req manager
