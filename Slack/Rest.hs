module Slack.Rest where

import Control.Exception
import Control.Monad
import Control.Monad.IO.Class
import Data.Aeson
import Data.Maybe
import Data.Monoid
import Data.Text
import Data.Text.Encoding
import Data.UUID (toString)
import Data.UUID.V4 (nextRandom)
import Network.HTTP.Conduit
import Prelude
import Yesod.Auth
import Yesod.Auth.OAuth2 hiding(authGetJSON)
import Yesod.Core
import Yesod.Form
import qualified Data.ByteString.Char8 as BS
    
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

authSlack::YesodAuth m => Text -> Text -> AuthPlugin m
authSlack authid secret = authSlackScoped authid secret ["identify", "read", "post"]

authSlackScoped::YesodAuth m => Text -> Text -> [Text] -> AuthPlugin m
authSlackScoped authid secret scopes = basicPlugin {apDispatch = dispatch}
    where
      oauth = OAuth2
              { oauthClientId            = encodeUtf8 authid
              , oauthClientSecret        = encodeUtf8 secret
              , oauthOAuthorizeEndpoint  = encodeUtf8 $ "https://slack.com/oauth/authorize?scope=" `append` intercalate "," scopes
              , oauthAccessTokenEndpoint = "https://slack.com/api/oauth.access"
              , oauthCallback            = Nothing
              }

      withState state = authOAuth2 "Slack"
                        (oauth {oauthOAuthorizeEndpoint = oauthOAuthorizeEndpoint oauth `BS.append` "&state=" `BS.append` encodeUtf8 state})
                        callback

      basicPlugin = authOAuth2 "Slack" oauth callback

      dispatch "GET" ["forward"] = do
        state <- liftIO $ fmap (pack . toString) nextRandom
        setSession "slackState" state
        apDispatch (withState state) "GET" ["forward"]

      dispatch "GET" ["callback"] = do
        state <- lift $ runInputGet $ ireq textField "state"
        savedState <- lookupSession "slackState"
        _ <- apDispatch basicPlugin "GET" ["callback"]
        case savedState of
          Just saved | saved == state -> apDispatch basicPlugin "GET" ["callback"]
          Just saved -> invalidArgs ["state: " <> state <> ", and not: " <> saved]
          _ -> invalidArgs ["state: " <> state]

      dispatch method ps = apDispatch basicPlugin method ps

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
