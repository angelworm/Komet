module Agent where
    
import Control.Applicative
import Control.Concurrent
import Control.Monad
import Control.Exception
import Data.Aeson
import Data.Maybe
import Data.Text as T
import Data.Text.IO as T
import Debug.Trace
import Network.HTTP.Conduit
import Network.WebSockets
import Prelude
import Slack
import Yesod.Auth.OAuth2 (OAuth2Result)
import qualified Data.ByteString.Lazy.Char8 as BSL
    
postMessageBot::Manager -> Text -> Text -> Text -> IO (OAuth2Result MessageResponse)
postMessageBot m token chan msg = postMessageWith m token chan msg q
    where
      q = [("username", "Komet")]

postIMBot::Manager -> Text -> Text -> Text -> IO (OAuth2Result MessageResponse)
postIMBot m token user msg = do
  res' <- imOpenWith m token user q
  res <- case res' of
    Left err -> throw $ InvalidSlackResponse "Slack" err
    Right x  -> return x
  chan <- if   imOpenResponceOk res
          then return $ imOpenResponceChannelID res
          else fail $ T.unpack $ T.append "cannot open im channel : " user
  postMessageBot m token chan msg
    where
      q = [("username", "Komet")]

lookupChanName::ResultRTI -> Text -> Maybe Text
lookupChanName info chanid = lookup chanid cs
    where
      cs = Prelude.map getPair $ rtiChannels info
      getPair = (,) <$> chanId <*> chanName

lookupUserName::ResultRTI -> Text -> Maybe Text
lookupUserName info userid = lookup userid us
    where
      us = Prelude.map getPair $ rtiUsers info
      getPair = (,) <$> userId <*> userName
          
slackRTI::T.Text -> SlackRTIClient ()
slackRTI token info conn = do
    m <- newManager conduitManagerSettings

    forkPingThread conn 10

    T.putStrLn $ T.append "Connected! you are " $ fromMaybe self $ lookupUserName info self

    void $ forever $ do
      recd <- receiveData conn
      case decode recd of
        Just x  -> reciever m x
        Nothing -> Prelude.putStrLn $ "Error message recieved: " ++ (BSL.unpack recd)
    where
      self::Text
      self = rtiSelf info
             
      reciever::Manager -> SlackEvent -> IO ()
      reciever m (EventMessage (Message _ c u v _)) = T.putStrLn msg
          where
            msg = T.concat [ fromMaybe u $ lookupUserName info u
                           , "@"
                           , fromMaybe "Shangri-La" $ lookupChanName info c
                           , " : " , v]
      reciever m (EventStarAdded u' i _) = do
          let c = fromMaybe "Shangri-La" $ lookupChanName info $ starMessageChannel i
          let u = fromMaybe u' $ lookupUserName info u'
          let msgLog = T.concat
                       [ u, "@", c, " starred : "
                       , messageText $ starMessageMessage i]
          let msg = T.concat
                  [ u, " ★ "
                  , messageText $ starMessageMessage i]
          T.putStrLn msgLog
          void $ if u' == self
                 then postMessageBot m token (starMessageChannel i) msg
                 else postIMBot m token u' msg
      reciever m (EventStarRemoved u' i _) = do
          let c = fromMaybe "Shangri-La" $ lookupChanName info $ starMessageChannel i
          let u = fromMaybe u' $ lookupUserName info u'
          let msgLog = T.concat
                       [ u, "@", c, " unstarred : "
                       , messageText $ starMessageMessage i]
          let msg = T.concat
                  [ u, " ☆ "
                  , messageText $ starMessageMessage i]
          T.putStrLn msgLog
          void $ if u' == self
                 then postMessageBot m token (starMessageChannel i) msg
                 else postIMBot m token u' msg
      reciever m (EventStarRemoved u' i _) | otherwise = return ()
      reciever m (EventOther x) = T.putStrLn $ T.append "unknown message: " x
      reciever m _              = return ()

runAgent::T.Text -> IO ThreadId
runAgent token = startSlackRTI token $ slackRTI token