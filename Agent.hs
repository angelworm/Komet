module Agent where
    
import Control.Concurrent
import Control.Monad
import Network.WebSockets
import Prelude
import Slack
import Data.Text as T
import Data.Text.IO as T
import Control.Applicative
import Data.Aeson
import qualified Data.ByteString.Lazy.Char8 as BSL
import Network.HTTP.Conduit
import Yesod.Auth.OAuth2 (OAuth2Result)
import Data.Maybe
import Control.Exception
import Debug.Trace
    
postMessageBot::Manager -> Text -> Text -> Text -> IO (OAuth2Result MessageResponse)
postMessageBot m token chan msg = postMessageWith m token chan msg q
    where
      q = [("username", "Komet")]

lookupChanName::ResultRTI -> Text -> Maybe Text
lookupChanName info chanid = lookup chanid cs
    where
      cs = Prelude.map getPair $ rtiChannels info
      getPair = (,) <$> chanId <*> chanName

lookupUserName::ResultRTI -> Text -> Maybe Text
lookupUserName info userid = lookup (traceShowId $ userid) us
    where
      us = Prelude.map getPair $ rtiUsers info
      getPair = (,) <$> userId <*> userName
          
slackRTI::T.Text -> SlackRTIClient ()
slackRTI token info conn = do
    let self = rtiSelf info

    T.putStrLn $ T.append "Connected! you are :" self
    Prelude.putStrLn $ show self
    m <- newManager conduitManagerSettings

    void $ forever $ trymsg $ do
      recd <- receiveData conn
      case decode recd of
        Just EventHello -> T.putStrLn "Hello"
        Just (EventMessage (Message _ c u v _)) -> do
          T.putStrLn $ T.concat [ fromMaybe u $ lookupUserName info u
                                , "@"
                                , fromMaybe "Shangri-La" $ lookupChanName info c
                                , " : " , v]
        Just (EventStarAdded u' i _) ->  when (u' == self) $ do
          let c = fromMaybe "Shangri-La" $ lookupChanName info $ starMessageChannel i
          let u = fromMaybe u' $ lookupUserName info u'
          let msgLog = T.concat
                  [ u, "@", c, " starred : "
                  , messageText $ starMessageMessage i]
          Prelude.putStrLn $ "addstar/" ++ show u
          T.putStrLn msgLog
          let msg = T.concat
                  [ u, " starred : "
                  , messageText $ starMessageMessage i]
          a <- postMessageBot m token (starMessageChannel i) msg
          Prelude.putStrLn $ show a
        Just (EventStarRemoved u' i _) -> when (u' == self) $ do
          let c = fromMaybe "Shangri-La" $ lookupChanName info $ starMessageChannel i
          let u = fromMaybe u' $ lookupUserName info u'
          let msgLog = T.concat
                  [ u, "@", c, " unstarred : "
                  , messageText $ starMessageMessage i]
          T.putStrLn msgLog
          let msg = T.concat
                  [ u, " unstarred : "
                  , messageText $ starMessageMessage i]
          void $ postMessageBot m token (starMessageChannel i) msg
        Just (EventOther x) -> T.putStrLn $ T.append "unknown message: " x
        Nothing -> Prelude.putStrLn $ "Error message recieved: " ++ (BSL.unpack recd)

    sendClose conn ("Bye!" :: Text)
--    runAgent token
        where
          trymsg m = catch m (\x -> Prelude.putStrLn $ "L:" ++ show (x::SomeException))

runAgent::T.Text -> IO ThreadId
runAgent token = startSlackRTI token $ slackRTI token