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

postMessageBot::Manager -> Text -> Text -> Text -> IO (OAuth2Result MessageResponse)
postMessageBot m token chan msg = postMessageWith m token chan msg q
    where
      q = [("username", "Komet")]

slackRTI::T.Text -> ClientApp ()
slackRTI token conn = do
    T.putStrLn "Connected!"
     
    m <- newManager conduitManagerSettings

    _ <- forever $ do
      recd <- receiveData conn
      case decode recd of
        Just EventHello -> T.putStrLn "Hello"
        Just (EventMessage (Message _ c u v _)) -> do
          T.putStrLn $ T.concat [ u , "@" , c , " : " , v]
        Just (EventStarAdded u i t) -> do
          let c = starMessageChannel i
          let msgLog = T.concat
                  [ u, "@", c, " starred : "
                  , messageText $ starMessageMessage i]
          T.putStrLn msgLog
          let msg = T.concat
                  [ u, " starred : "
                  , messageText $ starMessageMessage i]
          void $ postMessageBot m token c msg
        Just (EventStarRemoved u i t) -> do
          let msgLog = T.concat
                  [ u, "@", c, " unstarred : "
                  , messageText $ starMessageMessage i]
          T.putStrLn msgLog
          let msg = T.concat
                  [ u, " unstarred : "
                  , messageText $ starMessageMessage i]
          void $ postMessageBot m token c msg
        Just (EventOther x) -> T.putStrLn $ T.append "unknown message: " x
        Nothing -> Prelude.putStrLn $ "Error message recieved: " ++ (BSL.unpack recd)

    sendClose conn ("Bye!" :: Text)

runAgent::T.Text -> IO ThreadId
runAgent token = startSlackRTI token $ slackRTI token