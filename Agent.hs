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
    
slackRTI::ClientApp ()
slackRTI conn = do
    T.putStrLn "Connected!"

    _ <- forever $ do
      recd <- receiveData conn
      case decode recd of
        Just EventHello -> T.putStrLn "Hello"
        Just (EventMessage (Message _ c u v _)) -> do
          T.putStrLn $ T.concat [ u , "@" , c , " : " , v]
        Just (EventStarAdded u i t) -> do
          T.putStrLn $ T.concat
                  [ u
                  , "@"
                  , starMessageChannel i
                  , " starred : "
                  , messageText $ starMessageMessage i]
        Just (EventStarRemoved u i t) -> do
          T.putStrLn $ T.concat
                  [ u
                  , "@"
                  , starMessageChannel i
                  , " unsttarred : "
                  , messageText $ starMessageMessage i]
        Just (EventOther x) -> T.putStrLn $ T.append "unknown message: " x
        Nothing -> Prelude.putStrLn $ "Error message recieved: " ++ (BSL.unpack recd)

    sendClose conn ("Bye!" :: Text)

runAgent::T.Text -> IO ThreadId
runAgent token = startSlackRTI token slackRTI