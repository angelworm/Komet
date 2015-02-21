module Agent where
    
import Control.Concurrent
import Control.Monad
import Network.WebSockets
import Prelude
import Slack
import Data.Text as T
import Data.Text.IO as T
    
slackRTI::ClientApp ()
slackRTI conn = do
    T.putStrLn "Connected!"

    _ <- forever $ do
      msg <- receiveData conn
      T.putStrLn msg

    sendClose conn ("Bye!" :: Text)

runAgent::T.Text -> IO ThreadId
runAgent token = startSlackRTI token slackRTI