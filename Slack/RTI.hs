module Slack.RTI where
    
import Control.Concurrent
import Control.Exception
import Control.Monad
import Data.Text (Text)
import Data.Typeable
import Data.Aeson (Object)
import Network.HTTP.Conduit
import Network.Socket
import Network.WebSockets
import Network.WebSockets.Stream
import Prelude
import Slack.Rest
import Slack.Type
import Yesod
import qualified Data.ByteString.Lazy as BSL
import qualified Data.ByteString.Char8 as BS
import qualified Data.Text as T
import qualified Data.Text.IO as T
import Debug.Trace
import Control.Applicative
import qualified OpenSSL as SSL
import qualified OpenSSL.Session as SSL
import qualified System.IO.Streams.SSL as Streams
import qualified System.IO.Streams as Streams
    
data ResultRTI = ResultRTI
    { rtiUrl   :: Text
    , rtiSelf  :: Text
    , rtiUsers :: [SlackUser]
    , rtiChannels :: [SlackChannel]
    } deriving(Show)
               
instance FromJSON ResultRTI where
    parseJSON (Object o) = do
      ok <- o .:  "ok"
      when (not ok) $ (o .: "error") >>= fail
      self <- o .: "self"
              
      ResultRTI <$> o .: "url"
                <*> (self :: Object) .: "id"
                <*> o .: "users"
                <*> o .: "channels"

    parseJSON x = show x `trace` fail $ show x

type SlackRTIClient a = (ResultRTI -> ClientApp a)

data InvalidSlackResponse = InvalidSlackResponse Text BSL.ByteString
                            deriving (Show, Typeable)
instance Exception InvalidSlackResponse

runClientWithSSL::String -> Int -> String -> ClientApp a -> IO a
runClientWithSSL host port path app = SSL.withOpenSSL $ do
    ctx <- SSL.context
    is  <- getAddrInfo Nothing (Just host) (Just $ show port)
    let a = addrAddress $ head is
        f = addrFamily $ head is
    s <- socket f Stream defaultProtocol
    connect s a
    ssl <- SSL.connection ctx s
    SSL.connect ssl
    stream <- Streams.sslToStreams ssl >>= slackStream
    runClientWithStream stream host path defaultConnectionOptions [] app
    where
      slackStream::(Streams.InputStream BS.ByteString, Streams.OutputStream BS.ByteString) -> IO Stream
      slackStream (i, o) = makeStream (Streams.read i) (writer o)
      writer o = flip Streams.write o . liftM BSL.toStrict

startSlackRTISession::Text -> SlackRTIClient a -> IO a
startSlackRTISession token app = do
    manager <- newManager conduitManagerSettings
    result  <- authGetJSON manager token "https://slack.com/api/rtm.start" []
    info <- case result of
             Right x -> return x
             Left err -> throwIO $ InvalidSlackResponse "SlackRTI" err
    url <- parseUrl' $ rtiUrl info

    let host' = BS.unpack $ host url
    let path' = BS.unpack $ path url
    runClientWithSSL host' 443 path' $ app info
        where
          parseUrl' = parseUrl . T.unpack . T.append "http" . T.dropWhile (/=':')

startSlackRTI::Text -> SlackRTIClient a -> IO ThreadId
startSlackRTI token = forkIO . forever . startSlackRTISession token