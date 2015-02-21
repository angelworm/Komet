module Handler.Home where

import Import hiding(lookup, delete)
import Yesod.Form.Bootstrap3 (BootstrapFormLayout (..), renderBootstrap3,
                              withSmallInput)
import Agent
import Control.Concurrent
import Data.List

-- This is a handler function for the GET request method on the HomeR
-- resource pattern. All of your resource patterns are defined in
-- config/routes
--
-- The majority of the code you will write in Yesod lives in these handler
-- functions. You can spread them across multiple files if you are so
-- inclined, or create a single monolithic file.
getHomeR :: Handler Html
getHomeR = do
    (formWidget, formEnctype) <- generateFormPost sampleForm
    let submission = Nothing :: Maybe (FileInfo, Text)
        handlerName = "getHomeR" :: Text
    startUserRTI
    maid <- maybeAuthId
    defaultLayout $ do
        aDomId <- newIdent
        setTitle "Welcome To Yesod!"
        $(widgetFile "homepage")

postHomeR :: Handler Html
postHomeR = do
    ((result, formWidget), formEnctype) <- runFormPost sampleForm
    let handlerName = "postHomeR" :: Text
        submission = case result of
            FormSuccess res -> Just res
            _ -> Nothing

    maid <- maybeAuthId
    defaultLayout $ do
        aDomId <- newIdent
        setTitle "Welcome To Yesod!"
        $(widgetFile "homepage")

sampleForm :: Form (FileInfo, Text)
sampleForm = renderBootstrap3 BootstrapBasicForm $ (,)
    <$> fileAFormReq "Choose a file"
    <*> areq textField (withSmallInput "What's on the file?") Nothing

startUserRTI::Handler ()
startUserRTI = do
  maid <- maybeAuth
  case maid of
    Nothing        -> return ()
    Just (Entity _ user) -> do
      app         <- getYesod
      let userid  = userIdent user
      let sockets = appRTISocket app
      thread <- lookup userid <$> (liftIO $ readIORef $ sockets)
      case thread of
        Just th -> liftIO $ do
          killThread th
          modifyIORef sockets $ delete (userid,th)
        _ -> return ()
      let manager = appHttpManager app
      let tok = userToken user
      th <- liftIO $ runAgent tok
      modifyIORef sockets ((userid,th):)
      