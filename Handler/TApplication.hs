{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE QuasiQuotes           #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE ConstraintKinds       #-}

module Handler.TApplication where

import           Control.Arrow             ((&&&))
import           Control.Monad             (when)
import           Control.Monad.Trans.Class (lift)
import           Data.Maybe                (isNothing)
import qualified Data.Text                 as T
import           Data.Time.Clock           (getCurrentTime)
import           Handler.Messages          (initApplication)
import           Handler.TApplication.Git  (pullChanges)
import           Import
import           Network.HTTP.Types        (status200)
--import           Network.Wai
import           Tersus.AccessKeys         (decompose, newAccessKey,
                                            newRandomKey)
import           Yesod.Auth
import Text.Regex.Posix
import Prelude (last)
import Data.ByteString (ByteString)

import Data.Maybe(isJust)
import Data.String

-- The data type that is expected from registerAppForm
data AppLike = AppLike {
  appLikeName :: Text
  , appLikeIdentifier :: Text
  , appLikeDescription :: Text
  , appLikeRepositoryURL :: Maybe Text
  , appLikeContactEmail :: Text
} deriving Show

-- A monadic form
type ErrorMessage = Text

registerAppForm :: [ErrorMessage] -> Html -> MForm App App (FormResult AppLike,Widget)
registerAppForm errormessages extra = do
  (nameRes, nameView) <- mreq textField "{- not used -}" Nothing
  (descriptionRes, descriptionView) <- mreq textField "{- not used-}" Nothing
  (repositoryUrlRes, repositoryUrlView) <- mopt textField "{- not used -}" Nothing
  (contactEmailRes, contactEmailView) <- mreq textField "{- not used -}" Nothing
  (identifierRes, identifierView) <- mreq identifierField "{- not used -}" Nothing
  let appLikeResult = AppLike <$> nameRes <*> identifierRes <*> descriptionRes <*> repositoryUrlRes <*> contactEmailRes
  let widget = $(widgetFile "TApplication/registerFormWidget")
  return (appLikeResult, widget)
  where
    --field that verifies that an application doesn't exist already.
    identifierField = checkM validateIdentifier textField
    validateIdentifier appidfier = do
       tapp <- runDB $ getBy $ UniqueIdentifier $ appidfier
       return $ if isJust tapp
                then Left ("Error: app exists" :: Text)
                else Right appidfier
    

getRegisterTAppR :: Handler RepHtml
getRegisterTAppR = do
  (formWidget, enctype) <- generateFormPost $ registerAppForm []
  defaultLayout $(widgetFile "TApplication/register")
  
-- | Handles the form that registers a new TApplication
postRegisterTAppR :: Handler RepHtml
postRegisterTAppR = do
  ((result, _), _) <- runFormPost $ registerAppForm []
  case result of
    FormSuccess appLike -> do
      -- get data from the form
      let
        (appName,(appDescription,(appRepositoryUrl,(appContactEmail,appIdentifier)))) = (appLikeName &&& appLikeDescription &&& appLikeRepositoryURL &&& appLikeContactEmail &&& appLikeIdentifier) appLike

      creationDate <- liftIO getCurrentTime --ask date
      appKey <- liftIO $ newRandomKey 32  --create a new appkey
      
      --insert in database
      _ <- runDB $ insert $ TApplication appName appIdentifier appDescription appRepositoryUrl appContactEmail creationDate appKey
      defaultLayout $(widgetFile "TApplication/created")
      
    --form isn't success
    FormFailure errorMessages -> do
      (formWidget, enctype) <- generateFormPost $ registerAppForm errorMessages
      defaultLayout $(widgetFile "TApplication/register")
    -- form missing  
    _ -> getRegisterTAppR

userNotLogged :: ApplicationIdentifier -> Handler RepHtml
userNotLogged appIdentifier = defaultLayout $ do [whamlet|<h3>TODO: user not logged, application index of #{appIdentifier}|]

-- | The get parameter under which the access key is provided
accessKeyParam :: Text
accessKeyParam = "accessKey"

-- | Load an application. This function checks wether the user
-- is logged and if the application specified exists and redirects
-- the user to the appropiate location. If the user is logged and
-- the application exists, an access key is generated and the user
-- gets redirected to the index.html of the application.
getTAppHomeR :: ApplicationIdentifier -> Handler RepHtml
getTAppHomeR appIdentifier = do
  Entity _ app <- runDB $ getBy404 $ UniqueIdentifier $ appIdentifier
  maybeUserId <- maybeAuth
  maybeKey <- lookupGetParam accessKeyParam
  case (maybeUserId,maybeKey) of
    (Just (Entity userId user),Nothing) -> (liftIO $ pullChanges app) >>= \_ -> redirectToApplication user
    (_,Just accessKey) -> redirectToIndex accessKey
    _ -> userNotLogged appIdentifier

  where
    redirectToApplication user = do
      let nickname = userNickname user
      accessKey <- liftIO $ newAccessKey nickname appIdentifier
      initApplication $ AppInstance (T.unpack $ nickname) (T.unpack $ appIdentifier)
      home <- toTextUrl $ TAppHomeR appIdentifier
      redirect $ T.unpack $ T.concat [home,"?",accessKeyParam,"=",accessKey]

    redirectToIndex accessKey = do
      resources <- toTextUrl $ TAppResourceR appIdentifier ["index.html" :: T.Text]
      redirect $ T.unpack $ T.concat [resources,"?",accessKeyParam,"=",accessKey]

-- | The slash used to separate folders in the filesystem.
fsResourceSep :: T.Text
fsResourceSep = "/"

-- | The folder where application data is stored
fsResourcePrefix :: T.Text
fsResourcePrefix = T.concat [fsResourceSep,"tmp",fsResourceSep]

-- | Match a file extension ending with the appropiate mime-type
extensionMatcher :: Maybe String -> ContentType
extensionMatcher ext = case ext of
  (Just ".html") -> "text/html"
  (Just ".js") -> "text/javascript"
  (Just ".css") -> "text/css"
  _ -> "text/plain"

-- | Mathches a resource given as name and path with the mime type
-- of the resource. The mime type is matched using the extension
-- of the file.
extension :: [T.Text] -> ContentType
extension [] = "text/plain"
extension l = extensionMatcher $ (T.unpack $ Prelude.last l) =~~ ("\\.\\w+$" :: ByteString)

-- | Request that delivers a resource belonging to a particular application. Resource means
-- any file in the application's repository
getTAppResourceR :: ApplicationIdentifier -> [T.Text] -> Handler (ContentType,Content)
getTAppResourceR appIdentifier resource = do
  liftIO $ putStrLn $ (T.unpack $ T.concat [fsResourcePrefix,appIdentifier,path])
  return $ (extension resource,ContentFile (T.unpack $ T.concat [fsResourcePrefix,appIdentifier,path]) Nothing)
  where
    path = T.concat $ foldl (\x y -> x ++ [fsResourceSep] ++ [y]) [] resource

-- | Request that deploys an application (it pulls from its repository)
postDeployTAppR :: ApplicationIdentifier -> Handler RepHtml
postDeployTAppR appIdentifier  = do
  Entity _ tapp <- runDB $ getBy404 $ UniqueIdentifier $ appIdentifier
  liftIO $ pullChanges tapp
  defaultLayout [whamlet| TODO deployed|]