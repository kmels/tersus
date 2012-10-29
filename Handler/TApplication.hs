{-# LANGUAGE ConstraintKinds       #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE QuasiQuotes           #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeFamilies          #-}

module Handler.TApplication where

import           Control.Arrow            ((&&&))
import Control.Monad(when)
import           Data.ByteString          (ByteString)
import           Data.Maybe               (isJust)
import qualified Data.Text                as T
import           Data.Time.Clock          (getCurrentTime)
import           Handler.Messages         (initApplication)
import           Handler.TApplication.Git (pullChanges)
import           Handler.TFile(filenameContentType)
import           Import
import           Prelude                  (last)
import           Tersus.AccessKeys        (newAccessKey, newRandomKey)
import           Text.Regex.TDFA

--types
import Data.Aeson(toJSON)

--tersus
import Handler.User(requireSuperAdmin)
import Handler.Admin(getTApplicationsAdminR)

-- The data type that is expected from registerAppForm
data AppLike = AppLike {
  appLikeName            :: Text
  , appLikeIdentifier    :: Text
  , appLikeDescription   :: Textarea
  , appLikeRepositoryURL :: Text
  , appLikeContactEmail  :: Text
} deriving Show

-- A monadic form
type ErrorMessage = Text

-- A form that results in AppLike and is wraped in a widget. It receives a list of error messages to show in the widget and appLike in case default values are being shown (in case of editing and not registering).

tAppForm :: [ErrorMessage] -> Maybe TApplication -> Html -> MForm App App (FormResult AppLike,Widget)
tAppForm errormessages defaultValues extra = do
  (nameRes, nameView) <- mreq textField FieldSettings { fsId = Just "TAppNameField", fsLabel = "Application name", fsName = Just "TAppName", fsAttrs = [("placeholder","Turbo app")] } (tApplicationName <$> defaultValues)
  (identifierRes, identifierView) <- mreq identifierField FieldSettings { fsId = Just "TAppIdentifierField", fsLabel = "Application identifier", fsName = Just "TAppIdentifier", fsAttrs = [("placeholder","turbo-app")] } (tApplicationIdentifier <$> defaultValues)
  (descriptionRes, descriptionView) <- mreq textareaField FieldSettings { fsId = Just "TAppDescriptionField", fsLabel = "Description", fsName = Just "TAppDescription", fsAttrs = [("placeholder","An application that turboes your _")] } (Textarea . tApplicationDescription <$> defaultValues)
  (repositoryUrlRes, repositoryUrlView) <- mreq textField FieldSettings { fsId = Just "TApplicationRepositoryUrlField", fsLabel = "Application repository url", fsName = Just "TApplicationRepositoryUrl", fsAttrs = [("placeholder","http://github.com/turbo-nickname/turbo-app")] } (tApplicationRepositoryUrl <$> defaultValues)
  (contactEmailRes, contactEmailView) <- mreq emailField FieldSettings { fsId = Just "TApplicationContactEmailField", fsLabel = "Contact email", fsName = Just "TAppConcatEmail", fsAttrs = [("placeholder","turbo-email@example.com")] } (tApplicationContactEmail <$> defaultValues)
  let appLikeResult = AppLike <$> nameRes <*> identifierRes <*> descriptionRes <*> repositoryUrlRes <*> contactEmailRes
  let widget = $(widgetFile "TApplication/registerFormWidget")
  return (appLikeResult, widget)
  where
    --field that verifies that an application doesn't exist already.
    identifierField = checkM validateIdentifier textField      
    validateIdentifier appidfier =        
       case (tApplicationIdentifier <$> defaultValues) of 
         Just defaultAppIdentifier -> do
           tapp <- runDB $ getBy $ UniqueIdentifier $ appidfier
           return $ if (isJust tapp && defaultAppIdentifier /= appidfier)
                  then Left ("Error: application identifier exists" :: Text)
                  else Right appidfier
         Nothing -> do
           tapp <- runDB $ getBy $ UniqueIdentifier $ appidfier
           return $ if (isJust tapp)
                  then Left ("Error: application identifier exists" :: Text)
                  else Right appidfier       

getRegisterTAppR :: Handler RepHtml
getRegisterTAppR = do
  (formWidget, enctype) <- generateFormPost $ tAppForm [] Nothing
  defaultLayout $(widgetFile "TApplication/register")

getTApplicationDeleteR :: ApplicationIdentifier -> Handler RepJson
getTApplicationDeleteR appIdentifier = do
  --check for permissions
  superAdmin <- requireSuperAdmin
  case superAdmin of
    Just _ -> do
      Entity tappkey _ <- runDB $ getBy404 $ UniqueIdentifier $ appIdentifier
      runDB $ delete tappkey
      return $ RepJson $ toContent . toJSON $ TRequestResponse Success $ T.unpack appIdentifier ++ " deleted"
    _ -> permissionDenied "Permission denied " --return $ toContent .toJSON $ TRequestError NotEnoughPrivileges "Permission denied. You are not administrator of this application"
  
-- | Handles the form that registers a new TApplication
postRegisterTAppR :: Handler RepHtml
postRegisterTAppR = do
  ((result, _), _) <- runFormPost $ tAppForm [] Nothing 
  case result of
    FormSuccess appLike -> do
      -- get data from the form
      let
        (appName,(appDescription,(appRepositoryUrl,(appContactEmail,appIdentifier)))) = (appLikeName &&& appLikeDescription &&& appLikeRepositoryURL &&& appLikeContactEmail &&& appLikeIdentifier) appLike

      creationDate <- liftIO getCurrentTime --ask date
      appKey <- liftIO $ newRandomKey 32  --create a new appkey

      --insert in database
      _ <- runDB $ insert $ TApplication appName appIdentifier (unTextarea appDescription) appRepositoryUrl appContactEmail creationDate appKey
      defaultLayout $(widgetFile "TApplication/created")

    --form isn't success
    FormFailure errorMessages -> do
      (formWidget, enctype) <- generateFormPost $ tAppForm errorMessages Nothing
      defaultLayout $(widgetFile "TApplication/register")
    -- form missing
    _ -> getRegisterTAppR 

getTApplicationEditR :: ApplicationIdentifier -> Handler RepHtml 
getTApplicationEditR appIdentifier = do
  Entity _ tapp <- runDB $ getBy404 $ UniqueIdentifier $ appIdentifier  
  (formWidget, enctype) <- generateFormPost $ tAppForm [] $ Just tapp
  defaultLayout $(widgetFile "admin/TApplication/edit")

-- | processes a form produced by TApplicationeditR GET
postTApplicationEditR :: ApplicationIdentifier -> Handler RepHtml
postTApplicationEditR appIdentifier = do
  Entity tappkey tapp <- runDB $ getBy404 $ UniqueIdentifier $ appIdentifier
  ((result, _), _) <- runFormPost $ tAppForm [] $ Just tapp
  case result of
    FormSuccess appLike -> do
      -- get data from the form
      let
        (appName,(appDescription,(appRepositoryUrl,(appContactEmail,appIdentifier')))) = (appLikeName &&& unTextarea . appLikeDescription &&& appLikeRepositoryURL &&& appLikeContactEmail &&& appLikeIdentifier) appLike
        
      --update in database
      _ <- runDB $ update tappkey [TApplicationName =. appName,TApplicationDescription =. appDescription,TApplicationRepositoryUrl =. appRepositoryUrl, TApplicationContactEmail =. appContactEmail, TApplicationIdentifier =. appIdentifier']
      getTApplicationsAdminR

    --form isn't success
    FormFailure errorMessages -> do
      (formWidget, enctype) <- generateFormPost $ tAppForm errorMessages $ Just tapp
      defaultLayout $(widgetFile "admin/TApplication/edit")
    -- form missing
    _ -> getTApplicationEditR appIdentifier

userNotLogged :: ApplicationIdentifier -> Handler RepHtml
userNotLogged appIdentifier = defaultLayout $ do [whamlet|<h3>TODO: user not logged, application index of #{appIdentifier}|]

-- | The get parameter under which the access key is provided
accessKeyParam :: Text
accessKeyParam = "accessKey"

-- | The startup parameters for an application
argvParam :: Text
argvParam = "argv"

-- | Load an application. This function checks wether the user
-- is logged and if the application specified exists and redirects
-- the user to the appropiate location. If the user is logged and
-- the application exists, an access key is generated and the user
-- gets redirected to the index.html of the application.
getTAppHomeR :: ApplicationIdentifier -> Handler RepHtml
getTAppHomeR appIdentifier = do
  Entity _ tapp <- runDB $ getBy404 $ UniqueIdentifier $ appIdentifier
  maybeUserId <- maybeAuth
  maybeKey <- lookupGetParam accessKeyParam
  maybeArgv <- lookupGetParam argvParam
  let
    argv = case maybeArgv of
      Just argv' -> T.concat ["&",argvParam,"=",argv']
      Nothing -> ""
  case (maybeUserId,maybeKey) of
    (Just (Entity _ u),Nothing) -> (liftIO $ pullChanges tapp) >>= \_ -> redirectToApplication u argv
    (_,Just accessKey) -> redirectToIndex accessKey argv
    _ -> userNotLogged appIdentifier

  where
    redirectToApplication u argv = do
      let nickname = userNickname u
      accessKey <- liftIO $ newAccessKey nickname appIdentifier
      initApplication $ AppInstance nickname appIdentifier
      home <- toTextUrl $ TAppHomeR appIdentifier
      redirect $ T.unpack $ T.concat [home,"?",accessKeyParam,"=",accessKey,argv]

    redirectToIndex accessKey argv = do
      resources <- toTextUrl $ TAppResourceR appIdentifier ["index.html" :: T.Text]
      redirect $ T.unpack $ T.concat [resources,"?",accessKeyParam,"=",accessKey,argv]

-- | The slash used to separate folders in the filesystem.
fsResourceSep :: T.Text
fsResourceSep = "/"

-- | The folder where application data is stored
fsResourcePrefix :: T.Text
fsResourcePrefix = T.concat [fsResourceSep,"tmp",fsResourceSep]

-- | Matches a resource given as name and path with the mime type
-- of the resource. The mime type is matched using the extension
-- of the file.
extension :: [T.Text] -> ContentType
extension = filenameContentType . T.unpack . Prelude.last

-- | Request that delivers a resource belonging to a particular application. Resource means
-- any file in the application's repository
getTAppResourceR :: ApplicationIdentifier -> [T.Text] -> Handler (ContentType,Content)
getTAppResourceR _ [] = do
  return $ ("text/plain",toContent ("TODO: error, invalid resource " :: String))
getTAppResourceR appIdentifier resource = do
  return $ (extension resource,ContentFile (T.unpack $ T.concat [fsResourcePrefix,appIdentifier,path]) Nothing)
  where
    path = T.concat $ foldl (\x y -> x ++ [fsResourceSep] ++ [y]) [] resource

-- | Request that deploys an application (it pulls from its repository)
postDeployTAppR :: ApplicationIdentifier -> Handler RepHtml
postDeployTAppR appIdentifier  = do
  Entity _ tapp <- runDB $ getBy404 $ UniqueIdentifier $ appIdentifier
  liftIO $ pullChanges tapp
  defaultLayout $ [whamlet||]

