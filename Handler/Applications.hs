{-# LANGUAGE ConstraintKinds       #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE QuasiQuotes           #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeFamilies          #-}

module Handler.Applications where

import           Control.Arrow                   ((&&&))
import           Control.Monad                   (when)
import           Data.ByteString                 (ByteString)
import           Data.Maybe                      (isJust,fromMaybe)
import qualified Data.Text                       as T
import           Data.Time.Clock                 (getCurrentTime)
import           Handler.Messages                (initApplication)
import           Handler.TApplication.Git        (pullChanges)
import           Handler.Files                   (filenameContentType,pathContentType)
import           Import
import           Prelude                         (last)
import           Tersus.AccessKeys               (newAccessKey, newRandomKey)
import           Tersus.Global                   (orElse)
import           Text.Regex.TDFA
--haskell platform
import           Data.Maybe                      (catMaybes)

--json
import           Data.Aeson                      (toJSON)

--monads
import           Control.Monad.Trans.Maybe

--tersus
import           Database.Redis
import           Handler.Admin                   (getTApplicationsAdminR)
import           Handler.User                    (requireAdminFor,requireSuperAdmin)
import           Tersus.Filesystem               (pathToString, tAppDirPath)
import           Tersus.Filesystem.Resources
import           Tersus.Global
import           Tersus.HandlerMachinery
import           Tersus.Yesod.Handler(requireGetParameter)
-- Temporary fix of a yesod bug
import           Text.Julius
instance ToJavascript String where
         toJavascript = toJavascript . rawJS

--
data Hole = Hole
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

tAppForm :: [ErrorMessage] -> Maybe TApplication -> Html -> MForm Tersus Tersus (FormResult AppLike,Widget)
tAppForm errormessages defaultValues extra = do
  m <- lift getYesod
  let conn = redisConnection m  
  user <- lift . requireLogin $ conn
  let user = User 0 "todo" "todo" Nothing False
  
  (nameRes, nameView) <- mreq textField FieldSettings { fsId = Just "TAppNameField", fsLabel = "Application name", fsName = Just "TAppName", fsAttrs = [("placeholder","Turbo app")] } (name <$> defaultValues)
  (identifierRes, identifierView) <- mreq (identifierField conn) FieldSettings { fsId = Just "TAppIdentifierField", fsLabel = "Application identifier", fsName = Just "TAppIdentifier", fsAttrs = [("placeholder","turbo-app")] } (identifier <$> defaultValues)
  (descriptionRes, descriptionView) <- mreq textareaField FieldSettings { fsId = Just "TAppDescriptionField", fsLabel = "Description", fsName = Just "TAppDescription", fsAttrs = [("placeholder","An application that turboes your _")] } (Textarea . description <$> defaultValues)
  (repositoryUrlRes, repositoryUrlView) <- mreq textField FieldSettings { fsId = Just "TApplicationRepositoryUrlField", fsLabel = "Application repository url", fsName = Just "TApplicationRepositoryUrl", fsAttrs = [("placeholder","http://github.com/turbo-nickname/turbo-app")] } (repositoryUrl <$> defaultValues)
  (contactEmailRes, contactEmailView) <- mreq emailField FieldSettings { fsId = Just "TApplicationContactEmailField", fsLabel = "Contact email", fsName = Just "TAppConcatEmail", fsAttrs = [("placeholder","turbo-email@example.com")] } ((contactEmail <$> defaultValues) `orElse` (Just $ email user))
  let appLikeResult = AppLike <$> nameRes <*> identifierRes <*> descriptionRes <*> repositoryUrlRes <*> contactEmailRes
  let widget = $(widgetFile "TApplication/registerFormWidget")
  return (appLikeResult, widget)
  where
    -- field that verifies that an application doesn't exist already.
    identifierField conn = checkM (validateIdentifier conn) textField
                      
    -- if an application exists, we must check whether some owner is editing it
    -- or there is in fact a user that wants to create an app that is already owned
    appExists :: ApplicationIdentifier -> Either Text ApplicationIdentifier
    appExists idfier = maybe (Right idfier) checkEditMode maybeIdentifier where
       maybeIdentifier = identifier <$> defaultValues
       
       -- checks whether the id that already exists, is the same as that on the form
       checkEditMode :: ApplicationIdentifier -> Either Text ApplicationIdentifier
       checkEditMode appIdentifierOnForm | appIdentifierOnForm == idfier = Right idfier
                                           | otherwise = Left ("Error: application identifier exists" :: Text)
       
    validateIdentifier :: Connection -> ApplicationIdentifier -> GHandler s m (Either Text ApplicationIdentifier)
    validateIdentifier conn appidfier = do
      aid <- io $ conn `getAppId` appidfier -- :: Either Terror AppId
      return $ case aid of
        Left _ -> Right appidfier -- if it's a Left, identifier is free to use, otherwise appExists
        Right _ -> appExists appidfier --exists        
    
getRegisterTAppR :: Handler RepHtml
getRegisterTAppR = do
  t <- getYesod
  user <- requireLogin (redisConnection t)
  (formWidget, enctype) <- generateFormPost $ tAppForm [] Nothing
  defaultLayout $(widgetFile "TApplication/register")

deleteTApplicationR :: ApplicationIdentifier -> Handler RepJson
deleteTApplicationR identifier = do
  --check for permissions
  {-superAdmin <- requireSuperAdmin
  case superAdmin of
    Just _ -> do
      Entity tappkey _ <- runDB $ getBy404 $ UniqueIdentifier $ identifier

      --delete administrators (or users) first (foreign keys)
      runDB $ deleteWhere [UserApplicationApplication ==. tappkey]
      --delete app
      runDB $ delete tappkey

      return $ RepJson $ toContent . toJSON $ TRequestResponse Success $ (Message $ identifier `T.append` T.pack " deleted")
    _ -> permissionDenied "Permission denied " --return $ toContent .toJSON $ TRequestError NotEnoughPrivileges "Permission denied. You are not administrator of this application"-}
  permissionDenied "TODO"

-- | Handles the form that registers a new TApplication
postRegisterTAppR :: Handler RepHtml
postRegisterTAppR = do  
  user <- getYesod >>= requireLogin . redisConnection 
  
  ((result, _), _) <- runFormPost $ tAppForm [] Nothing
  case result of
    FormSuccess appLike -> do
      -- get data from the form
      let
        (appName,(appDescription,(appRepositoryUrl,(appContactEmail,identifier)))) = (appLikeName &&& appLikeDescription &&& appLikeRepositoryURL &&& appLikeContactEmail &&& appLikeIdentifier) appLike

      creationDate <- liftIO getCurrentTime --ask date
      appKey <- liftIO $ newRandomKey 32  --create a new appkey

      io $ putStrLn " Inserting "
      --insert in database
      t <- getYesod
      let conn = redisConnection t
      _ <- io $ insertNewTApp conn appName identifier (unTextarea appDescription) appRepositoryUrl appContactEmail creationDate appKey [uid user]

      defaultLayout $(widgetFile "TApplication/created")

    --form isn't success
    FormFailure errorMessages -> do
      (formWidget, enctype) <- generateFormPost $ tAppForm errorMessages Nothing
      defaultLayout $(widgetFile "TApplication/register")
    -- form missing
    _ -> getRegisterTAppR

getTApplicationEditR :: ApplicationIdentifier -> Handler RepHtml
getTApplicationEditR identifier = do
  {-Entity _ tapp <- runDB $ getBy404 $ UniqueIdentifier $ identifier
  (formWidget, enctype) <- generateFormPost $ tAppForm [] $ Just tapp
  let appname = identifier
  adminList <- fetchAdminsOf identifier
  let manageTAppAdminsWidget = $(widgetFile "admin/TApplication/manageTAppAdminsWidget")

  defaultLayout $(widgetFile "admin/TApplication/edit")-}
  permissionDenied "TODO"

-- | processes a form produced by TApplicationeditR GET
postTApplicationEditR :: ApplicationIdentifier -> Handler RepHtml
postTApplicationEditR identifier = do
  {-Entity tappkey tapp <- runDB $ getBy404 $ UniqueIdentifier $ identifier
  ((result, _), _) <- runFormPost $ tAppForm [] $ Just tapp
  case result of
    FormSuccess appLike -> do
      -- get data from the form
      let
        (appName,(appDescription,(appRepositoryUrl,(appContactEmail,identifier')))) = (appLikeName &&& unTextarea . appLikeDescription &&& appLikeRepositoryURL &&& appLikeContactEmail &&& appLikeIdentifier) appLike

      --update in database
      _ <- runDB $ update tappkey [TApplicationName =. appName,TApplicationDescription =. appDescription,TApplicationRepositoryUrl =. appRepositoryUrl, TApplicationContactEmail =. appContactEmail, TApplicationIdentifier =. identifier']
      getTApplicationsAdminR

    --form isn't success
    FormFailure errorMessages -> do
      (formWidget, enctype) <- generateFormPost $ tAppForm errorMessages $ Just tapp
      let appname = identifier
      let manageTAppAdminsWidget = $(widgetFile "admin/TApplication/manageTAppAdminsWidget")
      adminList <- fetchAdminsOf identifier
      defaultLayout $(widgetFile "admin/TApplication/edit")
    -- form missing
    _ -> getTApplicationEditR identifier-}
    permissionDenied "TODO"

userNotLogged :: ApplicationIdentifier -> Handler RepHtml
userNotLogged identifier = defaultLayout $ do [whamlet|<h3>TODO: user not logged, application index of #{identifier}|]

-- | The startup parameters for an application
argvParam :: Text
argvParam = "argv"

-- | Load an application. This function checks wether the user
-- is logged and if the application specified exists and redirects
-- the user to the appropiate location. If the user is logged and
-- the application exists, an access key is generated and the user
-- gets redirected to the index.html of the application.
getTAppHomeR :: ApplicationIdentifier -> Handler RepHtml
getTAppHomeR identifier = do
  tersus <- getYesod
  let
    con = redisConnection tersus
  app <- liftIO $ getTApplicationByName con identifier
  maybeKey <- maybeAccessKey
  maybeArgv <- lookupGetParam argvParam
  let
    argv = fromMaybe "" $ maybeArgv >>= \a -> return $ T.concat ["&",argvParam,"=",a]
  mUser <- maybeLoggedUser con
  case (app,maybeKey,mUser) of
    (Left _,_,_) -> notFound
    (Right _,_,Nothing) -> userNotLogged identifier
    (Right _,Just k,_) -> redirectToIndex k argv
    (Right a,_,Just user) -> (liftIO $ pullChanges a) >>= \_ -> redirectToApplication user argv
    _ -> notFound
      
  where
    redirectToApplication u argv = do
      let nickname' = nickname u
      accessKey <- liftIO $ newAccessKey nickname' identifier
      initApplication $ AppInstance nickname' identifier
      home <- toTextUrl $ TAppHomeR identifier
      redirect $ T.unpack $ T.concat [home,"?",accessKeyParameterName,"=",accessKey,argv]

    redirectToIndex accessKey argv = do
      resources <- toTextUrl $ TAppResourceR identifier ["index.html" :: T.Text]
      redirect $ T.unpack $ T.concat [resources,"?",accessKeyParameterName,"=",accessKey,argv]

-- | The slash used to separate folders in the filesystem.
fsResourceSep :: T.Text
fsResourceSep = "/"

-- | The folder where application data is stored
fsResourcePrefix :: T.Text
fsResourcePrefix = T.concat [fsResourceSep,"tmp",fsResourceSep]

-- | Request that delivers a resource belonging to a particular application. Resource means
-- any file in the application's repository
getTAppResourceR :: ApplicationIdentifier -> Path -> Handler (ContentType,Content)
getTAppResourceR appname path@[] = do  
  conn <- getConn
  tapp <- requireTApplication conn appname
  files <- io $ listAppResources conn (appname:path)
  RepHtml content <- defaultLayout $(widgetFile "r/list_files")
  returnHtml content
--  return $ ("text/plain",toContent ("TODO: error, invalid resource " :: String))
getTAppResourceR identifier resourcePath = do
  return $ (pathContentType resourcePath, contentFile Nothing)
  where
    contentFile = ContentFile $ pathToString $ tAppDirPath identifier ++ resourcePath

-- | Request that deploys an application (it pulls from its repository)
postDeployTAppR :: ApplicationIdentifier -> Handler RepHtml
postDeployTAppR identifier  = do
  {-Entity _ tapp <- runDB $ getBy404 $ UniqueIdentifier $ identifier
  liftIO $ pullChanges tapp-}
  defaultLayout $ [whamlet||]
  


----------------------------------------
-- Ajax calls
----------------------------------------

-- | Adds an admin to an application
putTApplicationAdminR :: ApplicationIdentifier -> Handler RepJson
putTApplicationAdminR identifier = do
  -- _ <- requireAdminFor $ identifier
  -- Entity tappkey _ <- runDB $ getBy404 $ UniqueIdentifier $ identifier

  -- --get new user (does it exist?)
  -- maybeNewAdmin <- runMaybeT $ do
  --   adminNickname <- MaybeT $ lookupPostParam "newAdminNickname"
  --   MaybeT $ runDB $ getBy $ UniqueNickname adminNickname

  -- case maybeNewAdmin of
  --     Just (Entity userkey user ) -> do -- insert new admin
  --       --exists?
  --       existing <- runDB $ selectFirst [UserApplicationApplication ==. tappkey, UserApplicationUser ==. userkey, UserApplicationIsAdmin ==. True] []
  --       case existing of
  --         Just _ -> entityExists user
  --         _ -> do
  --           _ <- runDB $ insert $ UserApplication userkey tappkey True
  --           entityCreated user
  --     Nothing -> invalidArguments "User doesnt exist"
  permissionDenied "TODO"
-- | Deletes an admin from an application
deleteTApplicationAdminR :: ApplicationIdentifier -> Handler RepJson
deleteTApplicationAdminR identifier = do
  {-_ <- requireAdminFor identifier
  Entity tappkey _ <- runDB $ getBy404 $ UniqueIdentifier $ identifier

  --get the admin
  maybeAdmin <- runMaybeT $ do
    adminNickname <- MaybeT $ lookupPostParam "adminNickname"
    MaybeT $ runDB $ getBy $ UniqueNickname adminNickname

  case maybeAdmin of
    Just (Entity userkey user) -> do
      uappRelation <- runDB $ selectFirst [UserApplicationApplication ==. tappkey, UserApplicationUser ==. userkey, UserApplicationIsAdmin ==. True] []
      case uappRelation of
        Just (Entity uapprkey _) -> do
          runDB $ delete uapprkey
          entityDeleted user
        _ -> invalidArguments $ "User is not an administrator of this application"
    _ -> invalidArguments $ "User was not provided or does not exist"-}
  permissionDenied "TODO"

-- | Returns a list of admins given an application
fetchAdminsOf :: ApplicationIdentifier -> GHandler s m [User]
fetchAdminsOf identifier = do
  {-Entity tappkey _ <- runDB $ getBy404 $ UniqueIdentifier identifier
  uapps <- runDB $ selectList [UserApplicationApplication ==. tappkey, UserApplicationIsAdmin ==. True] []
  adminMaybes <- mapM userAppToUser uapps
  return $ catMaybes adminMaybes -}
  return []

