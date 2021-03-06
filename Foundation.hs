module Foundation
    ( Tersus (..)
    , Route (..)
    , resourcesTersus
    , Handler
    , Widget
    , Form
    , maybeAuth
    , requireAuth
    , module Settings
    
    ) where

import           Prelude
import           Yesod
import           Yesod.Static
import           Yesod.Auth
import           Yesod.Auth.BrowserId
import           Yesod.Auth.GoogleEmail
import qualified Yesod.Auth.GoogleEmail as GoogleEmail
import           Yesod.Default.Config
import           Yesod.Default.Util (addStaticContentExternal)
--import Yesod.Logger (Logger, logMsg, formatLogText)
import           Network.HTTP.Conduit (Manager)
import qualified Settings
import           Settings.StaticFiles
import           Settings (widgetFile, Extra (..))
import           Text.Jasmine (minifym)
import           Web.ClientSession (getKey)
import           Text.Hamlet (hamletFile)
import           Control.Monad.Maybe
--Tersus

import qualified Tersus.Auth as TersusAuth
import           Tersus.Cluster.Types
import           Tersus.DataTypes
import           Tersus.DataTypes.User
-- Hedis
import           Database.Redis
-- Text
import           Data.Text.Encoding
-- Cloud Haskell
import           Control.Distributed.Process.Binder (ProcessBinder)

data Tersus = Tersus
    { settings :: AppConfig DefaultEnv Extra
    , getStatic :: Static -- ^ Settings for static file serving.
    , redisConnection :: Connection
    , processRunner :: ProcessBinder
    , httpManager :: Manager
    , appsSendChannels :: SendAddressTable
    , appsRecvChannels :: RecvAddressTable
    , tersusNodes :: TersusClusterList
    }

-- Set up i18n messages. See the message folder.
mkMessage "Tersus" "messages" "en"

-- This is where we define all of the routes in our application. For a full
-- explanation of the syntax, please see:
-- http://www.yesodweb.com/book/handler
--
-- This function does three things:
--
-- * Creates the route datatype AppRoute. Every valid URL in your
--   application can be represented as a value of this type.
-- * Creates the associated type:
--       type instance Route App = AppRoute
-- * Creates the value resourcesApp which contains information on the
--   resources declared below. This is used in Handler.hs by the call to
--   mkYesodDispatch
--
-- What this function does *not* do is create a YesodSite instance for
-- App. Creating that instance requires all of the handler functions
-- for our application to be in scope. However, the handler functions
-- usually require access to the AppRoute datatype. Therefore, we
-- split these actions into two functions and place them in separate files.
mkYesodData "Tersus" $(parseRoutesFile "config/routes")

type Form x = Html -> MForm Tersus Tersus (FormResult x, Widget)

-- Please see the documentation for the Yesod typeclass. There are a number
-- of settings which can be configured by overriding methods here.
instance Yesod Tersus where
    approot = ApprootMaster $ appRoot . settings

    -- Store session data on the client in encrypted cookies,
    -- default session idle timeout is 120 minutes
    makeSessionBackend _ = do
        key <- getKey "config/client_session_key.aes"
        return . Just $ clientSessionBackend key 120

    defaultLayout widget = do
        master <- getYesod
        loggedUser <- TersusAuth.maybeLoggedUser (redisConnection master)
        
        io . putStrLn $ "Logged user: " ++ show loggedUser
        mmsg <- getMessage

        -- auth links        
--        tm <- getRouteToMaster
--        let googleAuth = GoogleEmail.forwardUrl
        -- Returns a list of tersus applications owned by the logged user
        userApps <- maybeUserTApps 
        
        -- We break up the default layout into two components:
        -- default-layout is the contents of the body tag, and
        -- default-layout-wrapper is the entire page. Since the final
        -- value passed to hamletToRepHtml cannot be a widget, this allows
        -- you to use normal widget features in default-layout.

        pc <- widgetToPageContent $ do
            $(widgetFile "normalize")
            addStylesheet $ StaticR css_bootstrap_css
            $(widgetFile "default-layout")
        hamletToRepHtml $(hamletFile "templates/default-layout-wrapper.hamlet")

    -- This is done to provide an optimization for serving static files from
    -- a separate domain. Please see the staticRoot setting in Settings.hs
    urlRenderOverride y (StaticR s) =
        Just $ uncurry (joinPath y (Settings.staticRoot $ settings y)) $ renderRoute s
    urlRenderOverride _ _ = Nothing

    -- The page to be redirected to when authentication is required.
    authRoute _ = Just $ AuthR LoginR

    -- This function creates static content files in the static folder
    -- and names them based on a hash of their content. This allows
    -- expiration dates to be set far in the future without worry of
    -- users receiving stale content.
    addStaticContent = addStaticContentExternal minifym base64md5 Settings.staticDir (StaticR . flip StaticRoute [])

    -- Place Javascript at bottom of the body tag so the rest of the page loads first
    jsLoader _ = BottomOfBody

instance YesodAuth Tersus where
    type AuthId Tersus = UserId

    -- Where to send a user after successful login
    loginDest _ = HomeR
    -- Where to send a user after logout
    logoutDest _ = HomeR

    getAuthId creds = do
      master <- getYesod
      let conn = redisConnection master
      -- check if the user is already in our database      
      maybeUserId <- io $ getUserId conn (credsIdent creds)      
      io . putStrLn $ " Checking if the user is already in our database ... " ++ show maybeUserId
      
      case maybeUserId of
        Right uid -> return . Just $ uid
        Left e -> do
          io . putStrLn . show $ e
          uid <- io $ insertNewUser (credsIdent creds) (credsIdent creds) Nothing False conn 
          io $ putStrLn $ " Inserted new user in the database: " ++ show uid
          return uid
                
    -- You can add other plugins like BrowserID, email or OAuth here
    authPlugins _ = [authBrowserId, authGoogleEmail]

    authHttpManager = httpManager

-- This instance is required to use forms. You can modify renderMessage to
-- achieve customized and internationalized form validation messages.
instance RenderMessage Tersus FormMessage where
    renderMessage _ _ = defaultFormMessage

-- Note: previous versions of the scaffolding included a deliver function to
-- send emails. Unfortunately, there are too many different options for us to
-- give a reasonable default. Instead, the information is available on the
-- wiki:
--
-- https://github.com/yesodweb/yesod/wiki/Sending-email

maybeUserTApps :: GHandler s Tersus [TApplication]
maybeUserTApps = do
  m <- getYesod
  let conn = redisConnection m
  maybeUser <- TersusAuth.maybeLoggedUser conn
  maybe (return []) (io . findApps conn) maybeUser
  where    
    isOwner :: UserId -> TApplication -> Bool
    isOwner uid tapp = uid `elem` owners tapp 
    
    findApps :: Connection -> User -> IO [TApplication]
    findApps conn u = getApplications conn >>= return . filter (isOwner $ uid u)
  
{-
maybeUserTApps = do
  auth <- maybeAuth
  case auth of
    Just (Entity userId user) -> do 
      uapps <- runDB $ selectList [UserApplicationUser ==. userId, UserApplicationIsAdmin ==. True] []
        --TODO Very inefficient, do a join instead.
      mapM (\(Entity _ (UserApplication _ appid _)) -> do
                         app <- runDB $ get appid -- ::Maybe TApplication
                         return $ (app :: Maybe TApplication)
           ) uapps
--      return $ (apps :: [Entity TApplication])
    _ -> return $ [] -}

maybeGet id' = MaybeT $ Yesod.get id'


io = liftIO


