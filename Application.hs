{-# OPTIONS_GHC -fno-warn-orphans #-}
module Application
    ( makeApplication
    , getApplicationDev
    , makeFoundation
    , makeApplicationWrapper
    ) where

import           Data.Int()
import           Database.Persist.GenericSql (runMigration)
import qualified Database.Persist.Store
import           Import
import           Network.HTTP.Conduit (newManager, def)
import           Network.Wai.Middleware.RequestLogger (logCallback, logCallbackDev)
import           Settings
import           Yesod.Auth
import           Yesod.Default.Config
import           Yesod.Default.Handlers
import           Yesod.Default.Main
import           Yesod.Logger (Logger, logBS, toProduction)

-- CloudHaskell stuff
import Tersus.Cluster.Types

-- Import all relevant handler modules here.
-- Don't forget to add new modules to your cabal file!
import Handler.Home
import Handler.TFile
import Handler.Messages
import Handler.TApplication

-- This line actually creates our YesodSite instance. It is the second half
-- of the call to mkYesodData which occurs in Foundation.hs. Please see
-- the comments there for more details.
mkYesodDispatch "App" resourcesApp

-- Wrapper that reverses the parameters of makeApplication function from Application.hs
-- This wrapper is generally used to init Tersus from CloudHaskell
makeApplicationWrapper :: (TMessageQueue,TMessageQueue,NotificationsChannel,MailBoxTable,TMessageStatusTable,AcknowledgementSendPort) -> AppConfig DefaultEnv Extra -> Logger -> IO Application
makeApplicationWrapper env conf logger = makeApplication conf logger env

-- This function allocates resources (such as a database connection pool),
-- performs initialization and creates a WAI application. This is also the
-- place to put your migrate statements to have automatic database
-- migrations handled by Yesod.
makeApplication :: AppConfig DefaultEnv Extra -> Logger -> (TMessageQueue,TMessageQueue,NotificationsChannel,MailBoxTable,TMessageStatusTable,AcknowledgementSendPort) -> IO Application
makeApplication conf logger env = do
    foundation <- makeFoundation conf setLogger env
    app <- toWaiAppPlain $ foundation
    return $ logWare app
  where
    setLogger = if development then logger else toProduction logger
    logWare   = if development then logCallbackDev (logBS setLogger)
                               else logCallback    (logBS setLogger)

makeFoundation :: AppConfig DefaultEnv Extra -> Logger -> (TMessageQueue,TMessageQueue,NotificationsChannel,MailBoxTable,TMessageStatusTable,AcknowledgementSendPort) -> IO App
makeFoundation conf setLogger (sendChannel,recvChannel,actionsChannel,mailBoxes,statusTable,aSendPort) = do
    manager <- newManager def
    s <- staticSite
    dbconf <- withYamlEnvironment "config/postgres.yml" (appEnv conf)
              Database.Persist.Store.loadConfig >>=
              Database.Persist.Store.applyEnv
    p <- Database.Persist.Store.createPoolConfig (dbconf :: Settings.PersistConfig)
    Database.Persist.Store.runPool dbconf (runMigration migrateAll) p    
    return $ App conf setLogger s p manager dbconf sendChannel recvChannel actionsChannel mailBoxes statusTable aSendPort

-- for yesod devel
getApplicationDev :: (TMessageQueue,TMessageQueue,NotificationsChannel,MailBoxTable,TMessageStatusTable,AcknowledgementSendPort) -> IO (Int, Application)
getApplicationDev env= do    
    defaultDevelApp loader $ makeApplicationWrapper env
  where
    loader = loadConfig (configSettings Development)
        { csParseExtra = parseExtra
        }
