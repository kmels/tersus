{-# OPTIONS_GHC -fno-warn-orphans #-}
module Application
    ( makeApplication
    , getApplicationDev
    , makeFoundation
    , makeApplicationWrapper
    ) where

import Import
import Settings
import Yesod.Auth
import Yesod.Default.Config
import Yesod.Default.Main
import Yesod.Default.Handlers
import Yesod.Logger (Logger, logBS, toProduction)
import Network.Wai.Middleware.RequestLogger (logCallback, logCallbackDev)
import qualified Database.Persist.Store
import Database.Persist.GenericSql (runMigration)
import Network.HTTP.Conduit (newManager, def)

-- Import all relevant handler modules here.
-- Don't forget to add new modules to your cabal file!
import Handler.Home
import Handler.TFile
import Handler.Messages
import Data.HashTable as H 
import Handler.TApplication
import Control.Concurrent.MVar

-- Import the messaging pipeline that uses CloudHaskell
import MessagingPipeline.Pipeline

-- This line actually creates our YesodSite instance. It is the second half
-- of the call to mkYesodData which occurs in Foundation.hs. Please see
-- the comments there for more details.
mkYesodDispatch "App" resourcesApp

-- Wrapper that reverses the parameters of makeApplication function from Application.hs
makeApplicationWrapper :: H.HashTable (AppInstance) String -> H.HashTable AppInstance (MVar [TMessage]) -> AppConfig DefaultEnv Extra -> Logger -> IO Application
makeApplicationWrapper addresses mailBoxes conf logger = makeApplication conf logger addresses mailBoxes

-- This function allocates resources (such as a database connection pool),
-- performs initialization and creates a WAI application. This is also the
-- place to put your migrate statements to have automatic database
-- migrations handled by Yesod.
makeApplication :: AppConfig DefaultEnv Extra -> Logger -> H.HashTable (AppInstance) String -> H.HashTable AppInstance (MVar [TMessage]) -> IO Application
makeApplication conf logger addresses mailBoxes = do
    foundation <- makeFoundation conf setLogger addresses mailBoxes
    app <- toWaiAppPlain $ foundation
    return $ logWare app
  where
    setLogger = if development then logger else toProduction logger
    logWare   = if development then logCallbackDev (logBS setLogger)
                               else logCallback    (logBS setLogger)
                               
makeFoundation :: AppConfig DefaultEnv Extra -> Logger -> H.HashTable (AppInstance) String -> H.HashTable AppInstance (MVar [TMessage]) -> IO App
makeFoundation conf setLogger addresses mailBoxes = do
    manager <- newManager def
    s <- staticSite
    dbconf <- withYamlEnvironment "config/postgres.yml" (appEnv conf)
              Database.Persist.Store.loadConfig >>=
              Database.Persist.Store.applyEnv
    p <- Database.Persist.Store.createPoolConfig (dbconf :: Settings.PersistConfig)
    Database.Persist.Store.runPool dbconf (runMigration migrateAll) p    
    return $ App conf setLogger s p manager dbconf addresses mailBoxes

hashUserApp (AppInstance username application)  = H.hashString $ username ++ application

-- for yesod devel
getApplicationDev :: H.HashTable (AppInstance) String -> H.HashTable AppInstance (MVar [TMessage]) -> IO (Int, Application)
getApplicationDev addresses mailBoxes= do    
    defaultDevelApp loader $ makeAppWrapper addresses mailBoxes
  where
    makeAppWrapper a m x y = makeApplication x y a m
    loader = loadConfig (configSettings Development)
        { csParseExtra = parseExtra
        }
