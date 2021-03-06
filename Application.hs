{-# OPTIONS_GHC -fno-warn-orphans #-}
module Application
    ( makeApplication
    , getApplicationDev
    , makeFoundation
    , makeApplicationWrapper
    ) where

import Data.Int()
import Import
import Network.HTTP.Conduit (newManager, def)
import Network.Wai.Middleware.RequestLogger (logStdout, logStdoutDev)
import Settings

-- yesod
import Yesod.Auth
import Yesod.Default.Config
import Yesod.Default.Handlers
import Yesod.Default.Main

-- database
import qualified Database.Redis as Redis

-- CloudHaskell stuff
import Tersus.Cluster.Types
import Control.Distributed.Process.Binder

-- Import all relevant handler modules here.
-- Don't forget to add new modules to your cabal file!
import Handler.Admin
import Handler.Home
import Handler.Messages
import Handler.Permission
import Handler.Applications
import Handler.Files
import Handler.User
-- This line actually creates our YesodSite instance. It is the second half
-- of the call to mkYesodData which occurs in Foundation.hs. Please see
-- the comments there for more details.
mkYesodDispatch "Tersus" resourcesTersus

-- Wrapper that reverses the parameters of makeApplication function from Application.hs
-- This wrapper is generally used to init Tersus from CloudHaskell
makeApplicationWrapper :: (ProcessBinder,SendAddressTable,RecvAddressTable,TersusClusterList) -> AppConfig DefaultEnv Extra -> IO Application
makeApplicationWrapper env conf = makeApplication conf env

-- This function allocates resources (such as a database connection pool),
-- performs initialization and creates a WAI application. This is also the
-- place to put your migrate statements to have automatic database
-- migrations handled by Yesod.
makeApplication :: AppConfig DefaultEnv Extra -> (ProcessBinder,SendAddressTable,RecvAddressTable,TersusClusterList) -> IO Application
makeApplication conf env = do
    foundation <- makeFoundation conf env
    app' <- toWaiAppPlain $ foundation
    return $ logWare app'
  where
    logWare   = if development then logStdoutDev
                               else logStdout

makeFoundation :: AppConfig DefaultEnv Extra -> (ProcessBinder,SendAddressTable,RecvAddressTable,TersusClusterList) -> IO Tersus
makeFoundation conf (node,sendAddressTable,recvAddressTable,clusterList) = do
    manager <- newManager def
    s <- staticSite
    liftIO $ putStrLn ("Env: "++(show $ appEnv conf))
    conn <- Redis.connect Redis.defaultConnectInfo
    --dbconf <- withYamlEnvironment "config/postgres.yml" (appEnv conf)
    --          Database.Persist.Store.loadConfig >>=
    --          Database.Persist.Store.applyEnv
    --p <- Database.Persist.Store.createPoolConfig (dbconf :: Settings.PersistConfig)
    --Database.Persist.Store.runPool dbconf (runMigration migrateAll)     
    return $ Tersus conf s conn node manager sendAddressTable recvAddressTable clusterList

-- for yesod devel
getApplicationDev :: (ProcessBinder,SendAddressTable,RecvAddressTable,TersusClusterList) -> IO (Int, Application)
getApplicationDev env= do    
    defaultDevelApp loader $ makeApplicationWrapper env
  where
    loader = loadConfig (configSettings Development)
        { csParseExtra = parseExtra
        }
