module TersusCluster.Cluster where

import Prelude
import Control.Monad (forever)
import Control.Concurrent (forkIO,threadDelay)
import Remote
import Model.TMessage
import Remote.Call (mkClosureRec, mkClosure)
import Model
import Data.Text as T
import System.IO.Unsafe (unsafePerformIO)
import Data.Time.Clock (getCurrentTime)
import Control.Monad.Trans (liftIO)
import Data.HashTable as H
import Network.Wai.Handler.Warp (runSettings, defaultSettings, settingsPort)
import Application
import Control.Concurrent.MVar 
import Settings (parseExtra)
import Yesod.Default.Config (fromArgs)
import Yesod.Default.Main (defaultMain)
import Control.Concurrent.Chan
import Control.Concurrent.MVar
import Data.Time.Clock (getCurrentTime,UTCTime)
import Data.Hash.MD5
import TersusCluster.Types
import TersusCluster.MessageBackend

dummyUser = User (T.pack "neto") (Just (T.pack "1234")) []
                       
-- This is a dummy datatype only to show that this works
-- It will be removed and never used
-- unsafePerformIO is there just because it's simpler and
-- this will not be part of tersus
dummyApp = TApplication (T.pack "emacs") (T.pack "identifier") (T.pack "description dummy") (Just (T.pack "url")) (T.pack "mail@place.com") (unsafePerformIO getCurrentTime)  (T.pack "appkey")

dummyMsg = TMessage dummyUser dummyUser dummyApp dummyApp (T.pack "Alonso") (unsafePerformIO getCurrentTime)

-- Hash function for a user application combo instance
hashUserApp (AppInstance username application)  = H.hashString $ username ++ application

-- Produce all the data structures needed to communicate data between Tersusland and Clould Haskell
initDataStructures :: ProcessM (TMessageQueue,TMessageQueue,ActionsChannel,MailBoxTable,AddressTable,TMessageStatusTable,ProcessId)
initDataStructures = (liftIO $ H.new (==) hashUserApp) >>= \addresses ->
                     (liftIO $ H.new (==) hashUserApp) >>= \mailBoxes ->
                     (liftIO $ H.new (==) H.hashString) >>= \msgStatusTable ->
                     (liftIO $ newChan) >>= \sendChannel ->
                     (liftIO $ newChan) >>= \recvChannel ->
                     (liftIO $ newChan) >>= \actionsChannel ->
                     getSelfPid >>= \myPid ->
                     return (sendChannel,recvChannel,actionsChannel,mailBoxes,addresses,msgStatusTable,myPid)
                                      

-- Process that runs Tersus and Yesod in development mode
createTersusDevelInstance :: ProcessM ()
createTersusDevelInstance = do
  (sendChannel,recvChannel,actionsChannel,mailBoxes,addresses,msgStatusTable,myPid) <- initDataStructures
  liftIO $ do (port, app) <- getApplicationDev (sendChannel,recvChannel,actionsChannel,mailBoxes,msgStatusTable,myPid)
              forkIO $ runSettings defaultSettings
                         { settingsPort = port
                         } app
  runTersusMessaging sendChannel recvChannel actionsChannel mailBoxes addresses msgStatusTable 1

-- Process that runs Tersus and Yesod in producction mode
createTersusInstance :: ProcessM ()
createTersusInstance = do
  (sendChannel,recvChannel,actionsChannel,mailBoxes,addresses,msgStatusTable,myPid) <- initDataStructures
  liftIO $ forkIO $ defaultMain (fromArgs parseExtra) $ makeApplicationWrapper (sendChannel,recvChannel,actionsChannel,mailBoxes,msgStatusTable,myPid)
  runTersusMessaging sendChannel recvChannel actionsChannel mailBoxes addresses msgStatusTable 1

-- Will be removed
createTersusInstance' :: ProcessM ()
createTersusInstance' = do
                     peers <- receiveWait [matchProcesses]
                     myId <- getSelfPid
                     peers' <- return $ rmItem myId peers
                     forever $ do mapM_ (\id -> send id dummyMsg) peers
                                  mapM_ (\_ -> doRecieve) peers

                     where                        
                        rmItem _ [] = []
                        rmItem a (x:xs) 
                               | a == x = xs
                               | otherwise = x : (rmItem a xs)
                        doRecieve = do (TMessage _ _ _ _ msg _) <- receiveWait [matchTMessage]
                                       say $ "I got: " ++ (T.unpack msg)
--

-- Make the functions that initialize a tersus process remotable
remotable ['createTersusInstance,'createTersusDevelInstance]

-- Functions to initialize all the tersus nodes and distribute the ProcessId of such nodes
-- Development and producction version of the function
initTersusCluster "T2" = receiveWait []
initTersusCluster "T1" = do 
                            peers <- getPeers
                            t2s <- return $ findPeerByRole peers "T2"
                            pids <- mapM (\p -> (spawn p $(mkClosure 'createTersusInstance)))  t2s
                            mapM_ (\p -> send p pids) pids

initTersusClusterDevel "T2" = receiveWait []
initTersusClusterDevel "T1" = do peers <- getPeers
                                 t2s <- return $ findPeerByRole peers "T2"
                                 pids <- mapM (\p -> (spawn p $(mkClosure 'createTersusDevelInstance)))  t2s
                                 mapM_ (\p -> send p pids) pids

-- Initialize Tersus in producction mode running on top of CloudHaskell
tersusProducction :: IO ()
tersusProducction = do forkIO $ remoteInit (Just "config/servers") [TersusCluster.Cluster.__remoteCallMetaData] initTersusCluster
                       remoteInit (Just "config/servers2") [TersusCluster.Cluster.__remoteCallMetaData] initTersusCluster
                       return ()

-- Initialize Tersus in development mode running on top of CloudHaskell
tersusDevel :: IO ()
tersusDevel = do forkIO $ remoteInit (Just "config/servers") [TersusCluster.Cluster.__remoteCallMetaData] initTersusClusterDevel
                 remoteInit (Just "config/servers2") [TersusCluster.Cluster.__remoteCallMetaData] initTersusClusterDevel
                 return ()