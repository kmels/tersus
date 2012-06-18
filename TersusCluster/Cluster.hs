module TersusCluster.Cluster where

import           Application
import           Control.Concurrent (forkIO,threadDelay)
import           Control.Concurrent.MVar
import           Control.Monad (forever)
import           Control.Monad.Trans (liftIO)
import qualified Data.Binary as B
import           Data.HashTable as H
import           Data.Text as T
import           Data.Time.Clock (getCurrentTime)
import           Data.Typeable.Internal (Typeable)
import           Data.Int(Int32)
import           Model
import           Network.Wai.Handler.Warp (runSettings, defaultSettings, settingsPort)
import           Prelude
import           Remote
import           Remote.Call (mkClosure)
import           Settings (parseExtra)
import           System.Exit
import           System.IO.Unsafe (unsafePerformIO)
import           System.Posix.Signals
import           Yesod.Default.Config (fromArgs)
import           Yesod.Default.Main (defaultMain)

data TersusClusterSignals = TKill deriving Typeable

instance B.Binary TersusClusterSignals where
    put TKill = B.put (1 :: Int)
    get = (B.get :: B.Get Int) >>= \s -> 
          case s of
            1 -> return TKill
            --TODO fix this, I added this expresion to avoid warnings
            _ -> return TKill
            
dummyUser :: User
dummyUser = User (T.pack "neto") (Just (T.pack "1234")) []

-- This is a dummy datatype only to show that this works
-- It will be removed and never used
-- unsafePerformIO is there just because it's simpler and
-- this will not be part of tersus
dummyApp :: TApplication
dummyApp = TApplication (T.pack "emacs") (T.pack "identifier") (T.pack "description dummy") (Just (T.pack "url")) (T.pack "mail@place.com") (unsafePerformIO getCurrentTime)  (T.pack "appkey")

dummyMsg :: TMessage
dummyMsg = TMessage dummyUser dummyUser dummyApp dummyApp (T.pack "Alonso")

-- Function to match a process id sent from another process
matchProcesses :: MatchM [ProcessId] ()
matchProcesses = match return

-- Function to match a Message sent from another process                        
matchTMessage :: MatchM TMessage ()
matchTMessage = match return

-- Hash function for a user application combo instance
hashUserApp :: AppInstance -> Int32
hashUserApp (AppInstance username' app')  = H.hashString $ username' ++ app'

-- Process that runs Tersus and Yesod in development mode
createTersusDevelInstance :: ProcessM ()
createTersusDevelInstance = do addresses <- liftIO $ H.new (==) hashUserApp
                               mailBoxes <- liftIO $ H.new (==) hashUserApp
                               _ <- liftIO $ do (port, tAppInstance) <- getApplicationDev addresses mailBoxes    
                                                forkIO $ runSettings defaultSettings
                                                    { settingsPort = port
                                                    } tAppInstance
                               runTersusMessaging addresses mailBoxes

-- Process that runs Tersus and Yesod in producction mode
createTersusInstance :: ProcessM ()
createTersusInstance = do addresses <- liftIO $ H.new (==) hashUserApp
                          mailBoxes <- liftIO $ H.new (==) hashUserApp
                          _ <- liftIO $ forkIO $ defaultMain (fromArgs parseExtra) $ makeApplicationWrapper addresses mailBoxes
                          runTersusMessaging addresses mailBoxes

-- Function that handles messaging among the multiple Tersus instances
runTersusMessaging :: H.HashTable (AppInstance) String -> H.HashTable AppInstance (MVar [TMessage]) -> ProcessM ()
runTersusMessaging _ _ = receiveWait [killSignal] >> return ()

killSignal :: MatchM TersusClusterSignals ()
killSignal = match return

-- Will be removed
createTersusInstance' :: ProcessM ()
createTersusInstance' = do
                     peers <- receiveWait [matchProcesses]
--kmels: THIS WAS NOT BEING USED SO I COMMENTED IT
--                     myId <- getSelfPid
--kmels: THIS WAS NOT BEING USED SO I COMMENTED IT
--                     peers' <- return $ rmItem myId peers 
                     forever $ do mapM_ (\tid -> send tid dummyMsg) peers
                                  mapM_ (\_ -> doRecieve) peers
                     where                        
--kmels: THIS WAS NOT BEING USED SO I COMMENTED IT
--                        rmItem _ [] = []
--                        rmItem a (x:xs) 
--                               | a == x = xs
--                               | otherwise = x : (rmItem a xs)
                        doRecieve = do (TMessage _ _ _ _ msg) <- receiveWait [matchTMessage]
                                       say $ "I got: " ++ (T.unpack msg)
--

-- Make the functions that initialize a tersus process remotable
remotable ['createTersusInstance,'createTersusDevelInstance]

-- Functions to initialize all the tersus nodes and distribute the ProcessId of such nodes
-- Development and producction version of the function
initTersusCluster :: String -> ProcessM ()
initTersusCluster "T2" = receiveWait []
initTersusCluster "T1" = do 
                            peers <- getPeers
                            t2s <- return $ findPeerByRole peers "T2"
                            pids <- mapM (\p -> (spawn p $(mkClosure 'createTersusInstance)))  t2s
                            mapM_ (\p -> send p pids) pids
initTersusCluster _ = return ()

sigTermCatch :: MVar Int -> IO ()
sigTermCatch mVar = putMVar mVar 1 >>
                    threadDelay 10000000 >>
                    exitWith ExitSuccess
                                
                    
-- TODO: Add description of what this function does
initTersusClusterDevel :: String -> ProcessM ()
initTersusClusterDevel "T2" = receiveWait [killSignal] >> return ()
initTersusClusterDevel "T1" = do peers <- getPeers
                                 t2s <- return $ findPeerByRole peers "T2"
                                 pids <- mapM (\p -> (spawn p $(mkClosure 'createTersusDevelInstance)))  t2s
                                 mapM_ (\p -> send p pids) pids
                                 killMvar <- liftIO $ newEmptyMVar
                                 _ <- liftIO $ installHandler sigTERM (Catch (sigTermCatch killMvar) ) Nothing
                                 _ <- liftIO $ takeMVar killMvar
                                 mapM_ (\p -> send p TKill) pids
                                 return ()
initTersusClusterDevel _ = return()

-- Initialize Tersus in producction mode running on top of CloudHaskell
tersusProducction :: IO ()
tersusProducction = do 
  _ <- forkIO $ remoteInit (Just "config/servers") [TersusCluster.Cluster.__remoteCallMetaData] initTersusCluster
  remoteInit (Just "config/servers2") [TersusCluster.Cluster.__remoteCallMetaData] initTersusCluster
  return ()

-- Initialize Tersus in development mode running on top of CloudHaskell
tersusDevel :: IO ()
tersusDevel = do 
  _ <- forkIO $ remoteInit (Just "config/servers") [TersusCluster.Cluster.__remoteCallMetaData] initTersusClusterDevel
  remoteInit (Just "config/servers2") [TersusCluster.Cluster.__remoteCallMetaData] initTersusClusterDevel
  return ()