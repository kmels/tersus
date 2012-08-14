{-# LANGUAGE OverloadedStrings #-}
module Tersus.Cluster.TersusService where

-- TersusService
-- Contains all the functions and datatypes that control a server side Tersus Application
-- Author: Ernesto Rodriguez


import Prelude
import Tersus.Cluster.Types
import Control.Concurrent.STM.TChan
import Control.Concurrent.STM.TVar
import Control.Concurrent.STM (atomically)
-- import Model (AppInstance)
import Remote.Process (ProcessM,forkProcess)
import Remote (receiveChannel,newChannel,sendChannel)
import Control.Monad.IO.Class
-- import Control.Monad ()
import Control.Monad (forever,mapM_)
import Model (AppInstance,MessageResult(Delivered,ENoAppInstance),getAppInstance,Address(Address),TApplication,User,TMessage,getSendAppInstance)
import Tersus.Cluster.MessageBackend (sendNotifications)
import Remote (ReceivePort)
import Data.Typeable.Internal (Typeable)
import Data.Binary (Binary)
import qualified Database.Persist.Store
import Database.Persist.GenericSql (SqlPersist)
import Settings (PersistConfig)
import Tersus.Global
import Yesod.Default.Config (withYamlEnvironment)
import Control.Monad.Maybe
import Database.Persist (getBy,selectList,get)

-- Represents a tersus server side app.
data TersusServerApp = TersusServerApp
    {
      -- The Tersus Application that represents this app. This is here for messaging purposes
      -- so normal tersus apps can communicate with it as if it were a tersus app.
      applicationRep :: TApplication,
      -- The user that is `running` the app. It's just here because it's easier to integrate
      -- it to the current app implementation
      userRep :: User,
      -- The function that is executed every time a message is received by the application
      msgFun :: TMessage -> TersusServiceM (),
      -- A optional function that is executed every time a message delivery status is
      -- executed
      ackwFun :: Maybe (MessageResultEnvelope -> TersusServiceM ())
    }

-- The datatype that contains all necesary data to run a Tersus Server application
-- this datatype also represents the `state` of the application.
data TersusService = TersusService 
    {
      -- The channel which the app uses to deliver messages. The server side
      -- apps use the same channel as the client side apps running in this server
      deliveryChannel :: TMessageQueue,
      -- The port to receive messages and the port other applications use to
      -- to send messages to this app
      msgChannel :: MessagingPorts,
      -- The channel where message acknowlegements are received
      ackwChannel :: AcknowledgementPorts,
      -- App instance that represents this server side app while it runs
      appInstance :: AppInstance,
      -- List of Tersus clusters. Theese are the servers to which acknowledgement
      -- is sent when the app is created
      clusterList :: TersusClusterList,
      -- Database configuration
      dbConf :: (PersistConfig,Database.Persist.Store.PersistConfigPool PersistConfig)
    }

-- The monad on which a server side application runs. It passes a TersusService datatype
-- to represent the state and eventually collapses to a ProcessM since they are intended to
-- be run with cloud haskell
data TersusServiceM a = TersusServiceM {runTersusServiceM :: TersusService -> ProcessM (TersusService,a)}

instance Monad TersusServiceM where
    -- Apply runTersusServiceM to the first argument which is a monad to obtain a function that
    -- goes from TersusService -> ProcessM. Apply this function to the TersusService that will
    -- be provided as ts and pind the resulting ProcessM to the new state and the result of the
    -- monad. Apply k to the result to get a new TersusServiceM and use the runTersusServiceM
    -- function applied to the result of applying k and the new TersusService state ts'
    m >>= k = TersusServiceM (\ts -> (runTersusServiceM m) ts >>= \(ts',a) -> runTersusServiceM (k a) ts')    
    return a = TersusServiceM $ (\ts -> return (ts,a))

-- Use the liftIO function of ProcessM to lift an IO monad into the
-- ProcessM monad, then the resulting value is binded and the provided
-- TersusService state ts is coupled with the value to get back into
-- the TersusServiceM
instance MonadIO TersusServiceM where
    liftIO ioVal = TersusServiceM $ \ts -> (liftIO ioVal >>= \x -> return (ts,x))


mkDbConf :: TersusEnvoiernment -> ProcessM (PersistConfig,Database.Persist.Store.PersistConfigPool PersistConfig)
mkDbConf tersusEnv = liftIO $ do
  dbConn <- withYamlEnvironment databaseYaml tersusEnv Database.Persist.Store.loadConfig >>= Database.Persist.Store.applyEnv :: IO PersistConfig
  poolConf <- Database.Persist.Store.createPoolConfig dbConn
  return (dbConn,poolConf)
  

runQuery query = TersusServiceM $ runQuery' query

runQuery' :: SqlPersist IO a -> TersusService -> ProcessM (TersusService,a)
runQuery' query (TersusService sDeliveryChannel (mSendPort,mRecvPort) aPorts appInstance' sClusterList (dbConn,poolConf)) = do 
  res <- runQuery'' dbConn poolConf query
  return (TersusService sDeliveryChannel (mSendPort,mRecvPort) aPorts appInstance' sClusterList (dbConn,poolConf),res)

runQuery'' :: PersistConfig -> Database.Persist.Store.PersistConfigPool PersistConfig -> SqlPersist IO a -> ProcessM a
runQuery'' dbConn poolConf query = liftIO $ Database.Persist.Store.runPool dbConn query poolConf

-- Run a TersusServiceM with the given TersusService state ts. Usually the initial
-- state which are all the messaging pipeings as defined in the datatype
evalTersusServiceM :: TersusService -> TersusServiceM a -> ProcessM a
evalTersusServiceM ts (TersusServiceM service) = service ts >>= \(_,a) -> return a

recvListenerFun :: (Data.Typeable.Internal.Typeable a,Data.Binary.Binary a) => TersusService -> ReceivePort a -> [(a -> TersusServiceM ())] -> ProcessM ()
recvListenerFun ts recvPort funs = forever $ do 
                                     recvVal <- receiveChannel recvPort                                       
                                     mapM_ (\f -> evalTersusServiceM ts (f recvVal)) funs

makeTersusService :: TersusServerApp -> (TMessageQueue -> TersusClusterList -> TersusEnvoiernment -> ProcessM ())
makeTersusService (TersusServerApp aRep uRep mFun aFun) = makeTersusService' serviceAppInstance mFun aFun
    where
      serviceAppInstance = getAppInstance $ Address uRep aRep

makeTersusService' :: AppInstance -> (TMessage -> TersusServiceM ()) -> Maybe (MessageResultEnvelope -> TersusServiceM ()) -> TMessageQueue -> TersusClusterList -> TersusEnvoiernment -> ProcessM ()
makeTersusService' sAppInstance mFun aFun sDeliveryChannel sClusterList tersusEnv = do
  (aSendPort,aRecvPort) <- newChannel
  (mSendPort,mRecvPort) <- newChannel
  --  liftIO $ atomically $ writeTChan nChannel (Initialized' appInstance)
  clusters <- liftIO $ atomically $ readTVar sClusterList 
  databaseConf <- mkDbConf tersusEnv
  let {ts = TersusService sDeliveryChannel (mSendPort,mRecvPort) (aSendPort,aRecvPort) sAppInstance sClusterList databaseConf;
       initNotification = [Initialized sAppInstance (mSendPort,"HashLoco")]}
  mapM_ (\c -> sendNotifications initNotification sClusterList c >> (liftIO $ putStrLn "sent stuff")) clusters
  -- The acknowledgement port is stripped from the envelope since acknowledgement is enforced
  _ <- forkProcess $ recvListenerFun ts mRecvPort [acknowledgeMsg,\(msg,_) -> mFun msg]
  case aFun of
    Just f -> recvListenerFun ts aRecvPort [f]
    Nothing -> recvListenerFun ts aRecvPort [defaultAckwFun]

-- Default acknowledgement function ignores the message
-- acknowledgement
defaultAckwFun :: MessageResultEnvelope -> TersusServiceM ()
defaultAckwFun _ = return ()

-- notifyCreateProcess' (TersusService nChannel deliveryChannel mPorts aPorts appInstance clusterList) = do
--   liftIO $ atomically $ writeTChan nChannel (Initialized' appInstance)
--   return (TersusService nChannel deliveryChannel mPorts aPorts appInstance clusterList,())
  

-- notifyCreateProcess = TersusServiceM $ notifyCreateProcess'

-- getMessages' (TersusService nChannel deliveryChannel mailBox (sChan,rChan) appInstance) = do
--   msgs <- liftIO $ atomically $ readTVar mailBox
--   return ((TersusService nChannel deliveryChannel mailBox (sChan,rChan) appInstance),msgs)

-- getMessages = TersusServiceM getMessages'

getMessage' :: TersusService -> ProcessM (TersusService,TMessageEnvelope)
getMessage' (TersusService sDeliveryChannel (mSendPort,mRecvPort) aPorts appInstance' sClusterList databaseConfig) = do
  msg <- receiveChannel mRecvPort
  return (TersusService sDeliveryChannel (mSendPort,mRecvPort) aPorts appInstance' sClusterList databaseConfig,msg)

-- Get the next message delivered to this particular server side application.
-- Ie. message located in the delivery channle created for this app
getMessage :: TersusServiceM (TMessageEnvelope)
getMessage = TersusServiceM getMessage'

sendMessage' :: TMessage -> TersusService -> ProcessM (TersusService,())
sendMessage' msg (TersusService sDeliveryChannel mPorts (aSendPort,aRecvPort) appInstance' sClusterList databaseConfig) = do
  liftIO $ atomically $ writeTChan sDeliveryChannel (msg,aSendPort)
  return (TersusService sDeliveryChannel mPorts (aSendPort,aRecvPort)  appInstance' sClusterList databaseConfig,())
  
-- Send a message from a server side application it uses the message queue
-- form the server where it's actually running
sendMessage :: TMessage -> TersusServiceM ()
sendMessage msg = TersusServiceM $ sendMessage' msg

acknowledgeMsg' :: TMessageEnvelope -> TersusService -> ProcessM (TersusService,())
acknowledgeMsg' (msg,aPort) ts = do
  sendChannel aPort (generateHash msg, Delivered , getSendAppInstance msg)
  return (ts,())

-- Acknowledge a received message as received and infomr the 
-- sender that the message was received
acknowledgeMsg :: TMessageEnvelope -> TersusServiceM ()
acknowledgeMsg msgEnv = TersusServiceM $ acknowledgeMsg' msgEnv

type MaybeQuery = MaybeT (SqlPersist IO)

maybeGetBy criterion = MaybeT $ getBy criterion

maybeGet id' = MaybeT $ get id'

maybeSelectList l1 l2 = MaybeT $ selectList l1 l2 >>= \res -> case res of
                                                                [] -> return Nothing
                                                                a -> return $ Just a