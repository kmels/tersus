{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
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
import Control.Distributed.Process
import Control.Monad.IO.Class
-- import Control.Monad ()
import Control.Monad (forever)
import Tersus.Cluster.MessageBackend (sendNotifications)
import Data.Typeable.Internal (Typeable)
import Data.Binary (Binary)
import Tersus.Global
import Yesod.Default.Config (withYamlEnvironment)
import Control.Monad.Maybe
import Data.Acid (AcidState)
import Data.SafeCopy (SafeCopy)
import Control.Exception (Exception,throw)
import Data.Time.Clock (getCurrentTime,UTCTime)

-- tersus
import Tersus.DataTypes

-- | Exceptions that can result from server side applications
-- | NoStateException: raised if an application that dosen't use acid state tries to access it's acid state
data TersusServiceExceptions = NoStateException deriving (Show,Typeable)

instance Exception TersusServiceExceptions
  
-- | Represents a tersus server side app.
data (SafeCopy store) => TersusServerApp store = TersusServerApp
    {
      -- The Tersus Application that represents this app. This is here for messaging purposes
      -- so normal tersus apps can communicate with it as if it were a tersus app.
      applicationRep :: TApplication,
      -- The user that is `running` the app. It's just here because it's easier to integrate
      -- it to the current app implementation
      userRep :: User,
      -- The function that is executed every time a message is received by the application
      msgFun :: TMessage -> TersusServiceM store (),
      -- A optional function that is executed every time a message delivery status is
      -- obtained
      ackwFun :: Maybe (MessageResultEnvelope -> TersusServiceM store ()),
      -- Function that is called when the session of a AppInstance expires
      expireFun :: Maybe ([AppInstance] -> TersusServiceM store ()),
      -- The optional state that will be used by the application
      initialState :: Maybe (IO (AcidState store))
    }

-- The datatype that contains all necesary data to run a Tersus Server application
-- this datatype also represents the `state` of the application.
data (SafeCopy store) => TersusService store = TersusService 
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
      --dbConf :: (PersistConfig,Database.Persist.Store.PersistConfigPool PersistConfig),
      -- In memory database of the application
      memDb :: Maybe (AcidState store)
    }

-- The monad on which a server side application runs. It passes a TersusService datatype
-- to represent the state and eventually collapses to a Process since they are intended to
-- be run with cloud haskell
data TersusServiceM store a = TersusServiceM {runTersusServiceM :: TersusService store -> Process (TersusService store,a)}

instance Monad (TersusServiceM store) where
    -- Apply runTersusServiceM to the first argument which is a monad to obtain a function that
    -- goes from TersusService -> Process. Apply this function to the TersusService that will
    -- be provided as ts and pind the resulting Process to the new state and the result of the
    -- monad. Apply k to the result to get a new TersusServiceM and use the runTersusServiceM
    -- function applied to the result of applying k and the new TersusService state ts'
    m >>= k = TersusServiceM (\ts -> (runTersusServiceM m) ts >>= \(ts',a) -> runTersusServiceM (k a) ts')    
    return a = TersusServiceM $ (\ts -> return (ts,a))

-- Use the liftIO function of Process to lift an IO monad into the
-- Process monad, then the resulting value is binded and the provided
-- TersusService state ts is coupled with the value to get back into
-- the TersusServiceM
instance MonadIO (TersusServiceM store) where
    liftIO ioVal = TersusServiceM $ \ts -> (liftIO ioVal >>= \x -> return (ts,x))


{-mkDbConf :: TersusEnvoiernment -> Process (PersistConfig,Database.Persist.Store.PersistConfigPool PersistConfig)
mkDbConf tersusEnv = liftIO $ do
  dbConn <- withYamlEnvironment databaseYaml tersusEnv Database.Persist.Store.loadConfig >>= Database.Persist.Store.applyEnv :: IO PersistConfig
  poolConf <- Database.Persist.Store.createPoolConfig dbConn
  return (dbConn,poolConf)-}
  

{-runQuery :: (SafeCopy store) => forall a. SqlPersist IO a -> TersusServiceM store a

runQuery query = TersusServiceM $ runQuery' query

{-runQuery' :: (SafeCopy store) => SqlPersist IO a -> TersusService store -> Process (TersusService store,a)-}
runQuery' query (TersusService sDeliveryChannel (mSendPort,mRecvPort) aPorts appInstance' sClusterList (dbConn,poolConf) state) = do 
  res <- runQuery'' dbConn poolConf query
  return (TersusService sDeliveryChannel (mSendPort,mRecvPort) aPorts appInstance' sClusterList (dbConn,poolConf) state,res) -}

{-runQuery'' :: PersistConfig -> Database.Persist.Store.PersistConfigPool PersistConfig -> SqlPersist IO a -> Process a
runQuery'' dbConn poolConf query = liftIO $ Database.Persist.Store.runPool dbConn query poolConf -}

-- Run a TersusServiceM with the given TersusService state ts. Usually the initial
-- state which are all the messaging pipeings as defined in the datatype
evalTersusServiceM :: TersusService b -> TersusServiceM b a -> Process a
evalTersusServiceM ts (TersusServiceM service) = service ts >>= \(_,a) -> return a

recvListenerFun :: (Data.Typeable.Internal.Typeable a,Data.Binary.Binary a) => TersusService store -> ReceivePort a -> [(a -> TersusServiceM store ())] -> Process ()
recvListenerFun ts recvPort funs = forever $ do 
  recvVal <- receiveChan recvPort                                       
  mapM_ (\f -> evalTersusServiceM ts (f recvVal)) funs

-- | Initialize a process that runs the given server side application
makeTersusService :: (SafeCopy store) => TersusServerApp store -> TMessageQueue -> TersusClusterList -> TersusEnvoiernment -> Process ()
makeTersusService tersusServerApp sDeliveryChannel sClusterList tersusEnv = do
  liftIO $ putStrLn $ "TODO"
{-
  (aSendPort,aRecvPort) <- newChan
  (mSendPort,mRecvPort) <- newChan
  --  liftIO $ atomically $ writeTChan nChannel (Initialized' appInstance)
  clusters <- liftIO $ atomically $ readTVar sClusterList 
  databaseConf <- mkDbConf tersusEnv
  initializedDb <- stateInitializer
  let
    ts = TersusService sDeliveryChannel (mSendPort,mRecvPort) (aSendPort,aRecvPort) sAppInstance sClusterList databaseConf $ initializedDb
    initNotification = [Initialized sAppInstance (mSendPort,"HashLoco")]
  mapM_ (\c -> sendNotifications initNotification sClusterList c >> (liftIO $ putStrLn "sent stuff")) clusters
  -- The acknowledgement port is stripped from the envelope since acknowledgement is enforced
  _ <- spawnLocal $ recvListenerFun ts mRecvPort [acknowledgeMsg,\(msg,_) -> mFun msg]
  case aFun of
    Just f -> recvListenerFun ts aRecvPort [f]
    Nothing -> recvListenerFun ts aRecvPort [defaultAckwFun]

  where
    TersusServerApp aRep uRep mFun aFun _ _ = tersusServerApp
    sAppInstance = getAppInstance $ Address uRep aRep
    stateInitializer = case (initialState tersusServerApp) of
      Nothing -> return Nothing
      Just s -> liftIO s >>= return.Just -}

-- Default acknowledgement function ignores the message
-- acknowledgement
defaultAckwFun :: MessageResultEnvelope -> TersusServiceM store ()
defaultAckwFun _ = return ()

-- notifyCreateProcess' (TersusService nChannel deliveryChannel mPorts aPorts appInstance clusterList) = do
--   liftIO $ atomically $ writeTChan nChannel (Initialized' appInstance)
--   return (TersusService nChannel deliveryChannel mPorts aPorts appInstance clusterList,())
  

-- notifyCreateProcess = TersusServiceM $ notifyCreateProcess'

-- getMessages' (TersusService nChannel deliveryChannel mailBox (sChan,rChan) appInstance) = do
--   msgs <- liftIO $ atomically $ readTVar mailBox
--   return ((TersusService nChannel deliveryChannel mailBox (sChan,rChan) appInstance),msgs)

-- getMessages = TersusServiceM getMessages'

getMessage' :: (SafeCopy store) => TersusService store -> Process (TersusService store,TMessageEnvelope)
getMessage' (TersusService sDeliveryChannel (mSendPort,mRecvPort) aPorts appInstance' sClusterList state) = do
  msg <- receiveChan mRecvPort
  return (TersusService sDeliveryChannel (mSendPort,mRecvPort) aPorts appInstance' sClusterList state,msg)

-- Get the next message delivered to this particular server side application.
-- Ie. message located in the delivery channle created for this app
getMessage ::(SafeCopy store) => TersusServiceM store (TMessageEnvelope)
getMessage = TersusServiceM getMessage'

sendMessageInt :: (SafeCopy store) => TMessage -> TersusService store -> Process (TersusService store,())
sendMessageInt msg (TersusService sDeliveryChannel mPorts (aSendPort,aRecvPort) appInstance' sClusterList state) = do
  liftIO $ atomically $ writeTChan sDeliveryChannel (msg,aSendPort)
  return (TersusService sDeliveryChannel mPorts (aSendPort,aRecvPort)  appInstance' sClusterList state,())
  
-- Send a message from a server side application it uses the message queue
-- form the server where it's actually running
sendMessage :: (SafeCopy store) => TMessage -> TersusServiceM store ()
sendMessage msg = TersusServiceM $ sendMessageInt msg

-- | Send a message without timestamp, the timestamp will be added automatically
sendMessage' :: (SafeCopy store) => (UTCTime -> TMessage) -> TersusServiceM store ()
sendMessage' pMsg = do
  msg <- (liftIO getCurrentTime) >>= return.pMsg
  sendMessage msg

acknowledgeMsg' :: (SafeCopy store) => TMessageEnvelope -> TersusService store -> Process (TersusService store,())
acknowledgeMsg' (msg,aPort) ts = do
  sendChan aPort (generateHash msg, Delivered , getSendAppInstance msg)
  return (ts,())

-- | Get the acid state of the given Tersus Server Application
getDb :: (SafeCopy store) => TersusServiceM store (AcidState store)
getDb = TersusServiceM getDb'
  where
    getDb' tersusService = case memDb tersusService of
      Nothing -> throw NoStateException
      Just s -> return (tersusService,s)


-- Acknowledge a received message as received and infomr the 
-- sender that the message was received
acknowledgeMsg :: (SafeCopy store) => TMessageEnvelope -> TersusServiceM store ()
acknowledgeMsg msgEnv = TersusServiceM $ acknowledgeMsg' msgEnv

{- 
type MaybeQuery = MaybeT (SqlPersist IO)

{-maybeGetBy :: forall (m :: * -> *) val. (PersistEntity val,
                                         PersistUnique
                                         (PersistEntityBackend val) m) =>
              Unique val (PersistEntityBackend val)
              -> MaybeT
              (PersistEntityBackend val m) (Database.Persist.Store.Entity val)-}
maybeGetBy criterion = MaybeT $ getBy criterion

maybeGet :: forall a (backend :: (* -> *) -> * -> *) (m :: * -> *). (PersistEntity a, PersistStore backend m) => Key backend a -> MaybeT (backend m) a
maybeGet id' = MaybeT $ get id'

{-maybeSelectList :: forall val (m :: * -> *).
                   (PersistEntity val,
                    PersistQuery
                    (PersistEntityBackend val) m) =>
                   [Filter val]
                   -> [SelectOpt val]
                   -> MaybeT
                   (Database.Persist.Store.PersistEntityBackend val m)
                   [Database.Persist.Store.Entity val]-}
maybeSelectList l1 l2 = MaybeT $ selectList l1 l2 >>= \res -> case res of
                                                                [] -> return Nothing
                                                                a -> return $ Just a
-}
