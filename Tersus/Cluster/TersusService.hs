{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
module Tersus.Cluster.TersusService where

-- | TersusService
-- Contains all the functions and datatypes that control a server side Tersus Application
-- Author: Ernesto Rodriguez


import Prelude
import Control.Distributed.Process
import Control.Monad.IO.Class
import Control.Monad (forever)
import Data.Typeable.Internal (Typeable)
import Tersus.Global
import Control.Monad.Maybe
import Control.Exception (Exception,throw)
import Data.Time.Clock (getCurrentTime,UTCTime)
import Database.Redis hiding (msgChannel)
import Data.Binary (Binary)

-- tersus
import Tersus.Cluster.Types
import Tersus.Cluster.MessageFrontend (broadcastNotificationsProcess)
import Tersus.DataTypes

-- | Exceptions that can result from server side applications
-- | NoStateException: raised if an application that dosen't use acid state tries to access it's acid state
data TersusServiceExceptions = NoStateException deriving (Show,Typeable)

instance Exception TersusServiceExceptions
  
-- | Represents a tersus server side app.
data TersusServerApp = TersusServerApp
    {
      -- | The Tersus Application that represents this app. This is here for messaging purposes
      -- so normal tersus apps can communicate with it as if it were a tersus app.
      applicationRep :: TApplication,
      -- | The user that is `running` the app. It's just here because it's easier to integrate
      -- it to the current app implementation
      userRep :: User,
      -- | The function that is executed every time a message is received by the application
      msgFun :: TMessage -> TersusServiceM (),
      -- | A optional function that is executed every time a message delivery status is
      -- obtained
      ackwFun :: Maybe (MessageResultEnvelope -> TersusServiceM ()),
      -- | Function that is called when the session of a AppInstance expires
      expireFun :: Maybe ([AppInstance] -> TersusServiceM ())
    }

-- | The datatype that contains all necesary data to run a Tersus Server application
-- this datatype also represents the `state` of the application.
data TersusService = TersusService 
    {
      -- | The port to receive messages and the port other applications use to
      -- to send messages to this app
      msgChannel :: MessagingPorts,
      -- | The channel where message acknowlegements are received
      ackwChannel :: AcknowledgementPorts,
        -- | The redis connection for the database used by this application
      serviceConnection :: Connection,
      -- | The list of Tersus servers
      tersusNode :: TersusClusterList,
      -- | The Tersus Application that represents this app. This is here for messaging purposes
      -- so normal tersus apps can communicate with it as if it were a tersus app.
      serviceApplication :: TApplication,
      -- | The channels that contain the send ports of the tersus applications
      serviceSendChannels :: SendAddressTable,
      -- | The channels that contain the receive ports of the tersus applications
      serviceRecvChannels :: RecvAddressTable
    }

-- | The monad on which a server side application runs. It passes a TersusService datatype
-- to represent the state and eventually collapses to a Process since they are intended to
-- be run with cloud haskell
data TersusServiceM a = TersusServiceM {runTersusServiceM :: TersusService -> Process (TersusService,a)}

instance Monad TersusServiceM  where
    -- | Apply runTersusServiceM to the first argument which is a monad to obtain a function that
    -- goes from TersusService -> Process. Apply this function to the TersusService that will
    -- be provided as ts and pind the resulting Process to the new state and the result of the
    -- monad. Apply k to the result to get a new TersusServiceM and use the runTersusServiceM
    -- function applied to the result of applying k and the new TersusService state ts'
    m >>= k = TersusServiceM (\ts -> (runTersusServiceM m) ts >>= \(ts',a) -> runTersusServiceM (k a) ts')    
    return a = TersusServiceM $ (\ts -> return (ts,a))

-- | Use the liftIO function of Process to lift an IO monad into the
-- Process monad, then the resulting value is binded and the provided
-- TersusService state ts is coupled with the value to get back into
-- the TersusServiceM
instance MonadIO TersusServiceM where
    liftIO ioVal = TersusServiceM $ \ts -> (liftIO ioVal >>= \x -> return (ts,x))

-- | Run a TersusServiceM with the given TersusService state ts. Usually the initial
-- state which are all the messaging pipeings as defined in the datatype
evalTersusServiceM :: TersusService -> TersusServiceM a -> Process (TersusService,a)
evalTersusServiceM ts (TersusServiceM service) = service ts >>= \a -> return a

recvListenerFun :: (Typeable a, Binary a) => TersusService -> ReceivePort a -> [(a -> TersusServiceM ())] -> Process ()
recvListenerFun ts recvPort funs = forever $ do 
  recvVal <- receiveChan recvPort
  mapM_ (\f -> evalTersusServiceM ts (f recvVal)) funs

-- | Initialize a process that runs the given server side application
makeTersusService :: TersusServerApp -> Connection -> TersusClusterList -> SendAddressTable -> RecvAddressTable -> Process (ProcessId)
makeTersusService tersusServerApp conn server sSendChannels sRecvChannels = do
  (aSendPort,aRecvPort) <- newChan
  (mSendPort,mRecvPort) <- newChan
  let
    serverApp = TersusService{
      msgChannel = (mSendPort,mRecvPort),
      ackwChannel = (aSendPort,aRecvPort),
      serviceApplication = applicationRep tersusServerApp,
      serviceConnection = conn,
      tersusNode = server,
      serviceSendChannels = sSendChannels,
      serviceRecvChannels = sRecvChannels
      }
    tAppInstance = AppInstance{
      username = nickname $ userRep tersusServerApp,
      application = identifier $ applicationRep tersusServerApp
      }
  broadcastNotificationsProcess [Initialized tAppInstance (mSendPort,aSendPort,"hashPelado")] server 
  spawnLocal $ recvListenerFun serverApp mRecvPort [recvFunction $ msgFun tersusServerApp]

recvFunction :: (TMessage -> TersusServiceM ()) -> TMessageEnvelope -> TersusServiceM ()
recvFunction f (msg,ackwPort) = do
  ts <- getTersusService
--  liftProcess $ sendChan ackwPort $ (
  f $ msg
    
getTersusService :: TersusServiceM (TersusService)
getTersusService = TersusServiceM $ \ts -> return (ts,ts)

-- Default acknowledgement function ignores the message
-- acknowledgement
-- defaultAckwFun :: MessageResultEnvelope -> TersusServiceM ()
-- defaultAckwFun _ = return ()

-- notifyCreateProcess' (TersusService nChannel deliveryChannel mPorts aPorts appInstance clusterList) = do
--   liftIO $ atomically $ writeTChan nChannel (Initialized' appInstance)
--   return (TersusService nChannel deliveryChannel mPorts aPorts appInstance clusterList,())
  

-- notifyCreateProcess = TersusServiceM $ notifyCreateProcess'

-- getMessages' (TersusService nChannel deliveryChannel mailBox (sChan,rChan) appInstance) = do
--   msgs <- liftIO $ atomically $ readTVar mailBox
--   return ((TersusService nChannel deliveryChannel mailBox (sChan,rChan) appInstance),msgs)

-- getMessages = TersusServiceM getMessages'

liftProcess :: Process a -> TersusServiceM a
liftProcess p = TersusServiceM $ \ts -> p >>= \r -> return (ts,r)



-- getMessage' :: TersusService -> Process (TersusService,TMessageEnvelope)
-- getMessage' (TersusService sDeliveryChannel (mSendPort,mRecvPort) aPorts appInstance' sClusterList state) = do
--   msg <- receiveChan mRecvPort
--   return (TersusService sDeliveryChannel (mSendPort,mRecvPort) aPorts appInstance' sClusterList state,msg)

-- Get the next message delivered to this particular server side application.
-- Ie. message located in the delivery channle created for this app
-- getMessage :: TersusServiceM (TMessageEnvelope)
-- getMessage = TersusServiceM getMessage'

-- sendMessageInt :: TMessage -> TersusService -> Process (TersusService,())
-- sendMessageInt msg (TersusService sDeliveryChannel mPorts (aSendPort,aRecvPort) appInstance' sClusterList state) = do
--   liftIO $ atomically $ writeTChan sDeliveryChannel (msg,aSendPort)
--   return (TersusService sDeliveryChannel mPorts (aSendPort,aRecvPort)  appInstance' sClusterList state,())
  
-- -- Send a message from a server side application it uses the message queue
-- -- form the server where it's actually running
-- sendMessage :: TMessage -> TersusServiceM ()
-- sendMessage msg = TersusServiceM $ sendMessageInt msg

-- -- | Send a message without timestamp, the timestamp will be added automatically
-- sendMessage' :: (UTCTime -> TMessage) -> TersusServiceM ()
-- sendMessage' pMsg = do
--   msg <- (liftIO getCurrentTime) >>= return.pMsg
--   sendMessage msg

-- acknowledgeMsg' :: TMessageEnvelope -> TersusService -> Process (TersusService,())
-- acknowledgeMsg' (msg,aPort) ts = do
--   sendChan aPort (generateHash msg, Delivered , getSendAppInstance msg)
--   return (ts,())

-- | Get the acid state of the given Tersus Server Application
-- getDb :: TersusServiceM  (AcidState store)
-- getDb = TersusServiceM getDb'
--   where
--     getDb' tersusService = case memDb tersusService of
--       Nothing -> throw NoStateException
--       Just s -> return (tersusService,s)


-- Acknowledge a received message as received and infomr the 
-- sender that the message was received
-- acknowledgeMsg :: (SafeCopy store) => TMessageEnvelope -> TersusServiceM store ()
-- acknowledgeMsg msgEnv = TersusServiceM $ acknowledgeMsg' msgEnv

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
