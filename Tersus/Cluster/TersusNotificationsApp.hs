-- | This module contains the Notifications App. This application allows Tersus applications
-- to create topics to which other applications can subscribe and all messages sent to
-- this application under a particular topic will be delivered to all applications
-- subscribed to that topic
module Tersus.Cluster.TersusNotificationsApp where

import           Prelude
import           System.IO.Unsafe (unsafePerformIO)
import           Data.SafeCopy
import           Data.IxSet
import qualified Data.IxSet as I
import           Data.Text hiding (foldl,empty)
import           Data.Time.Clock (getCurrentTime)
import           Data.Typeable.Internal (Typeable)
import qualified Control.Monad.State as S
import qualified Control.Monad.Reader as R
import           Data.Aeson (ToJSON,FromJSON,toJSON,parseJSON,object,(.:),(.:?),fromJSON)
import           Data.Acid
import qualified Data.Aeson.Types as A
import           Data.Acid.Advanced (update',query')
import           Control.Monad (mzero)
import           Tersus.Cluster.TersusService
import           Tersus.Global
import           Safe (readMay)

import           Control.Applicative
import           Prelude
import           Tersus.DataTypes.Messaging
import           Tersus.DataTypes.TApplication
import           Tersus.DataTypes.User
import           Tersus.Database(io)
import qualified Tersus.Global as Tersus.Global
-- | The name of the notifications application
tersusNotificationAppName :: Text
tersusNotificationAppName = "tersusNotifications"

-- | The user that runs the Notifications App
tersusNotificationsUser :: Text
tersusNotificationsUser = "tersus"

tersusNotificationsApp' :: TApplication
tersusNotificationsApp' = TApp 2 tersusNotificationAppName tersusNotificationAppName "This app provides notifications via messages of a variety of events" "http://tersusland.com/tersus" "neto@netowork.me" (unsafePerformIO getCurrentTime) "notificationsAppKey" [Tersus.Global.tersusUserId]

tersusServiceUser :: User
tersusServiceUser = User 0 "tersus@tersusland.com" tersusNotificationsUser (Just "") False

data TopicSubscription = TopicSubscription {subscriber :: AppInstance,topic :: Text} deriving (Typeable,Show,Eq,Ord)

-- | A datatype to index the topics in the topics database
newtype Topic = Topic Text deriving (Typeable,Ord,SafeCopy,Eq)

instance Indexable TopicSubscription where
  empty = ixSet [ixFun $ \ts -> [subscriber ts],
                 ixFun $ \ts -> [Topic $ topic ts]
                ]

-- | Datatype for the in memory database of topic subscribers
data TopicsDb = TopicsDb (IxSet TopicSubscription) deriving (Typeable)

-- | The possible requests that can be done to the Notifications app
-- Subscribe: subscribe to a topic
-- Unsubscribe: unsubscribe from a topic
-- Notify: send a message to all subscribers of a topic
data Action = Subscribe | Unsubscribe | Notify deriving (Show,Read)

-- | Datatype representing a service request from an app instance
-- to the notifications app
data Operation = Operation {action :: Either Action Text
                           ,topicStr :: Text
                           ,notificationText :: Maybe Text} deriving (Show,Read)

-- | Possible outcomes that can ocurr in the topics application.
-- Success indicates the operation completed successfully
-- InvalidFormat indicates that the request could not be parsed
-- UnrecognizedOperation indicates that the action in the message is not an action of the Notifications App.
data NResultCode = UnrecognizedOperation | Success | InvalidFormat deriving (Show,Read)

-- | The result of a request to the topics application. Indicates wether it was successful
-- or erroneous
data NResult = InvalidOperation Text | SuccessfulOperation Action | FormatError deriving (Show,Read)

instance FromJSON Action where
  parseJSON (A.String appAction) = case readMay.unpack $ appAction of
    Nothing -> mzero
    Just a -> return a
    
  parseJSON _ = mzero

instance FromJSON (Either Action Text) where
  parseJSON (A.String appAction) = return $ case fromJSON $ A.String appAction of
    A.Error _ -> Right $ appAction
    A.Success a -> Left a
  parseJSON _ = mzero

instance ToJSON Action where
  toJSON = A.String . pack . show

instance ToJSON NResultCode where
  toJSON = A.String . pack . show

-- | The JSON field where the result of a service of the
-- topics application is placed
resultTxt :: Text
resultTxt = "result"

-- | The json field for the operation
operationTxt :: Text
operationTxt = "operation"

instance ToJSON NResult where
  toJSON (InvalidOperation t) = object [(resultTxt,toJSON UnrecognizedOperation)
                                       ,(operationTxt,toJSON t)]
  toJSON (SuccessfulOperation a) = object [(resultTxt,toJSON Success)
                                          ,(operationTxt,toJSON a)]
  toJSON FormatError = object [(resultTxt,toJSON InvalidFormat)]
                                                 

instance ToJSON Operation where
  toJSON op = object allFields
    where
      mandatory = [("action",toJSON $ either toJSON toJSON $ action op),("topic",toJSON $ topicStr op)]
      allFields = case notificationText op of
        Nothing -> mandatory
        Just t -> mandatory ++ [("notification",toJSON t)]

instance FromJSON Operation where
  parseJSON (A.Object operation) = Operation <$>
                                 operation .: "action" <*>
                                 operation .: "topic" <*>
                                 operation .:? "notification"
  parseJSON _ = mzero

-- | Add a subscription to the topics database
addSubscription :: TopicSubscription -> Update TopicsDb ()
addSubscription selTopic = do
  TopicsDb db <- S.get
  S.put $ TopicsDb $ insert selTopic db

-- | Remove the given subscription from the topics database
rmSubscription :: TopicSubscription -> Update TopicsDb ()
rmSubscription selTopic = do
  TopicsDb db <- S.get
  S.put $ TopicsDb $ delete selTopic db

-- | Get all topics to which the given app instance is subscribed
getSubscriptions :: AppInstance -> Query TopicsDb (IxSet TopicSubscription)
getSubscriptions subAppInstance = do
  TopicsDb db <- R.ask
  return $ getEQ subAppInstance db

-- | Remove the given subscriptions from the subscription database
rmSubscriptions :: IxSet TopicSubscription -> Update TopicsDb ()
rmSubscriptions subscriptions = do
  TopicsDb db <- S.get
  let
    updatedDb = foldl (\x y->delete y x) db $ toList subscriptions
  S.put $ TopicsDb updatedDb

-- | Query the topics database for all subscribers of the given topic
getSubscribers :: Text -> Query TopicsDb (IxSet TopicSubscription)
getSubscribers selTopic = do
  TopicsDb db <- R.ask
  return $ getEQ (Topic selTopic) db    

$(deriveSafeCopy 0 'base ''TopicSubscription)
$(deriveSafeCopy 0 'base ''TopicsDb)
$(makeAcidic ''TopicsDb ['addSubscription,'rmSubscription,'getSubscriptions,'rmSubscriptions,'getSubscribers])

-- | Function that processes the incoming messages. The message can have a request
-- to subscribe to a topic, to unsubscribe to a topic or to send a message to everyone subscribed to
-- the topic                                                                                             
tersusNotificationsRecv :: TMessage -> TersusServiceM TopicsDb ()
tersusNotificationsRecv message = do
  io $ putStrLn $ show message
  case operation of
    Nothing -> sendResponse $ encodeAsText FormatError
    Just op -> runOperation op

  where
    TMessage uSender uReceiver aSender aReceiver msgBody _ = message
    operation = decodeFromText $ msgBody
    sendResponse txt = sendMessage' $ TMessage uReceiver uSender aReceiver aSender txt
    
    runOperation (Operation (Right s) _ _) = sendResponse $ encodeAsText $ InvalidOperation s
    runOperation (Operation (Left Subscribe) selTopic _) = do
      db <- getDb
      _ <- update' db $ AddSubscription $ TopicSubscription (getSendAppInstance message) selTopic
      sendResponse $ encodeAsText $  SuccessfulOperation Subscribe
    runOperation (Operation (Left Unsubscribe) selTopic _) = do
      db <- getDb
      _ <- update' db $ RmSubscription $ TopicSubscription (getSendAppInstance message) selTopic
      sendResponse $ encodeAsText $ SuccessfulOperation Unsubscribe
    runOperation (Operation (Left Notify) selTopic notification) = do
      db <- getDb
      subscribers <- query' db (GetSubscribers selTopic) >>= return.(delete $ TopicSubscription (getSendAppInstance message) selTopic)
      mapM_ notificationMsg $ toList subscribers
      sendResponse $ encodeAsText $ SuccessfulOperation Notify
      
      where
        notificationMsg (TopicSubscription s _) = do
          let
            AppInstance uSubs aSubs = s
            resp = case notification of
              Nothing -> ""
              Just n -> n
            msg = TMessage uSender uSubs aSender aSubs resp
          sendMessage' msg


-- | Notifications App. This app allows applications to create topics to which they can send messages and theese messages
-- will be delivered to every application that is subscribed to the topic
tersusNotificationsApp :: TersusServerApp TopicsDb
tersusNotificationsApp = TersusServerApp tersusNotificationsApp' tersusServiceUser tersusNotificationsRecv Nothing Nothing $ Just $ openLocalStateFrom "/tmp/TNA" $ TopicsDb $ I.empty

