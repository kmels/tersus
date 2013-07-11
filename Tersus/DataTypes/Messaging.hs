{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE OverloadedStrings #-}

module Tersus.DataTypes.Messaging where

import           Control.Applicative
import           Control.Monad (mzero)
import           Data.Aeson as J
import qualified Data.Binary            as B
import           Data.ByteString        as BS
import           Data.Data (Data)
import           Data.List
import           Data.Maybe(fromJust)
import           Data.Text
import qualified Data.Text as T
import           Data.Time.Clock
import           Data.Typeable
import           Prelude
import           Tersus.DataTypes.TApplication
import           Tersus.DataTypes.TypeSynonyms
import           Tersus.DataTypes.User
import           Data.Hashable (Hashable,hash)
import           Data.Text.Encoding (encodeUtf8)

-- | Represents a app being run by a user
data AppInstance = AppInstance {
  username :: Text
  , application :: Text
  } deriving (Eq,Typeable,Show)
  

instance ToJSON AppInstance where
  toJSON a = J.object [("username",toJSON $ username a)
                       ,("application",toJSON $ application a)]

instance Ord AppInstance where
  compare a a' = compare (appInstanceAsText a) (appInstanceAsText a')
  a < a' = (<) (appInstanceAsText a) (appInstanceAsText a')
  a <= a' = (<=) (appInstanceAsText a) (appInstanceAsText a')
  a > a' = (>) (appInstanceAsText a) (appInstanceAsText a')
  a >= a' = (>=) (appInstanceAsText a) (appInstanceAsText a')
  max a a' | a > a' = a
           | otherwise = a'
  min a a' | a < a' = a
           | otherwise = a'
           

-- | Typeclass that represents the datatypes that can be used to address a User/App instance
-- basically getAppInstance is a function that given a object, it generates an addres
-- which is used as index to obtain it's mailbox from the MailBox table
class Addressable a where
      getAppInstance :: a -> AppInstance
      
class InvAddressable a where
    getSendAppInstance :: a -> AppInstance
    
    
instance Hashable AppInstance where
  hash a = hash $ encodeUtf8 $ appInstanceAsText a

appInstanceAsText :: AppInstance -> Text
appInstanceAsText (AppInstance u a) = T.concat [u,a]

instance B.Binary AppInstance where
    put (AppInstance username' app') = B.put (username',app')
    get = B.get >>= \(username,app) -> return $ AppInstance username app
    
instance Addressable AppInstance where
         getAppInstance a = a
         

-- | The responses that Tersus will send for the messaging api
data TMessageResponse = MsgDelivered String | MsgQueued String | InvalidAppKey AccessKey | NoAppInstance AppInstance | InvalidFormat Text deriving (Show)

instance ToJSON TMessageResponse where
  toJSON (MsgDelivered code) = J.object [("result","Delivered"),("hashCode",toJSON code)]
  toJSON (MsgQueued code) = J.object [("result","Queued"),("hashCode",toJSON code)]
  toJSON (InvalidAppKey accessKey) = J.object [("result","InvalidAppKey"),("appKey",toJSON accessKey)]
  toJSON (NoAppInstance appInstance) = J.object [("result","NoAppInstance"),("appInstance",toJSON appInstance)]
  toJSON (InvalidFormat errorMsg) = J.object [("result","InvalidFormat"),("reason",toJSON errorMsg)]

-- | A message with the structure it has when delivered to an application
data TMessage = TMessage {
  messageSender :: Username
  ,messageReceiver :: Username
  ,messageSenderApp :: ApplicationIdentifier
  ,messageReceiverApp :: ApplicationIdentifier
  ,messageBody :: Text
  ,messageTimestamp :: UTCTime} deriving (Eq, Typeable, Show)
  
-- Instance to convert a message into it's json representation, this instance is defined
-- according to the Tersus Developers Api.
instance ToJSON TMessage where
         toJSON (TMessage sender receiver senderApp receiverApp msgBody _) = J.object [
           ("userSender",toJSON sender),
           ("userReceiver",toJSON receiver),
           ("appSender",toJSON senderApp),
           ("appReceiver",toJSON receiverApp),
           ("content",toJSON msgBody)
           ]


instance B.Binary TMessage where
  put (TMessage sender receiver appSender' appReceiver msg sendTime) = B.put (sender
                                                                                ,receiver
                                                                                ,appSender'
                                                                                ,appReceiver
                                                                                ,msg
                                                                                ,sendTime)
  get = do
    (u1,u2,app1,app2,msg,sendTime) <- B.get
    return $ TMessage u1 u2 app1 app2 msg sendTime
    
instance B.Binary UTCTime where
  put time = B.put (show time)
  get = B.get >>= return.read
  
instance Addressable TMessage where
         getAppInstance (TMessage _ mReceiver _ appId _ _) = AppInstance mReceiver appId
         
instance InvAddressable TMessage where
         getSendAppInstance (TMessage mSender _ appId _ _ _) = AppInstance mSender appId
         


-- Update the function msgResultNums as well since it's used to
-- convert the result into binary data
data MessageResult = Delivered | ENoAppInstance | EInvalidAppKey | EBufferFull | EInvalidHashCode | InvalidMsgFormat | MsgTimeout  deriving (Show, Eq, Enum, Typeable)


-- Mapping from MessageResults to integers so they can be
-- serealized and sent through cloudhaskell
msgResultsNums :: [(Int,MessageResult)]
msgResultsNums = [(1,Delivered),(2,ENoAppInstance),(3,EInvalidAppKey),(4,EBufferFull),(5,EInvalidHashCode),(6,InvalidMsgFormat)]


newtype AUserId = AUserId Text deriving (Eq,Ord,Data,Typeable)--,SafeCopy)
newtype AAppId = AAppId Text deriving (Eq,Ord,Data,Typeable)--,SafeCopy)

-- | Represents a mailbox address of an application instance
-- mostly used by server side apps
data Address = Address {user :: User, app :: TApplication}

                                         
-- | A message sent by an application using it's appkey. This is what the post request accepts
data AuthMessage = AuthMessage AccessKey Username ApplicationIdentifier Text deriving (Eq, Typeable,Show)

-- | Datatype that represents opening and closing of an app instance
data AppInstanceActions = Init AppInstance | Terminate AppInstance



instance B.Binary User where
         put (User id email nickname _ _) = B.put (email,nickname)
         get = do
             (id,email,nickname) <- B.get
             return $ User id email nickname Nothing False



-- Utility functions to convert message results into binary data

getObjNum :: Eq a => [(Int,a)] -> a -> Int
getObjNum objs obj = let (n,_) = fromJust
                                 $ Data.List.find (\(_,obj') -> obj' == obj) objs
                     in
                       n

getNumObj :: Eq a => [(Int,a)] -> Int -> a
getNumObj objs num = let (_,obj) = fromJust
                                   $ Data.List.find (\(num',_) -> num' == num) objs
                     in
                       obj


getMsgResultNum :: MessageResult -> Int
getMsgResultNum = getObjNum msgResultsNums

getNumMsgResult :: Int -> MessageResult
getNumMsgResult = getNumObj msgResultsNums


instance B.Binary MessageResult where
         put msg = B.put $ getMsgResultNum msg
         get = B.get >>= \num -> return $ getNumMsgResult num

-- $(deriveSafeCopy 0 'base ''AppInstance)


instance FromJSON AuthMessage where
  parseJSON (Object authMessage) = AuthMessage <$>
                                   authMessage .: "senderAppKey" <*>
                                   authMessage .: "userReceiver" <*>
                                   authMessage .: "appReceiver"  <*>
                                   authMessage .: "content"
  parseJSON _ = mzero
                                   
