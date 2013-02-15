module Tersus.DataTypes.Messaging where

import qualified Data.Binary            as B
import           Data.ByteString
import qualified Data.ByteString  as BS
import           Data.Data (Data)
import           Data.IxSet (Indexable(..),ixSet,ixFun)
import           Data.List
import           Data.Maybe(fromJust)
import           Data.SafeCopy (deriveSafeCopy,base,SafeCopy)
import           qualified Data.Text as T
import           Data.Text
import           Data.Time.Clock
import           Data.Typeable
import           Prelude
import           Tersus.DataTypes.User
import           Tersus.DataTypes.TypeSynonyms
import           Tersus.DataTypes.TApplication

-- | Represents a app being run by a user
data AppInstance = AppInstance {
  username :: Text
  , application :: Text
  } deriving (Eq,Typeable,Show)
  

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
    

appInstanceAsText :: AppInstance -> Text
appInstanceAsText (AppInstance u a) = T.concat [u,a]

instance B.Binary AppInstance where
    put (AppInstance username' app') = B.put (username',app')
    get = do B.get >>= \(username,app) -> return $ AppInstance username app
    
instance Addressable AppInstance where
         getAppInstance a = a
         

-- | A message with the structure it has when delivered to an application
data TMessage = TMessage {
  messageSender :: Username
  ,messageReceiver :: Username
  ,messageSenderApp :: ApplicationIdentifier
  ,messageReceiverApp :: ApplicationIdentifier
  ,messageBody :: Text
  ,messageTimestamp :: UTCTime} deriving (Eq, Typeable)
  
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


newtype AUserId = AUserId Text deriving (Eq,Ord,Data,Typeable,SafeCopy)
newtype AAppId = AAppId Text deriving (Eq,Ord,Data,Typeable,SafeCopy)

instance Indexable AppInstance where
  empty = ixSet [ ixFun $ \ai -> [AUserId $ username ai]
                ,ixFun $ \ai -> [AAppId $ application ai]
                ,ixFun $ \ai -> [ai]
                ]

-- | Represents a mailbox address of an application instance
-- mostly used by server side apps
data Address = Address {user :: User, app :: TApplication}

                                         
-- | A message sent by an application using it's appkey. This is what the post request accepts
data AuthMessage = AuthMessage AccessKey Username ApplicationIdentifier ByteString deriving (Eq, Typeable,Show)

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

$(deriveSafeCopy 0 'base ''AppInstance)


