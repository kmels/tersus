{-# LANGUAGE TemplateHaskell #-}
module Model where

import qualified Data.Binary            as B
import           Data.List              (find)
import           Data.Maybe             (fromJust)
import           Data.Text              (Text)
import qualified Data.Text              as T
import           Data.Text.Encoding     (encodeUtf8,decodeUtf8)
import           Data.Time              (UTCTime)
import           Data.Typeable.Internal (Typeable)
import           Database.Persist.Quasi
import           Database.Persist.Store (PersistValue (..), SqlType (..))
import           Prelude
import           Yesod
import Data.SafeCopy (deriveSafeCopy,base,SafeCopy)
import Data.IxSet (Indexable(..),ixSet,ixFun)
import Data.Data (Data)

-- You can define all of your database entities in the entities file.
-- You can find more information on persistent and how to declare entities
-- at:
-- http://www.yesodweb.com/book/persistent/
type Username = Text
type RawPath = Text
type Path = [Text]
type Id = Double
type IdList = [Id]
type ApplicationName = Text
type ApplicationKey = Text --private app key
type ApplicationIdentifier = Text --this has the property that has no spaces in it, it goes in the url.
type AccessKey = Text
type UrlK = Text
type Query = Text
type Message = String

data WriteMode = Override | AppendToFile | Create | Delete deriving (Show, Eq, Enum)

data ReadMode = FileMetadata | GetContent | Pagination deriving (Show, Eq, Enum)


data FileType = File | Directory deriving Show

instance PersistField FileType where
  toPersistValue = PersistText . T.pack . Prelude.show
  fromPersistValue (PersistText s) = case (T.unpack s) of
    "File" -> Right $ File
    "Directory" -> Right $ Directory
    _ -> Left $ "Expected File or Directory"
  fromPersistValue _ = Left $ "Expected PersistText as PersistValue for FileType"
  sqlType _ = SqlString

data TersusResultCode = Success | InexistentFile | NotEnoughPrivileges | DirectoryNotEmpty | OutOfRange deriving (Show, Eq)

data TRequestError = TRequestError TersusResultCode Message
data TRequestResponse = TRequestResponse TersusResultCode Message

data TersusResult = TersusResult Int TersusResultCode

-- Update the function msgResultNums as well since it's used to
-- convert the result into binary data
data MessageResult = Delivered | ENoAppInstance | EInvalidAppKey | EBufferFull | EInvalidHashCode | InvalidMsgFormat | MsgTimeout  deriving (Show, Eq, Enum, Typeable)


-- Mapping from MessageResults to integers so they can be
-- serealized and sent through cloudhaskell
msgResultsNums :: [(Int,MessageResult)]
msgResultsNums = [(1,Delivered),(2,ENoAppInstance),(3,EInvalidAppKey),(4,EBufferFull),(5,EInvalidHashCode),(6,InvalidMsgFormat)]

share [mkSave "myDefs", mkPersist sqlSettings, mkMigrate "migrateAll"] $(persistFileWith lowerCaseSettings "config/models")

-- | Represents a app being run by a user
data AppInstance = AppInstance {username :: Text, application :: Text} deriving (Eq,Typeable,Show)

appInstanceAsText :: AppInstance -> Text
appInstanceAsText (AppInstance u a) = T.concat [u,a]

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

-- | A message with the structure it has when delivered to an application
data TMessage = TMessage {messageSender ::Username
                         ,messageReceiver :: Username
                         ,messageSenderApp :: ApplicationIdentifier
                         ,messageReceiverApp :: ApplicationIdentifier
                         ,messageBody :: Text
                         ,messageTimestamp :: UTCTime} deriving (Eq, Typeable, Show)

-- | A message sent by an application using it's appkey. This is what the post request accepts
data AuthMessage = AuthMessage AccessKey Username ApplicationIdentifier Text deriving (Eq, Typeable,Show)

-- | Datatype that represents opening and closing of an app instance
data AppInstanceActions = Init AppInstance | Terminate AppInstance

-- | Typeclass that represents the datatypes that can be used to address a User/App instance
-- basically getAppInstance is a function that given a object, it generates an addres
-- which is used as index to obtain it's mailbox from the MailBox table
class Addressable a where
      getAppInstance :: a -> AppInstance

class InvAddressable a where
    getSendAppInstance :: a -> AppInstance

instance InvAddressable TMessage where
         getSendAppInstance (TMessage mSender _ appId _ _ _) = AppInstance  mSender appId

instance Addressable TMessage where
         getAppInstance (TMessage _ mReceiver _ appId _ _) = AppInstance mReceiver appId

instance Addressable AppInstance where
         getAppInstance a = a

instance Addressable Address where
    getAppInstance (Address (User _ nickname _ _) (TApplication _ id' _ _ _ _ _)) = AppInstance nickname id'

instance B.Binary Text where
         put t = B.put (T.unpack t)
         get = B.get >>=  return . T.pack

instance B.Binary User where
         put (User email nickname _ _) = B.put (T.unpack email,T.unpack nickname)
         get = do
             (email,nickname) <- B.get
             return $ User email (T.pack nickname) Nothing False

instance B.Binary TApplication where
         put (TApplication name id' desc _ email date appKey) = B.put (name,id',desc,email,show date,appKey)

         get = do
             (name,id',desc,email,date,appKey) <- B.get
             return $ TApplication name id' desc "**TODO**:repo" email (read date :: UTCTime) appKey

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

-- Utility functions to convert message results into binary data

getObjNum :: Eq a => [(Int,a)] -> a -> Int
getObjNum objs obj = let (n,_) = fromJust
                                 $ find (\(_,obj') -> obj' == obj) objs
                     in
                       n

getNumObj :: Eq a => [(Int,a)] -> Int -> a
getNumObj objs num = let (_,obj) = fromJust
                                   $ find (\(num',_) -> num' == num) objs
                     in
                       obj


getMsgResultNum :: MessageResult -> Int
getMsgResultNum = getObjNum msgResultsNums

getNumMsgResult :: Int -> MessageResult
getNumMsgResult = getNumObj msgResultsNums


instance B.Binary MessageResult where
         put msg = B.put $ getMsgResultNum msg
         get = B.get >>= \num -> return $ getNumMsgResult num

instance B.Binary AppInstance where
    put (AppInstance username' app') = B.put (encodeUtf8 username',encodeUtf8 app')
    get = do B.get >>= \(username',app') -> return (AppInstance (decodeUtf8 username') (decodeUtf8 app'))

instance B.Binary UTCTime where
  put time = B.put (show time)
  get = B.get >>= return.read

$(deriveSafeCopy 0 'base ''AppInstance)
