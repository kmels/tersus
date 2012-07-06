{-# Language TemplateHaskell #-}
module Model where

import           Prelude
import           Yesod
import qualified Data.Text as T
import           Data.Text(Text)

import           Database.Persist.Quasi

import           Database.Persist.Store(PersistValue(..),SqlType(..))
import           Database.Persist.GenericSql.Raw (SqlPersist)

import           Data.Time(UTCTime)
import qualified Data.Binary as B
import Data.Typeable.Internal (Typeable)
import Data.List (find)
import Data.Maybe (fromJust)
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
type AccessToken = Text

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

data TersusResultCode = Success | InexistentFile | NotEnoughPrivileges | DirectoryNotEmpty | OutOfRange deriving (Show, Eq, Enum)

data TersusResult = TersusResult Int TersusResultCode

-- Update the function msgResultNums as well since it's used to 
-- convert the result into binary data
data MessageResult = Delivered | ENoAppInstance  deriving (Show, Eq, Enum, Typeable)
msgResultsNums = [(1,Delivered),(2,ENoAppInstance)]

share [mkSave "myDefs", mkPersist sqlSettings, mkMigrate "migrateAll"] $(persistFileWith lowerCaseSettings "config/models") 

-- Represents a app being run by a user
data AppInstance = AppInstance {username :: String, application :: String} deriving (Eq,Typeable)

-- Message = { userSender: User, usersReceiver: [User], appSender: Application, appReceiver: Application, content: String}
data TMessage = TMessage {userSender :: User, 
                         userReciever :: User, 
                         appSender :: TApplication, 
                         appReciever :: TApplication,
                         content :: Text, 
                         time :: UTCTime} deriving (Eq, Typeable)

-- Represents a mailbox address, composed from a user and an Application
data Address = Address {user :: User, app :: TApplication}

-- Datatype that represents opening and closing of an app instance
data AppInstanceActions = Init AppInstance | Terminate AppInstance

-- Typeclass that represents the datatypes that can be used to address a User/App instance
-- basically getAppInstance is a function that given a object, it generates an addres
-- which is used as index to obtain it's mailbox from the MailBox table
class Addressable a where
      getAppInstance :: a -> AppInstance

class InvAddressable a where
    getSendAppInstance :: a -> AppInstance

instance InvAddressable TMessage where
    getSendAppInstance (TMessage (User nickname _ _) _ (TApplication _ id _ _ _ _ _ ) _ _ _) = AppInstance (T.unpack nickname) (T.unpack id)

instance Addressable TMessage where
         getAppInstance (TMessage _ (User nickname _ _) _ (TApplication _ id _ _ _ _ _ ) _ _) = AppInstance (T.unpack nickname) (T.unpack id)

instance Addressable Address where
         getAppInstance (Address (User nickname _ _) (TApplication _ id _ _ _ _ _)) = AppInstance (T.unpack nickname) (T.unpack id)


instance B.Binary Text where
         put t = B.put (T.unpack t)
         get = B.get >>=  return . T.pack

instance B.Binary User where
         put (User nickname _ _) = B.put (T.unpack nickname)
         get = do 
             nickname <- B.get 
             return $ User (T.pack nickname) Nothing []

instance B.Binary TApplication where
         put (TApplication name id desc _ email date _) = B.put (name,id,desc,email,show date)

         get = do 
             (name,id,desc,email,date) <- B.get
             return $ TApplication name id desc Nothing email (read date :: UTCTime) ""

instance B.Binary TMessage where
         put (TMessage sender reciever appSender appReciever msg time) = B.put (sender
                                                                        ,reciever
                                                                        ,appSender
                                                                        ,appReciever
                                                                        ,msg
                                                                        ,time)

         get = do
             (u1,u2,app1,app2,msg,time) <- B.get
             return $ TMessage u1 u2 app1 app2 msg time

instance B.Binary UTCTime where
         put time = B.put $ show time
         get = do B.get >>= return . read

-- Utility functions to convert message results into binary data
getMsgResultNum :: MessageResult -> Int
getMsgResultNum msg = let (n,_) = fromJust $ find (\(_,msg') -> msg' == msg) msgResultsNums
                      in
                        n
                      
getNumMsgResult :: Int -> MessageResult
getNumMsgResult num = let (_,m) = fromJust $ find (\(num',_) -> num' == num) msgResultsNums
                      in
                        m

instance B.Binary MessageResult where
         put msg = B.put $ getMsgResultNum msg
         get = B.get >>= \num -> return $ getNumMsgResult num

instance B.Binary AppInstance where
    put (AppInstance user app) = B.put (user,app)
    get = do B.get >>= \(user,app) -> return (AppInstance user app)