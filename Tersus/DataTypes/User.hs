module Tersus.DataTypes.User where

-- Text
import qualified Data.ByteString as BS
import           Data.ByteString hiding (putStrLn)
import qualified Data.ByteString.Char8 as Char8
import           Data.Text
import qualified Data.Text as T
import           Data.Text.Encoding

import           Database.Redis
import           Prelude
-- control
import           Control.Applicative
--import Yesod
import           Data.Aeson
import qualified Data.Aeson as J
-- Tersus
import           Data.Maybe(fromMaybe)
import           Tersus.Database
import           Tersus.DataTypes.TypeSynonyms
import           Tersus.DataTypes.TError
data User = User {
  uid :: UserId
  , email :: Email
  , nickname :: Username
  , password :: Maybe Text
  , isSuperAdmin :: Bool
  } deriving Show
  
type Email = Text
type Password = Text

instance ToJSON User where
    toJSON (User id email nickname _ _) = J.object [("email",J.String email),("username",J.String nickname)]

getUserByNickname :: Connection -> Username -> IO (Either TError User)
getUserByNickname conn username = do
  uid <- getUserId conn username
  case uid of
    Left e -> return . Left $ e
    Right uid' -> getUser conn uid'
  
-- | Get an user id from a nickname
getUserId :: Connection -> Username -> IO (Either TError UserId)
getUserId conn username  = runRedis conn $ do
  redisReply <- get $ "user" .> encodeUtf8 username <. "id"
  return $ case redisReply of
    Right maybeUID -> maybe (Left . TUserNicknameNotFound $ username) (Right . byteStringToInteger) maybeUID
    Left reply -> Left . RedisTError . T.pack . show $ reply


-- | Inserts a username to the database
insertNewUser :: Email -> Username -> Maybe Password -> Bool -> Connection -> IO (Maybe UserId)
insertNewUser email username maybePassword isSuperAdmin conn = runRedis conn $ do
  userID <- incr "users:max_id"
  case userID of
    Left _ -> return Nothing
    Right userid -> let
      uid = integerToByteString userid
      in do
        set ("user" .> encodeUtf8 username <. "id") uid
        set ("user" .> uid <. "email") (encodeUtf8 email)
        set ("user" .> uid <. "nickname") (encodeUtf8 username)
        set ("user" .> uid <. "password") (encodeUtf8 $ fromMaybe "" maybePassword)
        set ("user" .> uid <. "isadmin") (Char8.pack $ if isSuperAdmin then "1" else "")
        return . Just . byteStringToInteger $ uid
  
getUser :: Connection -> UserId ->  IO (Either TError User)
getUser conn userID = runRedis conn $ do
  let uid = integerToByteString userID
  user <- multiExec $ do
    email <- get $ "user" .> uid <. "email"
    nickname <- get $ "user" .> uid <. "nickname" 
    password <- get $ "user" .> uid <. "password" 
    isSuperAdmin <- get $ "user" .> uid <. "isadmin"
    return $ userFromRedis uid <$> email <*> nickname <*> password <*> isSuperAdmin 
  return $ case user of
    TxSuccess a -> case a of
      Just u -> Right u
      Nothing -> Left . TUserIdNotFound . byteStringToInteger $ uid
    TxAborted -> Left . RedisTError $ "TxAborted"
    TxError msg -> Left . RedisTError . T.pack $ msg
      
  
userFromRedis :: ByteString -> MaybeBS -> MaybeBS -> MaybeBS -> MaybeBS -> Maybe User
userFromRedis uid (Just e) (Just n) (Just p) (Just a) = Just $ User {
  uid = byteStringToInteger uid
  , email = decodeUtf8 e
  , nickname = decodeUtf8 n
  , password = if BS.null p then Nothing else Just . decodeUtf8 $ p
  , isSuperAdmin = if BS.null a then False else True
  }
userFromRedis _ _ _ _ _ = Nothing