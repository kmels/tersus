module Tersus.DataTypes.User where

import qualified Data.ByteString.Char8 as Char8
import           Database.Redis
import           Prelude
import           Data.Text.Encoding

import           Data.Text
import           Database.Redis
import           Prelude
import           Tersus.DataTypes.TypeSynonyms
import           Tersus.DataTypes.TError
--import Yesod
import           Data.Aeson
import qualified Data.Aeson as J
-- Tersus
import           Data.Maybe(fromMaybe)
import           Tersus.Database
data User = User {
  uid :: UserId
  , email :: Email
  , nickname :: Username
  , password :: Maybe Text
  , isSuperAdmin :: Bool
  }
  
type Email = Text
type Password = Text

getUserByNickname :: Connection -> Username -> IO (Either TError User)
getUserByNickname conn userid = return . Left . TheImpossibleHappened $ "Not implemented yet"

instance ToJSON User where
    toJSON (User id email nickname _ _) = J.object [("email",J.String email),("username",J.String nickname)]


-- | Get an user id from a nickname
getUserIdByUsername :: Username -> Connection -> IO (Maybe UserId)
getUserIdByUsername user conn = do
  return Nothing

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
        set ("user" .> uid <. "isadmin") (Char8.pack $ if isSuperAdmin then "1" else "0")
        return . Just . byteStringToInteger $ uid
  
getUserById :: UserId -> Connection -> IO (Maybe User)
getUserById uid conn = runRedis conn $ do
  email <- get "users:id:email" 
  nickname <- get "users:id:nickname" 
  password <- get "users:id:password" 
  isSuperAdmin <- get "users:id:isSuperAdmin"   
  return Nothing --TODO
  