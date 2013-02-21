module Tersus.DataTypes.User where

import qualified Data.ByteString.Char8 as BSChar8
import           Database.Redis
import           Prelude
import Data.Text.Encoding

import Data.Text
import Database.Redis
import Prelude
import Tersus.DataTypes.TypeSynonyms
import Tersus.DataTypes.TError
--import Yesod
import Data.Aeson
import qualified Data.Aeson as J

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
insertNewUser :: Email -> Username -> Maybe Password -> Bool -> Connection -> IO UserId
insertNewUser email username maybePassword isSuperAdmin conn = do
  runRedis conn $ do
    id <- return 1 -- user id 
    sadd "users" [BSChar8.pack . show $ id] -- Integers chars are a subset of ASCII
    return id
  
getUserById :: UserId -> Connection -> IO (Maybe User)
getUserById uid conn = runRedis conn $ do
  email <- get "users:id:email" 
  nickname <- get "users:id:nickname" 
  password <- get "users:id:password" 
  isSuperAdmin <- get "users:id:isSuperAdmin"   
  return Nothing --TODO
  