module Tersus.DataTypes.User where

import Data.Text
import Database.Redis
import Prelude
import Tersus.DataTypes.TypeSynonyms
import Tersus.DataTypes.TError
import Yesod
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
