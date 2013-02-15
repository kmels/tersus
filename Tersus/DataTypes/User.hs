module Tersus.DataTypes.User where


import Data.Text
import Database.Redis
import Prelude
import Tersus.DataTypes.TypeSynonyms
import Yesod
data User = User {
  id :: UserId
  , email :: Email
  , nickname :: Username
  , password :: Maybe Text
  , isSuperAdmin :: Bool
  }
  
type Email = Text
type Password = Text
