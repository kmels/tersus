module Tersus.HandlerMachinery(
  getConn,
--  module Tersus.Auth,
  requireLogin,
  redirectLogin,
  maybeLoggedUser,
  module Tersus.DataTypes,
  module Tersus.AccessKeys,
  module Tersus.Responses,
  module Yesod.Json  
) where

import Database.Redis
import Import
import Tersus.AccessKeys
import qualified Tersus.Auth as Auth
import Tersus.DataTypes
import Tersus.Responses
import Yesod.Json

getConn :: GHandler s Tersus Connection
getConn = do
  m <- getYesod
  return (redisConnection m)

requireLogin :: GHandler s Tersus User
requireLogin = getConn >>= Auth.requireLogin

maybeLoggedUser :: GHandler s Tersus (Maybe User)
maybeLoggedUser = getConn >>= Auth.maybeLoggedUser

redirectLogin = Auth.redirectLogin
