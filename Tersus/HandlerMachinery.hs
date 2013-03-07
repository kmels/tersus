module Tersus.HandlerMachinery(
  getConn,
  module Tersus.Auth,
  module Tersus.DataTypes,
  module Tersus.AccessKeys,
  module Tersus.Responses,
  module Yesod.Json
) where

import Database.Redis
import Import
import Tersus.AccessKeys
import Tersus.Auth
import Tersus.DataTypes
import Tersus.Responses
import Yesod.Json
getConn :: GHandler s Tersus Connection
getConn = do
  m <- getYesod
  return (redisConnection m)
