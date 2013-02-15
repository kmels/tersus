module Tersus.DataTypes.Messaging.Util where

import qualified Data.Binary            as B
import           Data.Text
import qualified Data.Text as T
import           Prelude
import           Tersus.DataTypes.User
instance B.Binary Text where
         put t = B.put (T.unpack t)
         get = B.get >>=  return . T.pack
