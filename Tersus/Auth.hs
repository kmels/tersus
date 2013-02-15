module Tersus.Auth where

import Database.Redis
import Prelude
import Tersus.DataTypes
import Tersus.Users
import Yesod
import           Yesod.Auth

maybeLoggedUser :: (YesodAuth m, UserId ~ AuthId m) => Connection -> GHandler s m (Maybe User)
maybeLoggedUser conn = do
  maid <- maybeAuthId
  maybeUser <- liftIO $ runRedis conn $ do
    case maid of
      Nothing -> return Nothing
      Just uid -> do
        ma <- liftIO $ getUserById uid conn
        return ma
  return $ maybeUser