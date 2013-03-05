module Tersus.Auth where

import Database.Redis
import Prelude
import Tersus.DataTypes.User
import Tersus.DataTypes.TypeSynonyms
import Yesod
import           Yesod.Auth

maybeLoggedUser :: (YesodAuth m, UserId ~ AuthId m) => Connection -> GHandler s m (Maybe User)
maybeLoggedUser conn = do
  maid <- maybeAuthId
  maybeUser <- io $ runRedis conn $ do
    case maid of
      Nothing -> return Nothing
      Just uid -> do
        eTErrorTUser <- io $ getUser conn uid -- Either TError TUser
        case eTErrorTUser of
          Left msg -> do
            io . putStrLn $ show msg
            return Nothing
          Right u -> return . Just $ u
  io . putStrLn $ "Getting user ... " ++ show maybeUser
  return maybeUser
io = liftIO

requireLogin :: (YesodAuth m, UserId ~ AuthId m) => Connection -> GHandler s m User
requireLogin conn = maybeLoggedUser conn >>= maybe redirectLogin return

redirectLogin :: Yesod master => GHandler sub master a
redirectLogin = do
    y <- getYesod
    setUltDestCurrent
    case authRoute y of
        Just z -> redirect z
        Nothing -> permissionDenied "Please configure authRoute"
