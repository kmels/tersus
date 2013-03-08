{-# LANGUAGE ImplicitParams #-}

module Tersus.DataTypes.TApplication where

import qualified Data.Binary            as B
import           Data.ByteString
import           Data.ByteString.Char8
import           Data.String
import           Data.Text
import           Data.Time              (UTCTime)
import           Data.Time.Clock
import           Prelude
import           Tersus.DataTypes.Messaging.Util

import Yesod.Handler
import           Control.Applicative
import           Control.Monad.IO.Class
import           Data.ByteString
import qualified Data.ByteString.Char8 as Char8
import           Data.Maybe
import qualified Data.Text as T
import           Data.Text.Encoding
import           Database.Redis
import           Prelude
import           Tersus.DataTypes.TError
import           Tersus.DataTypes.TypeSynonyms
import           Tersus.Database
import           Tersus.Debug
import           Tersus.Filesystem
import           Tersus.Responses
data TApplication = TApp { 
  -- identifier
  aid :: AppId
  -- name of the application
  , name :: Text
  -- url friendly identifier
  , identifier :: Text
  -- what does this app do?
  , description :: Text
  -- where does it live?
  , repositoryUrl :: Text
  , contactEmail :: Text
  -- when was it registered?
  , creationDate :: UTCTime
  -- application key to encrypt access keys
  , appKey :: Text
  -- a set of owner ids
  , owners :: [UserId]
  } 
  
instance Show TApplication where
  show = T.unpack . name
  
instance B.Binary TApplication where
         put (TApp aid name id' desc _ email date appKey owners) = B.put (aid, name,id',desc,email,show date,appKey,owners)

         get = do
             (aid, name,id',desc,email,date,appKey,owners) <- B.get
             return $ TApp aid name id' desc "**TODO**:repo" email (read date :: UTCTime) appKey owners

-- | Returns the first 10 applications ordered by id
getApplications :: Connection -> IO [TApplication]
getApplications conn = do
  debugM "Getting applications"
  app_ids <- runRedis conn $ zrange ("apps" <.> "ids") 0 10 
  debugM $ "Found: " ++ show app_ids
  either fail getTApplications app_ids
  where
    fail :: a -> IO [TApplication]
    fail _ = return []
    
    getTApplications :: [ByteString] -> IO [TApplication]
    getTApplications [] = return []
    getTApplications (aid:aids) = do
      tapp <- conn `getApplication` (byteStringToInteger aid)
      debugM $ "getTApplications " ++ show tapp
      case tapp of        
        Right tapp -> getTApplications aids >>= \tapps -> return $ tapp:tapps
        _ -> getTApplications aids    

getTApplicationByName :: Connection -> ApplicationIdentifier -> IO (Either TError TApplication)
getTApplicationByName conn appName = do
  appid <- getAppId conn appName
  either (return . fail) (getApplication conn) appid
  where
    fail terror = Left terror

deleteApplicationByName :: Connection -> ApplicationIdentifier -> IO (Either TError ())
deleteApplicationByName conn app_name = do
  app_id <- getAppId conn app_name
  either (return . Left) (deleteApplication conn app_name) app_id

requireTApplication :: Connection -> ApplicationIdentifier -> GHandler s m TApplication
requireTApplication conn app_name = do
  eTApp <- io $ getTApplicationByName conn app_name
  either returnTError return eTApp 
     
-- | Gets an application id given an application identifier.
getAppId :: Connection -> ApplicationIdentifier -> IO (Either TError AppId)
getAppId conn identifier = runRedis conn $ do
  aid <- get $ tAppId identifier
  io $ debugM $ "Looking identifier " ++ T.unpack identifier
  case aid of
    Left _ -> return . Left . RedisTError $ "getAppId"
    Right Nothing -> return . Left . TAppIdNotFound $ identifier
    Right (Just aid') -> return . Right . byteStringToInteger $ aid'

getApplication :: Connection -> AppId -> IO (Either TError TApplication)
getApplication conn tAppID = runRedis conn $ do
  let ?app_id = integerToByteString tAppID
  io $ debugM $ "Getting tAppId " ++ show tAppID
  tapp <- multiExec $ do -- Redis Monad
    name' <- gets tAppName
    identifier' <- gets tAppIdentifier
    desc' <- gets tAppDescription
    repo' <- gets tAppRepositoryUrl 
    email' <- gets tAppContactEmail
    creationDate' <- gets tAppCreationDate
    appkey' <- gets tAppKey
    owners' <- smembers $ tAppOwners ?app_id
    return $ tAppFromRedis ?app_id <$> name' <*> identifier' <*> desc' <*> repo' <*> email' <*> creationDate' <*> appkey' <*> owners'
  return $ case tapp of
    TxSuccess a -> case a of
      Just tapp -> Right tapp
      Nothing -> Left . TAppIdNotFound . decodeUtf8 $ ?app_id
    TxAborted -> Left . RedisTError $ "TxAborted"
    TxError msg -> Left . RedisTError . T.pack $ msg 

-- | Deletes an application in the database
deleteApplication :: Connection -> ApplicationIdentifier -> AppId -> IO (Either TError ())
deleteApplication conn app_name app_id' = runRedis conn $ do
  let app_id = integerToByteString app_id'
  transaction_result <- multiExec $ do
    dels [tAppName, tAppIdentifier, tAppDescription, 
               tAppRepositoryUrl, tAppContactEmail, tAppCreationDate,
               tAppKey, tAppOwners] app_id
    del [(tAppId app_name)]
    zrem tAppIds [app_id]
  return $ case transaction_result of 
    TxSuccess a -> Right ()
    TxAborted -> Left . RedisTError $ "TxAborted"
    TxError msg -> Left . RedisTError . T.pack $ msg 
    
-- | This should be used outside this module

tAppFromRedis :: ByteString -> MaybeBS -> MaybeBS -> MaybeBS -> MaybeBS -> MaybeBS -> MaybeBS -> MaybeBS -> [ByteString] -> Maybe TApplication
tAppFromRedis appid (Just n) (Just i) (Just d) (Just r) (Just c) (Just cd) (Just ak) ows = Just $ TApp { 
      aid = byteStringToInteger appid,
      name = decodeUtf8 n,
      identifier = decodeUtf8 i,
      description = decodeUtf8 d,
      repositoryUrl = decodeUtf8 r,
      contactEmail = decodeUtf8 c,
      creationDate = read . T.unpack . decodeUtf8 $ cd,
      appKey = decodeUtf8 ak,
      owners = Prelude.map byteStringToInteger ows
      }
tAppFromRedis _ _ _ _ _ _ _ _ _ = Nothing

insertNewTApp :: Connection -> Text -> Text -> Text -> Text -> Text -> UTCTime -> Text -> [UserId] -> IO (Either TError AppId)
insertNewTApp conn name' identifier' description' repositoryUrl' contactEmail' creationDate appKey' owners' = runRedis conn $ do
  tAppID <- incr "apps:max_id"
  case tAppID of
    Left _ -> return . Left . RedisTError $ "Could not create index (id) for a new application"
    Right appid -> let
      ?app_id = integerToByteString appid
      in do
        zadd tAppIds [(fromInteger appid, ?app_id)]
        set (tAppId identifier') ?app_id
        setProperty tAppName name'
        setProperty tAppIdentifier description'
        setProperty tAppIdentifier identifier'
        setProperty tAppDescription description'
        setProperty tAppRepositoryUrl repositoryUrl'
        setProperty tAppContactEmail contactEmail'
        set (tAppCreationDate ?app_id) (Char8.pack . show $ creationDate)
        setProperty tAppKey appKey'
        sadd (tAppOwners ?app_id) (Prelude.map integerToByteString owners')
        return . Right $ appid

setProperty :: (?app_id :: ByteString, RedisCtx m f) => (ByteString -> ByteString) -> Text -> m (f Status) 
setProperty f p = set (f ?app_id) (encodeUtf8 p)

gets :: (?app_id :: ByteString, RedisCtx m f) => (ByteString -> ByteString) -> m (f (Maybe ByteString))
gets f = get (f ?app_id)

dels :: (RedisCtx m f) => [(ByteString -> ByteString)] -> ByteString -> m (f Integer)
dels ps id = del (Prelude.map (\f -> f id) ps)

tAppIds = "apps" <.> "ids"
tAppId identifier = "tapp" .> (encodeUtf8 identifier) <. "id"
tAppKey appid = "tapp" .> appid <. "appKey"
tAppCreationDate appid = "tapp" .> appid <. "creationDate"
tAppName appid = "tapp" .> appid <. "name"
tAppDescription appid = "tapp" .> appid <. "description"
tAppRepositoryUrl appid = "tapp" .> appid <. "repositoryUrl"
tAppContactEmail appid = "tapp" .> appid <. "contactEmail"
tAppOwners appid = "tapp" .> appid <. "owners"
tAppIdentifier appid =  ("tapp" .> appid <. "identifier")
