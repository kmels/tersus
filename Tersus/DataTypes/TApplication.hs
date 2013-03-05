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
import           Tersus.Filesystem
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
  
instance B.Binary TApplication where
         put (TApp aid name id' desc _ email date appKey owners) = B.put (aid, name,id',desc,email,show date,appKey,owners)

         get = do
             (aid, name,id',desc,email,date,appKey,owners) <- B.get
             return $ TApp aid name id' desc "**TODO**:repo" email (read date :: UTCTime) appKey owners

getApplications :: Connection -> IO [TApplication]
getApplications conn = runRedis conn $ do 
  return []
  
-- | Gets an application id given an application identifier.
getAppId :: Connection -> ApplicationIdentifier -> IO (Either TError AppId)
getAppId conn identifier = runRedis conn $ do
  aid <- get $ "tapp" .> (encodeUtf8 identifier) <. "id"
  case aid of
    Left _ -> return . Left . TAppIdNotFound $ identifier
    Right Nothing -> return . Left . TAppIdNotFound $ identifier
    Right (Just aid') -> return . Right . byteStringToInteger $ aid'

getTApplicationById :: Connection -> ApplicationIdentifier -> IO (Either TError TApplication)
getTApplicationById conn appidText = runRedis conn $ do
  let appid = encodeUtf8 appidText -- ByteString
  tapp <- multiExec $ do -- Redis Monad
    name' <- get $ "tapp" .> appid <. "name" -- Either Reply ByteString
    desc' <- get $ "tapp" .> appid <. "description"
    repo' <- get $ "tapp" .> appid <. "repositoryUrl"
    email' <- get $ "tapp" .> appid <. "contactEmail"
    creationDate' <- get $ "tapp" .> appid <. "creationDate"
    appkey' <- get $ "tapp" .> appid <. "appkey"
    return $ tAppFromRedis appid <$> name' <*> desc' <*> repo' <*> email' <*> creationDate' <*> appkey'
  return $ case tapp of
    TxSuccess a -> case a of
      Just tapp -> Right tapp
      Nothing -> Left . TAppIdNotFound . decodeUtf8 $ appid
    TxAborted -> Left . RedisTError $ "TxAborted"
    TxError msg -> Left . RedisTError . T.pack $ msg 

-- | This should be used outside this module

tAppFromRedis :: ByteString -> MaybeBS -> MaybeBS -> MaybeBS -> MaybeBS -> MaybeBS -> MaybeBS -> Maybe TApplication
tAppFromRedis appid (Just n) (Just d) (Just r) (Just c) (Just cd) (Just ak) = Just $ TApp { 
      identifier = decodeUtf8 appid,
      name = decodeUtf8 n, 
      description = decodeUtf8 d,
      repositoryUrl = decodeUtf8 r,
      contactEmail = decodeUtf8 c,
      creationDate = read . T.unpack . decodeUtf8 $ cd,
      appKey = decodeUtf8 ak 
      }
tAppFromRedis _ _ _ _ _ _ _ = Nothing

insertNewTApp :: Connection -> Text -> Text -> Text -> Text -> Text -> UTCTime -> Text -> [UserId] -> IO (Either TError AppId)
insertNewTApp conn name' identifier' description' repositoryUrl' contactEmail' creationDate appKey' owners' = runRedis conn $ do
  tAppID <- incr "apps:max_id"
  case tAppID of
    Left _ -> return . Left . RedisTError $ "Could not create index (id) for a new application"
    Right appid -> let
      aid = integerToByteString appid
      in do
        set ("tapp" .> (encodeUtf8 identifier') <. "id") aid
        set ("tapp" .> aid <. "name") (encodeUtf8 name')
        set ("tapp" .> aid <. "identifier") (encodeUtf8 identifier')
        set ("tapp" .> aid <. "description") (encodeUtf8 description')
        set ("tapp" .> aid <. "repositoryUrl") (encodeUtf8 repositoryUrl')
        set ("tapp" .> aid <. "contactEmail") (encodeUtf8 contactEmail')
        set ("tapp" .> aid <. "creationDate") (Char8.pack . show $ creationDate)
        set ("tapp" .> aid <. "appKey") (encodeUtf8 identifier')
        sadd ("tapp" .> aid <. "owners") (Prelude.map integerToByteString owners')
        return . Right $ appid
