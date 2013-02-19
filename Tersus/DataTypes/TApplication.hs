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
import           Data.Maybe
import qualified Data.Text as T
import           Data.Text.Encoding
import           Database.Redis
import           Prelude
import           Tersus.Database
import           Tersus.DataTypes.TypeSynonyms
import           Tersus.DataTypes.TError

data TApplication = TApp { 
  -- name of the application
  name :: Text
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
  }
  
instance B.Binary TApplication where
         put (TApp name id' desc _ email date appKey) = B.put (name,id',desc,email,show date,appKey)

         get = do
             (name,id',desc,email,date,appKey) <- B.get
             return $ TApp name id' desc "**TODO**:repo" email (read date :: UTCTime) appKey

getApplications :: Connection -> IO [TApplication]
getApplications conn = runRedis conn $ do 
  return []
  
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
