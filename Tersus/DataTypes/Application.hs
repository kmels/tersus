module Tersus.DataTypes.TApplication where

import qualified Data.Binary            as B
import           Data.ByteString
import           Data.String
import           Data.Time              (UTCTime)
import           Data.Time.Clock
import           Prelude
import Data.ByteString.Char8


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
  
instance B.Binary Application where
         put (TApp name id' desc _ email date appKey) = B.put (name,id',desc,email,show date,appKey)

         get = do
             (name,id',desc,email,date,appKey) <- B.get
             return $ TApp name id' desc "**TODO**:repo" email (read date :: UTCTime) appKey

