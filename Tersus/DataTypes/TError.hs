{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE UndecidableInstances  #-}

module Tersus.DataTypes.TError where

import Control.Exception.Base
import Data.Text
import Data.Typeable
import Prelude
import Tersus.DataTypes.TypeSynonyms
import Yesod.Handler
import GHC.Generics (Generic)
import Data.Aeson (FromJSON, ToJSON, decode, encode)

data TError = 
  --General errors
  TheImpossibleHappened Text -- if we catch these types of errors, we ought to fix them in the code
  
  -- Database errors
  | RedisTError Text              

  -- From requests
  | MissingParameter Text Text -- parameter name, parameter description
  | TUserIdNotFound UserId
  | TUserNicknameNotFound Username
  | TAppIdNotFound ApplicationIdentifier
  | TFileIdNotFound FileId  
  | TFilePathNotFound Path deriving (Eq,Show,Typeable,Generic)
  
instance ToJSON TError 
instance Exception TError
