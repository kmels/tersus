{-# LANGUAGE UndecidableInstances  #-}

module Tersus.DataTypes.TError where

import Control.Exception.Base
import Data.Text
import Data.Typeable
import Prelude
import Tersus.DataTypes.TypeSynonyms
import Yesod.Handler

data TError = 
  --General errors
  TheImpossibleHappened Text -- if we catch these types of errors, we ought to fix them in the code
  
  -- Database errors
  | RedisTError Text              

  -- From requests
  | MissingParameter Text Text -- parameter name, parameter description
  | TAppIdNotFound ApplicationIdentifier
  | TFileIdNotFound FileId  
  | TFilePathNotFound Path deriving (Show,Typeable)
  
instance Exception TError
