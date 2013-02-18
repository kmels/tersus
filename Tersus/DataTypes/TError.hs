module Tersus.DataTypes.TError where

import Data.Text
import Prelude
import Tersus.DataTypes.TypeSynonyms
data TError = 
  --General errors
  TheImpossibleHappened Text -- if we catch these types of errors, we ought to fix them in the code
  
  -- Database errors
  | RedisTError Text              
  
  -- From requests
  | TAppIdNotFound ApplicationIdentifier
  | TFilePathNotFound Path deriving Show
  