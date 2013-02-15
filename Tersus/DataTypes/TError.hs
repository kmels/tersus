module Tersus.DataTypes.TError where

import Data.Text
import Tersus.DataTypes.TypeSynonyms
data TError = 
  TheImpossibleHappened Text -- if we catch these types of errors, we ought to fix them in the code
  | RedisTError Text              
  | TAppIdNotFound ApplicationIdentifier
 