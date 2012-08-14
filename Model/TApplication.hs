{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE OverloadedStrings #-}

-- File to write all instances to wich TApplicaiton will belong to
module Model.TApplication where

import Import
import Data.Aeson as J

-- JSON Encoding for application as defined in the Tersus Developer
-- API. This API probably needs review
instance ToJSON TApplication where
    toJSON (TApplication _ _ _ _ _ _ appKey) = J.object [("id",String appKey)]
