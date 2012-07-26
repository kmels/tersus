{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE OverloadedStrings #-}
module Model.User where

import           Data.Aeson as J
import qualified Data.Text  as T
import           Import

instance ToJSON User where
    toJSON (User email _ _ _) = J.object [("email",String email)]
