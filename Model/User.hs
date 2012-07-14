{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE OverloadedStrings     #-}
module Model.User where

import Import
import Data.Aeson as J
import qualified Data.Text as T

instance ToJSON User where
    toJSON (User nickname _ _) = J.object [("username",String nickname)]