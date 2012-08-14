{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE OverloadedStrings #-}
module Model.User where

import           Data.Aeson as J
import           Import

instance ToJSON (UserGeneric a) where
    toJSON (User email nickname _ ) = J.object [("email",String email),("username",String nickname)]

