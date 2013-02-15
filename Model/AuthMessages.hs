{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE OverloadedStrings #-}
module Model.AuthMessages where

import Import
import Data.Aeson as J
import Control.Monad (mzero)

instance FromJSON AuthMessage where
  parseJSON (Object authMessage) = AuthMessage <$>
                                   authMessage .: "senderAppKey" <*>
                                   authMessage .: "userReceiver" <*>
                                   authMessage .: "appReceiver"  <*>
                                   authMessage .: "content"
  parseJSON _ = mzero
                                   