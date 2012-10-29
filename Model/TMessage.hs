{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE OverloadedStrings #-}
module Model.TMessage where

import           Data.Aeson         as J
import           Import
import           Model.TApplication
import           Model.User

-- Instance to convert a message into it's json representation, this instance is defined
-- according to the Tersus Developers Api.
instance ToJSON TMessage where
         toJSON (TMessage sender receiver senderApp receiverApp msgBody _) = J.object [
           ("userSender",toJSON sender),
           ("userReceiver",toJSON receiver),
           ("appSender",toJSON senderApp),
           ("appReceiver",toJSON receiverApp),
           ("content",toJSON msgBody)
           ]
