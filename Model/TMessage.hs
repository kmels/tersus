{-# OPTIONS_GHC -fno-warn-orphans #-}
module Model.TMessage where

import           Data.Aeson as J
import           Import

instance ToJSON TMessage where
         toJSON (TMessage _ _ _ _ content') = (J.String content')
