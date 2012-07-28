{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE OverloadedStrings #-}
module Tersus.Global where

import Prelude

import Import
import Data.Aeson as J
import qualified Data.Text as T
import Database.Persist.Store (Entity)

-- Datatypes and functions that are general for many of the Tersus components

-- The launch configurations. Theese should be named the same as the launch
-- envoiernments in the yaml configuration files.
data TersusEnvoiernment = Development | Production | Staging | Testing | Default deriving (Show,Eq,Read)

-- The location of the database yaml file
databaseYaml :: String
databaseYaml = "config/postgres.yml"

instance ToJSON a => ToJSON (Entity a) where
    toJSON (Entity _ a) = toJSON a