{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE OverloadedStrings #-}
module Tersus.Global where

import Prelude

import Import
import Data.Aeson as J
import qualified Data.Text as T
import Data.Text.Lazy.Internal (foldlChunks)
import Data.Text.Lazy.Internal as LT (Text)


-- Datatypes and functions that are general for many of the Tersus components

-- The launch configurations. Theese should be named the same as the launch
-- envoiernments in the yaml configuration files.
data TersusEnvoiernment = Development | Production | Staging | Testing | Default deriving (Show,Eq,Read)

-- The location of the database yaml file
databaseYaml :: String
databaseYaml = "config/postgres.yml"

instance ToJSON a => ToJSON (Entity a) where
    toJSON (Entity _ a) = toJSON a

collapseLazyText :: LT.Text -> T.Text
collapseLazyText text = foldlChunks (\t1 t2 -> T.concat [t1,t2]) "" text

-- | Left-biased choice on maybes
orElse :: Maybe a -> Maybe a -> Maybe a
x `orElse` y = case x of
                 Just _  -> x
                 Nothing -> y
                 