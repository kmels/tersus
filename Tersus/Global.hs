{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE OverloadedStrings #-}
module Tersus.Global where
import           Prelude

import           Data.Aeson              as J
import qualified Data.Text               as T
import           Data.Text.Lazy          (fromChunks)
import           Data.Text.Lazy.Encoding (decodeUtf8, encodeUtf8)
import           Data.Text.Lazy.Internal (foldlChunks)
import           Data.Text.Lazy.Internal as LT (Text)
import           Import

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

-- | Function to encode datatypes as JSON Text
encodeAsText :: ToJSON obj => obj -> T.Text
encodeAsText = collapseLazyText.decodeUtf8.encode.toJSON

-- | Decode a Text containing json representation of an object
decodeFromText :: FromJSON obj => T.Text -> Maybe obj
decodeFromText = decode.encodeUtf8.fromChunks.return

-- | Left-biased choice on maybes
orElse :: Maybe a -> Maybe a -> Maybe a
x `orElse` y = case x of
                 Just _  -> x
                 Nothing -> y

-- | The get parameter under which the access key is provided
accessKeyParameterName :: T.Text
accessKeyParameterName = "access_key"
