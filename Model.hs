{-# LANGUAGE TemplateHaskell #-}
module Model where

import           Data.ByteString 
import qualified           Data.ByteString  as BS
import           Data.List              (find)
import           Data.Maybe             (fromJust)

import qualified Data.Text              as T
import           Data.Text(Text)
import           Data.Text.Encoding     (encodeUtf8,decodeUtf8)
import           Data.Typeable.Internal (Typeable)
import           Prelude
import           Yesod
import qualified Data.Binary            as B

-- Data types
import           Tersus.DataTypes.TApplication
import           Tersus.DataTypes.User
-- You can define all of your database entities in the entities file.
-- You can find more information on persistent and how to declare entities
-- at:
-- http://www.yesodweb.com/book/persistent/


type Id = Double
type IdList = [Id]
type ApplicationName = ByteString

type UrlK = Text

data WriteMode = Override | AppendToFile | Create | Delete deriving (Show, Eq, Enum)

data ReadMode = FileMetadata | GetContent | Pagination deriving (Show, Eq, Enum)

data FileType = File | Directory deriving Show

{- TODO NOTE: THIS FILE SHOULD BE DELETED.... -}