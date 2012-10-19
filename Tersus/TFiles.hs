module Tersus.TFiles where

-- | Description: This file contains general datatypes and functions to inspect the resources in
-- the filesystem. It has convenience functions that will encode the contents of
-- directories in nice ways.
-- Author: Ernesto Rodriguez

import           Data.Aeson         as J
import Data.Text (Text,pack)
import System.Directory
import Control.Monad
import Import hiding (catch)

data ResourceType = FileType | FolderType deriving Show

-- | Represents a file system resource
data Resource = Resource{
  resourceName :: Text, -- | The filesystem name of the resource
  resourceFolder :: Text, -- | The folder where the resource resides
  resourceType :: ResourceType -- | The type of the resource
  }

instance ToJSON Resource where
  toJSON (Resource resourceName' resourceFolder' resourceType') = J.object [
    ("name",toJSON resourceName'),
    ("folder",toJSON resourceFolder'),
    ("resourceType",toJSON resourceType')]

instance ToJSON ResourceType where
  toJSON = toJSON . show

instance ToContent Resource where
  toContent = toContent . toJSON

instance ToContent ([Resource]) where
  toContent = toContent . toJSON

-- | Get the files and folders inside a folder and
-- and also obtain the type of resource each of the
-- elements is.
getDirectoryContentsTyped :: FilePath -> IO [Resource]
getDirectoryContentsTyped directory = do
  allFiles <- getDirectoryContents directory
  let
    files = Import.filter (\f -> f /= "." && f /= "..") allFiles
  mapM buildMetadata files

  where
    buildMetadata file = do
      exists <- doesDirectoryExist $ directory ++ "/" ++ file
      if exists
        then return $ Resource (pack file) (pack directory) FolderType
        else return $ Resource (pack file) (pack directory) FileType


                

                
  