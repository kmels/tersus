module Tersus.Filesystem.Resources where

import           Data.Either
import qualified Data.Text as T
import           Database.Redis
import           Prelude
import           System.Directory
import           Tersus.DataTypes.TFile
import           Tersus.DataTypes.TypeSynonyms
import           Tersus.Filesystem

-- | Returns a list of resources contained in the applications source code
listAppResources :: Connection -> Path -> IO [TFile]
listAppResources conn parent_dir = do
  filenames <- getPathContents (apps_dir:parent_dir)
  eithers <- mapM tFileFromPath' filenames 
  return . rights $ eithers
  where
    tFileFromPath' fn = tFileFromPath conn (parent_dir ++ [T.pack fn])
  
