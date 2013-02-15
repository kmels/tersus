-- | This module deals with git operations on an application's repository.
module Handler.TApplication.Git(
    pullChanges
  , clone
) where

import           Control.Monad        (when)
import qualified Data.Conduit         as C
import qualified Data.Conduit.Binary  as CB
import           Data.Conduit.Process
import qualified Data.Text            as T
import           Import
import           Prelude
import           System.Directory     (doesDirectoryExist)
import           System.IO

--os filesystem
import           Tersus.Filesystem

import           Tersus.DataTypes.TApplication
tApplicationDirectory :: TApplication -> String
tApplicationDirectory = pathToString . tAppDirPath . identifier

-- TODO: implement
repositoryExists :: TApplication -> IO Bool
repositoryExists tapp = do
--  $(logDebug) "Checking if repository exists.."
  exists <- doesDirectoryExist . tApplicationDirectory $ tapp
--  $(logDebug) "Repository exists:" ++ show exists
  return exists

-- TODO: implemento
pullChanges :: TApplication -> IO()
pullChanges tapp = do
--  $(logDebug) "Pulling changes.."
  repoExists <- repositoryExists tapp
  when (not repoExists) $ clone tapp
  when (repoExists) $ pull tapp
  return ()

-- TODO: implement
clone :: TApplication -> IO ()
clone tapp = C.runResourceT $ do
--  liftIO $(logDebug) "Cloning repository.."
  let
    (repoUrl',repoName) = (T.unpack . repositoryUrl $ tapp, T.unpack . identifier $ tapp)
    cloneCmd = "git clone --quiet " ++ repoUrl' ++ " " ++ tApplicationDirectory tapp
--  $(logDebug) "Spawning: " ++ cloneCmd
  sourceCmd cloneCmd C.$$ CB.sinkHandle stdout

pull :: TApplication -> IO ()
pull tapp = C.runResourceT $ do
  let
    repoName = T.unpack . identifier $ tapp
    pullCmd = "cd "++ tApplicationDirectory tapp ++ " git pull"
  sourceCmd pullCmd C.$$ CB.sinkHandle stdout