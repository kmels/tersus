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
import           Database.Redis
import           Tersus.DataTypes.TApplication
import           Tersus.DataTypes.TFile(mkTFileFromPath)
import           Tersus.Database
import           Tersus.Debug
import           Tersus.Filesystem
import           Tersus.HandlerMachinery
tApplicationDirectory :: TApplication -> String
tApplicationDirectory tapp = fullStrPath $ [apps_dir,identifier tapp]

-- TODO: implement
repositoryExists :: TApplication -> IO Bool
repositoryExists tapp = do
--  $(logDebug) "Checking if repository exists.."
  exists <- doesDirectoryExist . tApplicationDirectory $ tapp
--  $(logDebug) "Repository exists:" ++ show exists
  return exists

-- TODO: implemento
pullChanges :: Connection -> TApplication -> IO ()
pullChanges conn tapp = do
--  $(logDebug) "Pulling changes.."
  repoExists <- io $ repositoryExists tapp
  when (not repoExists) $ clone conn tapp
  when (repoExists) $ pull tapp
  return ()

-- TODO: implement
clone :: Connection -> TApplication -> IO ()
clone conn tapp  = do
  C.runResourceT $ do
    --  liftIO $(logDebug) "Cloning repository.."
    let
      repoUrl' = T.unpack . repositoryUrl $ tapp
      cloneCmd = "git clone --quiet " ++ repoUrl' ++ " " ++ tApplicationDirectory tapp
--  $(logDebug) "Spawning: " ++ cloneCmd
    sourceCmd cloneCmd C.$$ CB.sinkHandle stdout
  fid <- mkTFileFromPath conn (apps_dir:[identifier tapp])
  debugM $ " Created file id " ++ show fid
  return ()

pull :: TApplication -> IO ()
pull tapp = C.runResourceT $ do
  let
    repoName = T.unpack . identifier $ tapp
    pullCmd = "cd "++ tApplicationDirectory tapp ++ " git pull"
  sourceCmd pullCmd C.$$ CB.sinkHandle stdout
