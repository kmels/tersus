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
import           Tersus.DataTypes.TApplication
import           Tersus.DataTypes.TFile
import           Tersus.HandlerMachinery
import           Tersus.Filesystem
import Tersus.Database
import           Tersus.Debug
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
pullChanges :: TApplication -> GHandler s Tersus ()
pullChanges tapp = do
--  $(logDebug) "Pulling changes.."
  repoExists <- io $ repositoryExists tapp
  when (not repoExists) $ clone tapp
  when (repoExists) $ pull tapp
  return ()

-- TODO: implement
clone :: TApplication -> GHandler s Tersus ()
clone tapp = do
  conn <- getConn
  C.runResourceT $ do
    --  liftIO $(logDebug) "Cloning repository.."
    let
      (repoUrl',repoName) = (T.unpack . repositoryUrl $ tapp, T.unpack . identifier $ tapp)
      cloneCmd = "git clone --quiet " ++ repoUrl' ++ " " ++ tApplicationDirectory tapp
--  $(logDebug) "Spawning: " ++ cloneCmd
    sourceCmd cloneCmd C.$$ CB.sinkHandle stdout
  fid <- io $ mkTFileFromPath conn (apps_dir:[identifier tapp])
  io . debugM $ " Created file id " ++ show fid
  return ()

pull :: TApplication -> GHandler s Tersus ()
pull tapp = io $ C.runResourceT $ do
  let
    repoName = T.unpack . identifier $ tapp
    pullCmd = "cd "++ tApplicationDirectory tapp ++ " git pull"
  sourceCmd pullCmd C.$$ CB.sinkHandle stdout
