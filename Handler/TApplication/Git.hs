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
import           Model
import           Prelude
import           System.Directory     (doesDirectoryExist)
import           System.IO

-- TODO: implement
repositoryExists :: TApplication -> IO Bool
repositoryExists tapp = do
--  $(logDebug) "Checking if repository exists.."
  exists <- doesDirectoryExist $ "/tmp/" ++ (T.unpack $ tApplicationIdentifier tapp)
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
    (repoUrl',repoName) = (case tApplicationRepositoryUrl tapp of
                             Just repoUrl'' -> T.unpack $ repoUrl''
                             _ -> error "Git.clone, Not implemented yet",
                          T.unpack $ tApplicationIdentifier tapp)
    cloneCmd = "git clone --quiet " ++ repoUrl' ++ " /tmp/"++ repoName
--  $(logDebug) "Spawning: " ++ cloneCmd
  sourceCmd cloneCmd C.$$ CB.sinkHandle stdout

pull :: TApplication -> IO ()
pull tapp = C.runResourceT $ do
  let
    repoName = T.unpack $ tApplicationIdentifier tapp
    pullCmd = "cd /tmp/"++repoName++"; git pull"
  sourceCmd pullCmd C.$$ CB.sinkHandle stdout