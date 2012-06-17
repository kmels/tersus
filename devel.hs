{-# LANGUAGE PackageImports #-}
-- import "tersus" Application (getApplicationDev)
import "tersus" TersusCluster.Cluster (tersusDevel)
import Network.Wai.Handler.Warp
    (runSettings, defaultSettings, settingsPort)
import Control.Concurrent (forkIO)
import System.Directory (doesFileExist, removeFile)
import System.Exit (exitSuccess)
import Control.Concurrent (threadDelay)
-- import TersusCluster.Cluster (runTersusDevel)
import Data.HashTable as H

main :: IO ()
main = do
    putStrLn "Starting devel application"
    -- addresses <- liftIO $ H.new (==) hashUserApp
    -- mailBoxes <- liftIO $ H.new (==) hashUserApp
    -- (port, app) <- getApplicationDev addresses mailBoxes
    -- forkIO $ runSettings defaultSettings
    --    { settingsPort = port
    --    } app
    tersusDevel
    loop

loop :: IO ()
loop = do
  threadDelay 100000
  e <- doesFileExist "dist/devel-terminate"
  if e then terminateDevel else loop

terminateDevel :: IO ()
terminateDevel = exitSuccess