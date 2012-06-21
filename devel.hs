{-# LANGUAGE PackageImports #-}
-- import "tersus" Application (getApplicationDev)
import "tersus" TersusCluster.Cluster (tersusDevel)
import System.Exit (exitSuccess)
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
    --loop

