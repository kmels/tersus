{-# LANGUAGE PackageImports #-}
-- import "tersus" Application (getApplicationDev)
import "tersus" TersusCluster.Cluster (tersusDevel)

main :: IO ()
main = do
    putStrLn "Starting devel application \n\n If requests are not being responded, make sure Postgress is running!!! "
    tersusDevel
