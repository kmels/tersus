-- This module provides functions to interact with the virtual file system.
-- They're, when they can, mainly conduits.

module TFile.OSUtils(writeFile) where

import Database.Persist.MongoDB
import Foundation
import Model
import Prelude
import Yesod

-- this function doesn't check security issues (that should be done before calling this function)
{-writeFile :: TFile -> IO ()
writeFile file = withMongoDBConn "tersus" "localhost" $ runMongoDBConn ReadStaleOk action
                 where
                   action = allDatabases-}
--                   pool = createMongoDBPool "tersus" "localhost" 1
--                   pool = connPool App 
