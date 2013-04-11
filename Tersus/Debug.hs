module Tersus.Debug where

import Control.Monad.IO.Class
import Prelude

debugM :: String -> IO ()
debugM = putStrLn

verboseM :: String -> IO ()
verboseM = debugM

--io :: MonadIO m => IO a -> m a
--io = liftIO
