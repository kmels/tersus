module Tersus.Debug where

import Control.Monad.IO.Class
import Prelude
debugM :: String -> IO ()
debugM = putStrLn

--io :: MonadIO m => IO a -> m a
--io = liftIO
