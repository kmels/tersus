{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections     #-}
module Handler.Messages where

--Author: Ernesto Rodriguez

--Description: Functions to handle the messaging system of Tersus.

import           Control.Concurrent (MVar, isEmptyMVar, modifyMVar_, newEmptyMVar, putMVar, takeMVar)
import           Data.Aeson         as D
import           Data.HashTable     as H
import           Data.Text          as T
import           Data.Time.Clock    (getCurrentTime)
import           Import
import           Model
import           Model.TMessage
import           Model.TersusResult
import           System.IO.Unsafe   (unsafePerformIO)
import           MessagingPipeline.Pipeline
                

-- Dummy test functions, will not exist in future releases
getInitMVarR = do
            master <- getYesod
            liftIO $ createMailbox master $ Address dummyUser dummyApp
            jsonToRepJson $ encode $ TersusResult 3 Import.Success

getMessageR :: Handler RepJson
getMessageR = do
            master <- getYesod
            Just msgs <- liftIO $ retrieveMessages master $ Address dummyUser dummyApp
            jsonToRepJson $ encode msgs

getSetMVarR :: Handler RepJson
getSetMVarR = do
            master <- getYesod
            liftIO $ writeMessage master dummyMsg
            jsonToRepJson $ encode $ TersusResult 3 Import.Success
