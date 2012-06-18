{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections     #-}
module Handler.Messages where

--Author: Ernesto Rodriguez

--Description: Functions to handle the messaging system of Tersus.

import           Data.Aeson                 as D
import           Import
import           MessagingPipeline.Pipeline
import           Model.TersusResult         ()


-- Dummy test functions, will not exist in future releases
getInitMVarR :: Handler RepJson
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
            _ <- liftIO $ writeMessage master dummyMsg
            jsonToRepJson $ encode $ TersusResult 3 Import.Success
