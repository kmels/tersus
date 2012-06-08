{-# LANGUAGE TupleSections, OverloadedStrings #-}
module Handler.Messages where

--Author: Ernesto Rodriguez

--Description: Functions to handle the messaging system of Tersus.

import Import
import Data.HashTable as H
import Control.Concurrent (MVar,putMVar,takeMVar,newEmptyMVar,isEmptyMVar,modifyMVar_)
import Model.TersusResult
import Data.Aeson as D
import Data.Text as T
import Model
import Model.TMessage
import Data.Time.Clock (getCurrentTime)
import System.IO.Unsafe (unsafePerformIO)

-- -- registerUser :: String -> String -> IO ()
-- registerUser user app = do
--              master <- getYesod
--              mailBoxes' <- mailBoxes master
--              mailBox <- liftIO newEmptyMVar
--              liftIO $ H.insert mailBoxes' (user,app) mailBox
--              jsonToRepJson $ encode $ TersusResult 3 Import.Success

dummyUser = User (T.pack "neto") (Just (T.pack "1234")) []

-- This is a dummy datatype only to show that this works
-- It will be removed and never used
-- unsafePerformIO is there just because it's simpler and
-- this will not be part of tersus
dummyApp = TApplication (T.pack "emacs") (T.pack "dummy") (Just (T.pack "url")) (T.pack "mail@place.com") (unsafePerformIO getCurrentTime)  (T.pack "appkey")

dummyMsg = TMessage dummyUser dummyUser dummyApp dummyApp (T.pack "Alonso")

dummyInstance = AppInstance "neto" "emacs"

getInitMVarR = do 
            master <- getYesod
            mailBoxes' <- return $ mailBoxes master
            mailBox <- liftIO newEmptyMVar
            liftIO $ H.insert mailBoxes' dummyInstance mailBox
            jsonToRepJson $ encode $ TersusResult 3 Import.Success

getMessageR :: Handler RepJson
getMessageR = do
            master <- getYesod
            mailBoxes' <- return $ mailBoxes master
            Just mailBox <- liftIO $ H.lookup mailBoxes' dummyInstance
            msgs <- liftIO $ takeMVar mailBox
            jsonToRepJson $ encode msgs

getSetMVarR :: Handler RepJson
getSetMVarR = do
            master <- getYesod
            mailBoxes' <- return $ mailBoxes master
            Just mailBox <- liftIO $ H.lookup mailBoxes' dummyInstance
            liftIO $ addMessage mailBox dummyMsg
            jsonToRepJson $ encode $ TersusResult 3 Import.Success

            where
                addMessage var msg =  isEmptyMVar var >>= \b -> case b of
                                     True -> putMVar var [msg]
                                     False -> modifyMVar_ var (\x -> return (msg:x))
            
             