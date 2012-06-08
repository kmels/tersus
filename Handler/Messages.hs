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

dummyUser = User (T.pack "neto") (Just (T.pack "1234")) []

-- This is a dummy datatype only to show that this works
-- It will be removed and never used
-- unsafePerformIO is there just because it's simpler and
-- this will not be part of tersus
dummyApp = TApplication (T.pack "emacs") (T.pack "identifier") (T.pack "description dummy") (Just (T.pack "url")) (T.pack "mail@place.com") (unsafePerformIO getCurrentTime)  (T.pack "appkey")

dummyMsg = TMessage dummyUser dummyUser dummyApp dummyApp (T.pack "Alonso")

dummyInstance = AppInstance "neto" "emacs"

-- Load the mailbox belonging to the given app instance. Or None
-- if the mailBox has not been created or is not in this server
loadMailbox :: App -> AppInstance -> IO (Maybe (MVar [TMessage]))
loadMailbox master appInstance = do
            mailBoxes' <- return $ mailBoxes master
            H.lookup mailBoxes' appInstance

-- Create a mailbox accesible to the given app instance
createMailbox :: App -> AppInstance -> IO ()
createMailbox master appInstance = loadMailbox master appInstance
              >>= \m -> case m of
                  Nothing -> mkMailbox
                  _ -> return ()
              where
                mkMailbox = do
                          mailBoxes' <- return $ mailBoxes master
                          mailBox <- newEmptyMVar
                          H.insert mailBoxes' appInstance mailBox

-- Get the messages from the mailbox belonging to the 
-- given app instance
getMessages :: App -> AppInstance -> IO (Maybe [TMessage])
getMessages master appInstance =
            loadMailbox master appInstance
            >>= openMailBox
            where
                openMailBox (Just mail) = takeMVar mail 
                                          >>= return . Just
                openMailBox Nothing = return Nothing

-- Add a message to the mailbox belonging to the given
-- App instance. Return true if the message was written
-- or false if such mailbox dosen't exist
writeMessage :: App -> AppInstance -> TMessage -> IO Bool
writeMessage master appInstance message = 
             loadMailbox master appInstance
             >>= addMessage
             where
                addMessage (Just var)  =  isEmptyMVar var >>= \b -> case b of
                           True -> putMVar var [message] >> return True
                           False -> modifyMVar_ var (return . (:) message) 
                                    >> return True
                addMessage Nothing = return False
             

-- Dummy test functions, will not exist in future releases
getInitMVarR = do
            master <- getYesod
            liftIO $ createMailbox master dummyInstance
            jsonToRepJson $ encode $ TersusResult 3 Import.Success

getMessageR :: Handler RepJson
getMessageR = do
            master <- getYesod
            Just msgs <- liftIO $ getMessages master dummyInstance
            jsonToRepJson $ encode msgs

getSetMVarR :: Handler RepJson
getSetMVarR = do
            master <- getYesod
            liftIO $ writeMessage master dummyInstance dummyMsg
            jsonToRepJson $ encode $ TersusResult 3 Import.Success
