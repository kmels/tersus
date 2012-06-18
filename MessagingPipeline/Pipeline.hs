{-# LANGUAGE TemplateHaskell #-}
module MessagingPipeline.Pipeline where

import           Control.Concurrent (MVar, ThreadId, isEmptyMVar, modifyMVar_, newEmptyMVar, putMVar,
 takeMVar)
import           Control.Concurrent (forkIO)
import           Control.Monad      (forever)
import           Data.HashTable     as H
import           Data.Text          as T
import           Data.Time.Clock    (getCurrentTime)
import           Import
import           Prelude
import           Remote
import           System.IO.Unsafe   (unsafePerformIO)
-- import Control.Monad.Trans (lift)
-- import Control.Monad (liftM)

-- Load the mailbox belonging to the given app instance. Or None
-- if the mailBox has not been created or is not in this server
loadMailbox :: Addressable a => App -> a -> IO (Maybe (MVar [TMessage]))
loadMailbox master appInstance = do
            mailBoxes' <- return $ mailBoxes master
            H.lookup mailBoxes' $ getAppInstance appInstance

-- Create a mailbox accesible to the given app instance
createMailbox :: App -> Address -> IO ()
createMailbox master appInstance = loadMailbox master appInstance
              >>= \m -> case m of
                  Nothing -> mkMailbox
                  _ -> return ()
              where
                mkMailbox = do
                          mailBoxes' <- return $ mailBoxes master
                          mailBox <- newEmptyMVar
                          H.insert mailBoxes' (getAppInstance appInstance) mailBox

-- Get the messages from the mailbox belonging to the
-- given app instance
retrieveMessages :: App -> Address -> IO (Maybe [TMessage])
retrieveMessages master appInstance =
            loadMailbox master appInstance
            >>= openMailBox
            where
                openMailBox (Just mail) = takeMVar mail
                                          >>= return . Just
                openMailBox Nothing = return Nothing

-- Add a message to the mailbox belonging to the given
-- App instance. Return true if the message was written
-- or false if such mailbox dosen't exist
writeMessage :: App -> TMessage -> IO Bool
writeMessage master message =
             putStrLn "No se imprime" >> -- Esto No se imprime, pero deberia imprimirse
             loadMailbox master message
             >>= addMessage
             where
                addMessage (Just var)  =  isEmptyMVar var >>= \b -> case b of
                           True -> putMVar var [message] >> return True
                           False -> modifyMVar_ var (return . (:) message)
                                    >> return True
                addMessage Nothing = return False

printStatus :: Bool -> IO ()
printStatus True = putStrLn "Success"
printStatus False = putStrLn "Nay"

recieveMessage :: TMessage -> ProcessM ()
recieveMessage msg = do
               -- _ <- liftIO $ Control.Monad.Trans.lift getYesod >>= \y' -> (return (writeMessage y' msg))
               _ <- liftIO $ putStrLn "Si se imprime" >> return getYesod >>= \y -> return (y >>= \y' -> liftIO (writeMessage y' msg))
               -- master' <- return getYesod
               -- bool <- return (master' >>= \master -> liftIO ((writeMessage master msg)))
               -- str <- lift $ return (bool >>= return . show)
               -- _ <- liftIO $ putStrLn "STR ajja"
               -- _ <- return $ (liftM printStatus) bool
               -- Control.Monad.Trans.Class.lift (str>>= \s -> return s)
               -- l <- return (str >>= \oStr -> liftIO (putStrLn oStr))
               -- checkRes bool
               say "Done..."

               -- where
               -- --   checkRes (GHandler sub0 App True) = say "Send"
               --      checkRes a = a >>= \x -> case x of
               --                            True -> say "Open"
               --                            _ -> say "Not Open"


putNr :: Int -> ProcessM ()
putNr num  = do
      say ("Got Number: "++ (show num) ++". From: ") -- ++ (show process))

$( remotable ['recieveMessage] )
-- $( remotable ['putNr] )


makeChannels :: String -> ProcessM ()
makeChannels "T2" = receiveWait []
makeChannels "T1" = forever $ do
             peers <- getPeers
             let t2s = findPeerByRole peers "T2"
             mapM_ (\peer -> spawn peer (recieveMessage__closure dummyMsg)) t2s
             -- mapM_ (\peer -> spawn peer (putNr__closure 5)) t2s
             say "Data Sent\n"
--TODO Implement makeChannels _
--I recommend making data types for T2 and T1 if they are the only value posibilities
makeChannels _ = say "What?"


initPipeline :: IO ThreadId
initPipeline = do
             _ <- forkIO $ remoteInit (Just "config/servers") [MessagingPipeline.Pipeline.__remoteCallMetaData] makeChannels
             forkIO $ remoteInit (Just "config/servers2") [MessagingPipeline.Pipeline.__remoteCallMetaData] makeChannels

dummyUser :: User
dummyUser = User (T.pack "neto") (Just (T.pack "1234")) []

-- This is a dummy datatype only to show that this works
-- It will be removed and never used
-- unsafePerformIO is there just because it's simpler and
-- this will not be part of tersus
dummyApp :: TApplication
dummyApp = TApplication (T.pack "emacs") (T.pack "identifier") (T.pack "description dummy") (Just (T.pack "url")) (T.pack "mail@place.com") (unsafePerformIO getCurrentTime)  (T.pack "appkey")

dummyMsg :: TMessage
dummyMsg = TMessage dummyUser dummyUser dummyApp dummyApp (T.pack "Alonso")
