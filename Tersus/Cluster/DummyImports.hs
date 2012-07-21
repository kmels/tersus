module Tersus.Cluster.DummyImports where

import Data.Text as T
import Model
import System.IO.Unsafe (unsafePerformIO)
import Data.Time.Clock (getCurrentTime)
import Data.Maybe (Maybe(Just))


-- This is a dummy datatype only to show that this works
-- It will be removed and never used
-- unsafePerformIO is there just because it's simpler and
-- this will not be part of tersus

dummyUser :: User
dummyUser = User (T.pack "neto") (Just (T.pack "1234")) []

dummyUser2 = User (T.pack "kmels") (Just (T.pack "1234")) []
                       
dummyApp :: TApplication
dummyApp = TApplication (T.pack "emacs") (T.pack "identifier") (T.pack "description dummy") (Just (T.pack "url")) (T.pack "mail@place.com") (unsafePerformIO getCurrentTime)  (T.pack "appkey")

dummyApp2 = TApplication (T.pack "vi") (T.pack "idvi") (T.pack "description dummy") (Just (T.pack "url")) (T.pack "mail@place.com") (unsafePerformIO getCurrentTime)  (T.pack "appkey2")

dummyMsg = TMessage dummyUser dummyUser2 dummyApp dummyApp2 (T.pack "Alonso") (unsafePerformIO getCurrentTime)

dummyAddress = Address dummyUser dummyApp

dummyAddress2 = Address dummyUser2 dummyApp2
