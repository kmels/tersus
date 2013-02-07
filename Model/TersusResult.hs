{-# OPTIONS_GHC -fno-warn-orphans #-}
module Model.TersusResult where


import Import
import Data.Aeson as J
import qualified Data.Text as T

instance ToJSON TersusResultCode where
         toJSON resultCode = (J.String (T.pack (show resultCode)))

instance ToJSON TersusResult where
         toJSON (TersusResult httpStatusCode tersusResultCode) = J.object [(T.pack "httpStatusCode") .= httpStatusCode, (T.pack "response") .= tersusResultCode]
         toJSON (TersusErrorResult tersusResultCode errorMessage) = J.object [(T.pack "response") .= tersusResultCode, (T.pack "message") .= errorMessage]

instance ToJSON TRequestResponse where
         toJSON (TRequestResponse httpStatusCode (Message message')) = J.object [(T.pack "httpStatusCode") .= httpStatusCode, (T.pack "message") .= message']
         toJSON (TRequestResponse httpStatusCode (JsonResult j')) = J.object [(T.pack "httpStatusCode") .= httpStatusCode, (T.pack "result") .= j']
