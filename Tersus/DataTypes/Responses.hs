module Tersus.DataTypes.Responses where

import Data.Aeson as J
import qualified Data.Text as T
import Import

data TersusResultCode = Success | SuccessDontUpdate | RequestError | InexistentFile | NotEnoughPrivileges | DirectoryNotEmpty | OutOfRange deriving (Show, Eq)

data TRequestResponseBody = Message Text | JsonResult Value
data TRequestResponse = TRequestResponse TersusResultCode TRequestResponseBody

data TersusResult = TersusResult Int TersusResultCode | TersusErrorResult TersusResultCode Text

instance ToJSON TersusResultCode where
         toJSON resultCode = (J.String (T.pack (show resultCode)))

instance ToJSON TersusResult where
         toJSON (TersusResult httpStatusCode tersusResultCode) = J.object [(T.pack "httpStatusCode") .= httpStatusCode, (T.pack "response") .= tersusResultCode]
         toJSON (TersusErrorResult tersusResultCode errorMessage) = J.object [(T.pack "response") .= tersusResultCode, (T.pack "message") .= errorMessage]

instance ToJSON TRequestResponse where
         toJSON (TRequestResponse httpStatusCode (Message message')) = J.object [(T.pack "httpStatusCode") .= httpStatusCode, (T.pack "message") .= message']
         toJSON (TRequestResponse httpStatusCode (JsonResult j')) = J.object [(T.pack "httpStatusCode") .= httpStatusCode, (T.pack "result") .= j']
