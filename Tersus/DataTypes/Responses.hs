module Tersus.DataTypes.Responses where

import           Control.Exception
import           Data.Aeson as J hiding (Success)
import           Data.Text
import qualified Data.Text as T
import           Network.HTTP.Types.Status
import           Prelude
import           Tersus.DataTypes.TError

data TResponse = TResult TResult
                 | TResponse TResult Text --TResultBody 

--instance Exception TResponse

data TResult = TError TError
               | Success 
               | SuccessDontUpdate 
               | RequestError 
               | InexistentFile 
               | NotEnoughPrivileges 
               | DirectoryNotEmpty 
               | OutOfRange deriving (Show, Eq)

instance ToJSON TResult where
  toJSON (TError terror) = J.object [ T.pack "result" .= (show terror)]
  toJSON result = J.object [ T.pack "result" .= (show result)]

instance ToJSON TResponse where
  toJSON (TResult result) = toJSON result
  toJSON (TResponse result text) = J.object [ 
    T.pack "result" .= (toJSON result), 
    T.pack "message" .= text
    ]
  
--data TResultBody = Message Text -- | JsonResult Value

mkResultStatus :: TResult -> Status
mkResultStatus (TError (TheImpossibleHappened text)) = status500 -- Internal server error
mkResultStatus (TError (RedisTError _)) = status500 -- Internal server error
mkResultStatus (TError (MissingParameter name desc)) = status400
mkResultStatus (TError (TUserIdNotFound _)) = status400
mkResultStatus (TError (TUserNicknameNotFound _)) = status400
mkResultStatus (TError (TAppIdNotFound _)) = status400
mkResultStatus (TError (TFileIdNotFound _)) = status400
mkResultStatus (TError (TFilePathNotFound _)) = status400
mkResultStatus Success = ok200 -- OK
mkResultStatus InexistentFile = status204 -- No content

mkResponseStatus :: TResponse -> Status
mkResponseStatus (TResult result) = mkResultStatus result
--should use repToJson instead as this doesn't consider the body
mkResponseStatus (TResponse result result_body) = mkResultStatus result 

--data TersusResult = TersusResult Int TersusResultCode | TersusErrorResult TersusResultCode Text

--instance T
-- instance ToJSON TersusResultCode where
--          toJSON resultCode = (J.String (T.pack (show resultCode)))
    
         -- toJSON (TersusResult httpStatusCode tersusResultCode) = J.object [(T.pack "httpStatusCode") .= httpStatusCode, (T.pack "response") .= tersusResultCode]
         -- toJSON (TersusErrorResult tersusResultCode errorMessage) = J.object [(T.pack "response") .= tersusResultCode, (T.pack "message") .= errorMessage]

-- instance ToJSON TResponse where
--          toJSON (TResponse result (Message message')) = J.object [(T.pack "httpStatusCode") .= httpStatusCode, (T.pack "message") .= message']
--          toJSON (TResponse httpStatusCode (JsonResult j')) = J.object [(T.pack "httpStatusCode") .= httpStatusCode, (T.pack "result") .= j']
