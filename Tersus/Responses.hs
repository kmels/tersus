{-# LANGUAGE UndecidableInstances #-}
----------------------------------------------------------------------------
-- |
-- Module      :  Tersus.Responses
-- Copyright   :  (c) Carlos López-Camey, Ernesto Rodriguez
-- License     :
--
-- Maintainer  :  c.lopez@kmels.net,neto@netowork.me
-- Stability   :  stable
--
--
-- Helper functions for request responses
-----------------------------------------------------------------------------

module Tersus.Responses(  
  reply, replyResult, replyJsonContent,
  fileDoesNotExist,
  tError,
  returnHtml, returnTError
) where

--json
import           Data.Aeson                 (toJSON)
import           Data.Aeson                 (ToJSON)
--import Yesod.Json                   (Value (..))
--yesod
--import           Import
import           Prelude
import           Yesod.Content
import           Yesod.Handler
import           Yesod.Json
--tersus
import           Tersus.DataTypes.Responses
--types
import           Control.Exception.Base
import           Control.Monad.IO.Class (liftIO)
import           Data.Text
import qualified Data.Text                  as T
import           Tersus.DataTypes.TError
{-invalidArguments :: Text -> GHandler s m RepJson
invalidArguments t = jsonToRepJson $ TRequestResponse RequestError (Message t)

entityCreated :: (ToJSON val) => val -> GHandler s m RepJson
entityCreated e = jsonToRepJson $ TRequestResponse Success (JsonResult $ toJSON e)

entityExists :: (ToJSON val) => val -> GHandler s m RepJson
entityExists e = jsonToRepJson $ TRequestResponse SuccessDontUpdate (JsonResult $ toJSON e)

entityDeleted :: (ToJSON val) => val -> GHandler s m RepJson
entityDeleted e = jsonToRepJson $ TRequestResponse Success (JsonResult $ toJSON e)
-}

--errorResponse :: TError -> Handler RepJson
--errorResponse e = jsonToRepJson $ TRequestResponse RequestError (JsonResult $ toJSON e)

-- files

-- | Returns the json content type according to RFC 4627
jsonContentType :: ContentType
jsonContentType = "application/json"
  
fileDoesNotExist :: TResponse
fileDoesNotExist = TResult InexistentFile 

reply :: TResponse -> GHandler s m (ContentType,Content) 
reply (TResult result) = sendResponseStatus (mkResultStatus result) (typeJson, toContent . toJSON $ result)
reply r@(TResponse result text) = sendResponseStatus (mkResultStatus result) (typeJson, toContent . toJSON $ r)


replyResult :: TResult -> GHandler s m (ContentType,Content) 
replyResult = reply . TResult

replyJsonContent :: (ToContent c) => c -> GHandler s m (ContentType,Content)
replyJsonContent c = return (jsonContentType, toContent c)

--fileDoesNotExistError = TersusErrorResult InexistentFile "File does not exist"

fileDoesNotExistErrorResponse = reply fileDoesNotExist

returnTError :: TError -> GHandler s m a
returnTError e = sendResponseStatus (mkResultStatus . TError $ e) (typeJson, toContent . toJSON $ e)

--reply . TResult . TError

--liftIO . throwIO
--reply (TResult . TError $ e)

--permissionDenied $ T.pack . show $ e

-- | Return a 403 permission denied page.
tError :: TError -> GHandler sub master RepJson
tError terr = jsonToRepJson $ show terr

returnHtml :: Content -> GHandler s m (ContentType,Content)
returnHtml c = return ("text/html",c)
