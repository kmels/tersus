----------------------------------------------------------------------------
-- |
-- Module      :  Tersus.Yesod.Responses
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
  fileDoesNotExistError, fileDoesNotExistErrorResponse,
  tError,
  returnHtml, returnTError
) where

--json
import           Data.Aeson                 (toJSON)
import           Data.Aeson                 (ToJSON)
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
import           Data.Text
import qualified Data.Text                  as T
import           Tersus.DataTypes.TError

invalidArguments :: Text -> GHandler s m RepJson
invalidArguments t = jsonToRepJson $ TRequestResponse RequestError (Message t)

entityCreated :: (ToJSON val) => val -> GHandler s m RepJson
entityCreated e = jsonToRepJson $ TRequestResponse Success (JsonResult $ toJSON e)

entityExists :: (ToJSON val) => val -> GHandler s m RepJson
entityExists e = jsonToRepJson $ TRequestResponse SuccessDontUpdate (JsonResult $ toJSON e)

entityDeleted :: (ToJSON val) => val -> GHandler s m RepJson
entityDeleted e = jsonToRepJson $ TRequestResponse Success (JsonResult $ toJSON e)


--errorResponse :: TError -> Handler RepJson
--errorResponse e = jsonToRepJson $ TRequestResponse RequestError (JsonResult $ toJSON e)

-- files
fileDoesNotExistError :: TersusResult
fileDoesNotExistError = TersusErrorResult InexistentFile "File does not exist"

fileDoesNotExistErrorResponse = return $ (typeJson, toContent . toJSON $ fileDoesNotExistError)

returnTError :: TError -> GHandler s m a
returnTError e = permissionDenied $ T.pack . show $ e

-- | Return a 403 permission denied page.
tError :: TError -> GHandler sub master RepJson
tError terr = jsonToRepJson $ show terr

returnHtml :: Content -> GHandler s m (ContentType,Content)
returnHtml c = return ("text/html",c)
