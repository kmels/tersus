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

module Tersus.Responses where

--json
import Data.Aeson(toJSON)
import Data.Aeson(ToJSON)
--yesod 
import Import

--tersus
import Model.TersusResult

invalidArguments :: Text -> Handler RepJson
invalidArguments t = jsonToRepJson $ TRequestResponse RequestError (Message t)

entityCreated :: (ToJSON val) => val -> Handler RepJson
entityCreated e = jsonToRepJson $ TRequestResponse Success (JsonResult $ toJSON e)
