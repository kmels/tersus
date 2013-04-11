----------------------------------------------------------------------------
-- |
-- Module      :  Tersus.Yesod.Handler
-- Copyright   :  (c) Carlos López-Camey, Ernesto Rodriguez
-- License     :  
--
-- Maintainer  :  c.lopez@kmels.net
-- Stability   :  stable
--
--
-- Helper functions for handlers
-----------------------------------------------------------------------------

module Tersus.Yesod.Handler where

import qualified Data.Text as T
import           Import
import           Tersus.DataTypes.TError
import           Tersus.Responses

requireGETParameter :: Text -> TError -> GHandler Tersus Tersus Text
requireGETParameter paramName e = lookupGetParam paramName >>= maybe (returnTError e) return

requirePOSTParameter :: Text -> TError -> GHandler Tersus Tersus Text
requirePOSTParameter paramName e = lookupPostParam paramName >>= maybe (returnTError e) return
