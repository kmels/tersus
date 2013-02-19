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
requireGetParameterOr :: Text -> GHandler s m Text -> GHandler s m Text
requireGetParameterOr name response = lookupGetParam name >>= maybe response return

missingParameter :: Text -> GHandler s m a
missingParameter name = invalidArgs $ [name `T.append` T.pack " parameter is missing"]

requireGetParameter :: Text -> GHandler s m Text
requireGetParameter n = n `requireGetParameterOr` (missingParameter n)

requireParameterOr :: Text -> TError -> GHandler Tersus Tersus Text
requireParameterOr paramName e = lookupGetParam paramName >>= maybe (returnTError e) return
