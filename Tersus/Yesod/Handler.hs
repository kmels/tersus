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

import Import
import qualified Data.Text as T

requireGetParameterOr :: Text -> GHandler s m Text -> GHandler s m Text
requireGetParameterOr name response = lookupGetParam name >>= maybe response return

missingParameter :: Text -> GHandler s m a
missingParameter name = invalidArgs $ [name `T.append` T.pack " parameter is missing"]

requireGetParameter :: Text -> GHandler s m Text
requireGetParameter n = n `requireGetParameterOr` (missingParameter n)