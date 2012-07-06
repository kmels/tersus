{-# OPTIONS -fno-warn-orphans #-}
{-# LANGUAGE ExtendedDefaultRules  #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
-- This module provides functions to interact with the virtual file system.
-- They're, when they can, mainly conduits.

module TFile.FilesystemUtils(
--  fileAncestors
  ) where

--import           Data.List
--import           Foundation
--import           Model
--import           Prelude
--import           Yesod

-- | Returns a list of this file ancestors.
--fileAncestors :: Path -> [Maybe Id]
--fileAncestors xs = map pathId $ inits xs

-- | Get a path id
{-pathId :: Path -> Maybe Id
pathId p = do
  file <- runDB $ getBy $ UniquePath $ rawPath p
  case file of
    Just f -> Just $ tFileUser file
    _ -> Nothing
-}

-- | Converts a list that represents a path to a simple Text
--rawPath :: Path -> RawPath
--rawPath xs = T.intercalate (T.pack "/") xs
