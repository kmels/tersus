{-# OPTIONS_GHC -fno-warn-orphans #-}
module Model.TersusResultCode where

import           Data.Aeson as J
import qualified Data.Text  as T
import           Import

instance ToJSON TersusResultCode where
         toJSON Import.Success = (J.String (T.pack "Success"))
         --TODO Implement
         toJSON _ = (J.String (T.pack "TODO"))
