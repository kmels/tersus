{-# OPTIONS_GHC -fno-warn-orphans #-}
module Model.TersusResult where

import           Data.Aeson             as J
import qualified Data.Text              as T
import           Import
import           Model.TersusResultCode ()

instance ToJSON TersusResult where
         toJSON (TersusResult httpStatusCode tersusCode) = J.object [(T.pack "httpStatusCode") .= httpStatusCode,
                                                                  (T.pack "tersusCode") .= tersusCode]
