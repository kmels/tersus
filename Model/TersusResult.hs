{-# OPTIONS_GHC -fno-warn-orphans #-}
module Model.TersusResult where


import Import
import Data.Aeson as J
import qualified Data.Text as T

instance ToJSON TersusResultCode where
         toJSON resultCode = (J.String (T.pack (show resultCode)))

instance ToJSON TersusResult where
         toJSON (TersusResult httpStatusCode tersusCode) = J.object [(T.pack "httpStatusCode") .= httpStatusCode,
                                                                  (T.pack "tersusCode") .= tersusCode]
instance ToJSON TRequestError where
         toJSON (TRequestError httpStatusCode errorMessage) = J.object [(T.pack "httpStatusCode") .= httpStatusCode,
                                                                  (T.pack "message") .= errorMessage]
