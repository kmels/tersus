module Model.TersusResult where

import Import
import Data.Aeson as J
import Control.Monad
import Data.Functor
import Control.Applicative
import qualified Data.Text as T

instance ToJSON TersusResultCode where
         toJSON Import.Success = (J.String (T.pack "Success"))

instance ToJSON TersusResult where
         toJSON (TersusResult httpStatusCode tersusCode) = J.object [(T.pack "httpStatusCode") .= httpStatusCode,
                                                                  (T.pack "tersusCode") .= tersusCode]
