module Model.TMessage where

import Import
import Data.Aeson as J
import Control.Monad
import Data.Functor
import Control.Applicative
import qualified Data.Text as T

instance ToJSON TMessage where
         toJSON (TMessage userSender userReciever appSender appReciever content) = (J.String content)
