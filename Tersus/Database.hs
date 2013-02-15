{-# LANGUAGE ScopedTypeVariables #-}
module Tersus.Database where

import           Data.ByteString
import           Data.String
import           Data.Text
import qualified Data.Text as T
import           Data.Text.Encoding
import           Prelude

(<:>) :: String -> Text -> ByteString
s1 <:> s2 = encodeUtf8 $ (T.pack s1) `T.append` (':' `T.cons` s2)
