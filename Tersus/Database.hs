{-# LANGUAGE ScopedTypeVariables #-}
module Tersus.Database where

import           Data.ByteString
import qualified           Data.ByteString as B
import           Data.String
import           Data.Text
import qualified Data.Text as T
import           Data.Text.Encoding
import           Prelude

sep :: Char
sep = '$'

--(<.>) :: String -> Text -> ByteString
--s1 <:> s2 = encodeUtf8 $ (T.pack s1) `T.append` (sep `T.cons` s2)

(.>) :: String -> ByteString -> ByteString
(.>) s b = (encodeUtf8 . T.pack $ s ++ [sep]) `B.append` b

(<.) :: ByteString -> String -> ByteString
(<.) b s = b `B.append` (encodeUtf8 . T.pack $ sep:s)
