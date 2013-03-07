{-# LANGUAGE ScopedTypeVariables #-}
module Tersus.Database where

import           Control.Monad.IO.Class
import           Data.ByteString
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as Char8
import           Data.String
import           Data.Text
import qualified Data.Text as T
import           Data.Text.Encoding
import           Database.Redis
import           Prelude
import           System.IO.Unsafe
import           Yesod.Handler
sep :: Char
sep = '$'

--(<.>) :: String -> Text -> ByteString
--s1 <:> s2 = encodeUtf8 $ (T.pack s1) `T.append` (sep `T.cons` s2)

(.>) :: String -> ByteString -> ByteString
(.>) s b = (encodeUtf8 . T.pack $ s ++ [sep]) `B.append` b

(<.) :: ByteString -> String -> ByteString
(<.) b s = b `B.append` (encodeUtf8 . T.pack $ sep:s)

(<.>) :: String -> String -> ByteString
(<.>) s s' = Char8.pack $ s ++ sep:s'

foldEitherToMaybe :: Either a b -> Maybe b
foldEitherToMaybe (Left _) = Nothing
foldEitherToMaybe (Right b') = Just b'

byteStringToInteger :: ByteString -> Integer
byteStringToInteger = read . Char8.unpack

integerToByteString :: Integer -> ByteString
integerToByteString = Char8.pack . show

-- | Equivalent to join . foldEitherToMaybe
getRedisResponse :: Either Reply (Maybe e) -> Maybe e
getRedisResponse (Left _) = Nothing
getRedisResponse (Right me) = me

io = liftIO
uio = unsafePerformIO

