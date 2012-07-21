module Tersus.AccessKeys where

import           Import
import           System.Random            (newStdGen, randomRs)
import qualified Codec.Binary.UTF8.String as UTF8
import qualified Codec.Crypto.AES         as AES
import qualified Data.ByteString          as B
import           Data.Text                as T
import qualified Data.Text.Encoding       as TE
import           Data.Char(chr)

-- decrypts
--aesUserAppKey :: User -> AccessToken -> AES.Direction -> Maybe T.Text
--aesUserAppKey _ appkey dir = do
  --splitIndex <- T.findIndex ((==) ':') $ TE.decodeUtf8 decrypteLEdRawAuth
  --(_,applicationName) <- Just $ B.splitAt splitIndex decryptedRawAuth
  --return $ TE.decodeUtf8 applicationName
aes :: T.Text -> AES.Direction -> B.ByteString
aes message dir = 
  let
    aesKey = B.pack . UTF8.encode $ "01234567890abcde" --16,24 or 32 byte symmetric key
    aesIV = B.pack . UTF8.encode $ "tersus>>=lorenzo" -- initialization vector, 16 byte
  --in T.pack $ UTF8.decode $ B.unpack $ AES.crypt' AES.CTR aesKey aesIV dir (B.pack $ UTF8.encode $ T.unpack $ message)
    in AES.crypt' AES.CTR aesKey aesIV dir (TE.encodeUtf8 message)

-- | Generate a random access key for a given user nickname and an application name
-- this will the key that an application will use to make requests
newRandomAccessKey :: Username -> ApplicationName -> IO AccessToken
newRandomAccessKey user appName = do
  randomText <- newRandomKey 16
  let
    sep = T.pack ":"
    encodedAuth = user `append` sep `append` randomText `append` sep `append` appName
    keyBinary = aes encodedAuth AES.Encrypt 
    wordToChr = chr . fromEnum
    in return $ T.pack $ Import.map wordToChr $ B.unpack keyBinary

-- | Generate a n-length random alphanumerical text.
-- The text contains the caracter sets (a-z, A-Z, and 0-9)
-- (Copied from hidden module Yesod.Internal.Request)
newRandomKey :: Int -> IO Text
newRandomKey n = do
  stdgen <- liftIO newStdGen
  return $ T.pack $ Import.take n $ Import.map toChar (randomRs (0, 61) stdgen) --32 chars
  where
    toChar i
      | i < 26 = toEnum $ i + fromEnum 'A'
      | i < 52 = toEnum $ i + fromEnum 'a' - 26
      | otherwise = toEnum $ i + fromEnum '0' - 52