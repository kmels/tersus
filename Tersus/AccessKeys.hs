module Tersus.AccessKeys where

import qualified Codec.Binary.UTF8.String as UTF8
import qualified Codec.Crypto.AES         as AES
import qualified Data.ByteString          as B
import           Data.ByteString.Base16   as Base16
import           Data.Char                (chr, ord)
import           Data.List.Split          (splitOn)
import           Data.Text                as T
import qualified Data.Text.Encoding       as TE
import           Data.Word                (Word8)
import           Import
import           System.Random            (newStdGen, randomRs)


-- | Converts UTF8 text to a Bytestring
textToBytestring :: T.Text -> B.ByteString
textToBytestring = B.pack . UTF8.encode . T.unpack

-- | Converts bytestring to a UTF8 text
bytestringToText :: B.ByteString -> T.Text
bytestringToText = T.pack . UTF8.decode . B.unpack

-- | Encrypts or decrypts a bytestring to a bytestring with the AES (Advanced Encryption Standard) algorithm
aes :: B.ByteString -> AES.Direction -> B.ByteString
aes data' dir =
  let
    aesKey = B.pack . UTF8.encode $ "01234567890abcde" --16,24 or 32 byte symmetric key
    aesIV = B.pack . UTF8.encode $ "tersus>>=lorenzo" -- initialization vector, 16 byte
    in AES.crypt' AES.CTR aesKey aesIV dir data' --run AES

-- | Extract a UserNickname and a ApplicationIdentifier from a hexadecimal access key
decompose :: AccessKey -> Maybe (Username,ApplicationIdentifier)
decompose key = do
  case (Data.List.Split.splitOn ":" encodedAuth) of
    [random,nickname,appname] -> do
      Just (T.pack nickname,T.pack appname)
    _ -> Nothing
  where
    decodedAccessKey :: B.ByteString
    --Let's don't care about invalid (we're using fst) chars, since the decrypted key must have the right structure anyway
    decodedAccessKey = fst $ Base16.decode $ textToBytestring key
    encodedAuth :: String -- if valid, a string of the form "usernickname:ramdomstr:appname"
    encodedAuth = T.unpack $ bytestringToText $ aes decodedAccessKey AES.Decrypt

-- | Wraps decomposed access key in a GHandler monad
decomposeM :: AccessKey -> GHandler s m (Maybe (Username,ApplicationIdentifier))
decomposeM = return . decompose

-- | Given a username and an application name, this function generates a new hexagesimal (base 16) random string.
newHexRandomAccessKey :: Username -> ApplicationIdentifier -> IO AccessKey
newHexRandomAccessKey user appIdentifier = do
  randomText <- newRandomKey 16
  let
    sep = T.pack ":"
    encodedAuth = randomText `append` sep `append` user `append` sep `append` appIdentifier
    randomHexBytestring = Base16.encode $ aes (textToBytestring encodedAuth) AES.Encrypt --ByteString cypher (encrypted by AES)
  liftIO $ putStrLn $ show $ "****************************************\nGenerated access key: "++ (T.unpack $ bytestringToText randomHexBytestring)
  return $ bytestringToText randomHexBytestring


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
