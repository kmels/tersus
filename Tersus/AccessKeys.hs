module Tersus.AccessKeys where

import qualified Codec.Binary.UTF8.String   as UTF8
import qualified Codec.Crypto.AES           as AES
import qualified Data.ByteString            as B
import           Data.ByteString.Base64.URL as Base64
import           Data.Char                  (chr, ord)
import           Data.List.Split            (splitOn)
import           Data.Text                  as T
import qualified Data.Text.Encoding         as TE
import           Data.Time.Clock            (UTCTime, getCurrentTime)
import           Data.Word                  (Word8)
import           Import
import           System.Random              (newStdGen, randomRs)
import Data.Maybe(fromJust)

import Tersus.Global(accessKeyParameterName)

--models
import Tersus.DataTypes

--monads/control
import Control.Monad.Trans.Maybe

--types
type AuthPair = (Username,ApplicationIdentifier)

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
    in AES.crypt' AES.CBC aesKey aesIV dir data' --run AES

-- | The separation string used to separate data in appKeys. This must
-- be a string that is not allowed in the username, appname or that
-- dosen't appear in a date string.
keySeparator :: String
keySeparator = "$"

-- | Extract a UserNickname and a ApplicationIdentifier from a hexadecimal access key
decompose :: AccessKey -> Maybe AuthPair
decompose key = do  
  case (Data.List.Split.splitOn keySeparator encodedAuth) of
    [random,nickname,appname,_] -> do
      Just (T.pack nickname,T.pack appname)
    _ -> Nothing
  where
    decodedAccessKey :: B.ByteString
    --Let's don't care about invalid chars, since the decrypted key must have the right structure anyway
    decodedAccessKey = case Base64.decode $ textToBytestring key of
      Right b -> if (B.length b `mod` 16 ==0) -- aes with CBC can only handle 16-byte frames
                 then b
                 else B.empty
      _ -> B.empty

    encodedAuth :: String -- if valid, a string of the form "usernickname:ramdomstr:appname"
    encodedAuth = T.unpack $ bytestringToText $ aes decodedAccessKey AES.Decrypt

-- | Wraps decomposed access key in a GHandler monad
decomposeM :: AccessKey -> GHandler s m (Maybe AuthPair)
decomposeM = return . decompose

-- | Returns an invalid access key response if the given access key doesn't decode a well formed auth pair
reqValidAuthPair :: AccessKey -> GHandler s m AuthPair
reqValidAuthPair ak = maybe invalidAccessKey return (decompose ak)

-- | The size of the access key. Must be a multiple of 64 because that's the AES block size
keySize :: Int
keySize = 128

-- | Given a username and an application name, this function generates a new hexagesimal (base 16) random string.
newAccessKey :: Username -> ApplicationIdentifier -> IO AccessKey
newAccessKey user appIdentifier = do
  currentTime <- getCurrentTime >>= return . T.pack . show
  let
    sep = T.pack keySeparator
    dataToEncode = sep `append` user `append` sep `append` appIdentifier `append` sep `append` currentTime
    randTextSize = keySize - (T.length dataToEncode)

  randomText <- newRandomKey randTextSize
  let
    encodedAuth = randomText `append` dataToEncode
    randomHexBytestring = Base64.encode $ aes (textToBytestring encodedAuth) AES.Encrypt --ByteString cypher (encrypted by AES)
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

-- | GHandler helper that gets the access key from the GET request parameters. If it is missing, returns an invalid arguments response.
requireAccessKey :: GHandler s m AccessKey
requireAccessKey = lookupGetParam accessKeyParameterName >>= maybe accessKeyRequired return

-- | GHandler helper that gets the access key from the GET request parameters.
maybeAccessKey :: GHandler s m (Maybe AccessKey)
maybeAccessKey = lookupGetParam accessKeyParameterName

-- | Request response that indicates that the access_key is missing
accessKeyRequired :: GHandler s m a
accessKeyRequired = invalidArgs $ [accessKeyParameterName]

invalidAccessKey :: GHandler s m a
invalidAccessKey = invalidArgs $ [accessKeyParameterName]

-- | Returns an invalidAccessKey response if it can't deduce a tuple (user,application) from a given access key
requireValidAuthPair :: AccessKey -> GHandler s Tersus (User, TApplication)
requireValidAuthPair ak = do
  accessKey <- requireAccessKey
  authPair <- reqValidAuthPair accessKey 
  master <- getYesod
  let conn = redisConnection master
  
  eitherUser <- liftIO $ getUserByNickname conn $ fst authPair
  eitherTApp <- liftIO $ getTApplicationByIdentifier conn $ snd authPair
    
  -- TODO : \todo -> invaliAccessKey should be replaced by a function
  -- of type TError -> GHandler s m RepJson 
  
  user <- either (\todo -> invalidAccessKey) return eitherUser
  tapp <- either (\todo -> invalidAccessKey) return eitherTApp
  return $ (user,tapp)

