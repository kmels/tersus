module Model where

import           Prelude
import           Yesod
import qualified Data.Text as T
import           Data.Text(Text)

import           Database.Persist.Quasi

import           Database.Persist.Store(PersistValue(..),SqlType(..))

import           Data.Time(Day,TimeOfDay)
-- You can define all of your database entities in the entities file.
-- You can find more information on persistent and how to declare entities
-- at:
-- http://www.yesodweb.com/book/persistent/
type TAppKey = Text
type Username = Text
type RawPath = Text
type Path = [Text]
type Id = Double
type IdList = [Id]
type ApplicationName = Text

data WriteMode = Override | AppendToFile | Create | Delete deriving (Show, Eq, Enum)

data ReadMode = FileMetadata | GetContent | Pagination deriving (Show, Eq, Enum)


data FileType = File | Directory deriving Show

instance PersistField FileType where
  toPersistValue = PersistText . T.pack . Prelude.show
  fromPersistValue (PersistText s) = case (T.unpack s) of 
    "File" -> Right $ File
    "Directory" -> Right $ Directory
    _ -> Left $ "Expected File or Directory"
  fromPersistValue _ = Left $ "Expected PersistText as PersistValue for FileType"
  sqlType _ = SqlString

data TersusResultCode = Success | InexistentFile | NotEnoughPrivileges | DirectoryNotEmpty | OutOfRange deriving (Show, Eq, Enum)

data TersusResult = TersusResult Int TersusResultCode

data MessageResult = Read | NotRead

share [mkSave "myDefs", mkPersist sqlSettings, mkMigrate "migrateAll"] $(persistFileWith lowerCaseSettings "config/models")