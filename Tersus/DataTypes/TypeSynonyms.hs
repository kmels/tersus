module Tersus.DataTypes.TypeSynonyms where
import Data.ByteString
import Data.Text
import Prelude


type RawPath = Text
type AccessKey = Text


type Query = Text

-- text
type MaybeBS = Maybe ByteString

-- urls
type Path = [Text]

-- applications
type ApplicationKey = Text --private app key
type ApplicationIdentifier = Text  --this has the property that has no spaces in it, it goes in the url.

-- users
type UserId = Integer -- 64 bit integers
type Username = Text