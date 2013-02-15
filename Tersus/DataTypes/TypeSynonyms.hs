module Tersus.DataTypes.TypeSynonyms where
import Data.ByteString
import Data.Text
import Prelude
type ApplicationIdentifier = Text  --this has the property that has no spaces in it, it goes in the url.
type Username = Text
type RawPath = Text
type AccessKey = Text
type UserId = Integer
type ApplicationKey = Text --private app key
type Query = Text