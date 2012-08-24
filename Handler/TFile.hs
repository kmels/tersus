{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE QuasiQuotes           #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeFamilies          #-}
module Handler.TFile where


import           Import
import           Tersus.AccessKeys
import           Handler.User(getValidUser)
{- Handler methods for operations on files. -}

-- A way to convert between urls and a file path.
-- See: Dynamic multi in http://www.yesodweb.com/book/routing-and-handlers
data TFilePath = TFilePath [Text]  -- 2 or more
instance PathMultiPiece TFilePath where
    toPathMultiPiece (TFilePath anyPath) = anyPath
    fromPathMultiPiece (p:ps) = Just $ TFilePath ([p] ++ ps)
    fromPathMultiPiece _ = Nothing

getFileR :: Text -> AccessKey -> Path -> Handler RepHtml
getFileR username' appKey path = do
  defaultLayout $ do
    [whamlet|
     <h1>Username: #{username'}
     <h1>AppKey: #{appKey}
     <h1>Path: #{show path}
            |]


-- Form to handle writing to files.
--
-- The **HTML** form (i.e. GET) is only used for testing reasons, the value is used to handle
-- requests coming from REST (i.e. POST)
--
--

--Use standard english (to i18n, supply a translation function)
data WFileLike = WFileLike{
  fileContentContent :: Text
  , fileLikeAccessKey :: AccessKey
} deriving Show

writeFileForm :: AccessKey -> Html -> MForm App App (FormResult WFileLike, Widget)
writeFileForm t = renderDivs $ WFileLike --TODO Implement security
    <$> areq textField "Content" Nothing
    <*> areq hiddenField "AccessKey" (Just t)

-- This won't really exist after, it is used for testing purposes only.
getWriteFileR :: Username -> AccessKey -> Path -> Handler RepHtml
getWriteFileR username' accessToken path = do
  --get the form
  let (userNickname,applicationName) = case decompose accessToken of
        Just (u,a) -> (Just u, Just a)
        _ -> (Nothing,Nothing)
  (formWidget, enctype) <- generateFormPost $ writeFileForm accessToken
  defaultLayout [whamlet|
     <h1>Dear user #{username'}, you are going to write content to file #{show path}
     <p> From your access key we can tell that you are: #{show userNickname} and are using #{show applicationName}
     <form method=post action=@{WriteFileR username' accessToken path} enctype=#{enctype}>
                  ^{formWidget}
                  <input type=submit>
     |]

-- Temporal function to test uploading of documents
postWriteFileR :: Username -> AccessKey -> Path -> Handler RepHtml
postWriteFileR username' accessToken path = do
  maybeValidUser <- getValidUser username' accessToken
  case maybeValidUser of
    Just user' -> do
      -- process form
      ((result, _), _) <- runFormPost $ writeFileForm accessToken
      --file <- runDB $ insert $ Email "asdf" (Just "zasdf") (Just "as")
      --  let file = user >>= \u -> Just $ TFile u
      case result of
        FormSuccess fc -> defaultLayout $ do
          [whamlet|
           <h1>Dear #{show $ user'}
           <h1>You wrote: #{show $ fileContentContent fc}
           <h3>in #{show path}
              |]
        _ -> defaultLayout [whamlet|<p>Invalid input!|]
   -- username in url doesn't exist
    Nothing -> defaultLayout $ do
        [whamlet|
         <h1> No user|]


{-  where
    -- | Returns a list of this file ancestors.
    fileAncestors :: Path -> [Maybe Id]
    fileAncestors xs = let x = 1 in []
-}
