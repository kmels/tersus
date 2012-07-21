{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE QuasiQuotes           #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeFamilies          #-}
module Handler.TFile where


import           Import

{- Handler methods for operations on files. -}

-- A way to convert between urls and a file path.
-- See: Dynamic multi in http://www.yesodweb.com/book/routing-and-handlers
data TFilePath = TFilePath [Text]  -- 2 or more
instance PathMultiPiece TFilePath where
    toPathMultiPiece (TFilePath anyPath) = anyPath
    fromPathMultiPiece (p:ps) = Just $ TFilePath ([p] ++ ps)
    fromPathMultiPiece _ = Nothing

getFileR :: Text -> AccessToken -> Path -> Handler RepHtml
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
  , fileLikeAccessToken :: AccessToken
} deriving Show

writeFileForm :: AccessToken -> Html -> MForm App App (FormResult WFileLike, Widget)
writeFileForm _ = renderDivs $ WFileLike --TODO Implement security
    <$> areq textField "Content" Nothing
    <*> areq hiddenField "AccessToken" Nothing

-- This won't really exist after, it is used for testing purposes only.
getWriteFileR :: Username -> AccessToken -> Path -> Handler RepHtml
getWriteFileR username' accessToken path = do
  --get the form
  (formWidget, enctype) <- generateFormPost $ writeFileForm accessToken
  defaultLayout [whamlet|
     <h1>Dear user #{username'}, you are going to write content to file #{show path}
     <form method=post action=@{WriteFileR username' accessToken path} enctype=#{enctype}>
                  ^{formWidget}
                  <input type=submit>
     |]

-- | Returns the app responsible for the request, from the AccessToken,
-- this is our security layer used for incoming requests.
--decryptUserAppAuth :: User -> AccessToken -> B.ByteString
--decryptUserAppAuth u a = aesUserAppKey u a AES.Decrypt



  --username = usernameNickname user
postWriteFileR :: Username -> AccessToken -> Path -> Handler RepHtml
postWriteFileR username' accessToken path = do
  user' <- runDB $ getBy $ UniqueNickname $ username' --find user by username
  case user' of
    -- Do we have a user?
    Just (Entity _ _ ) -> do
      -- process form
      ((result, _), _) <- runFormPost $ writeFileForm accessToken
      --      file <- runDB $ insert $ Email "asdf" (Just "zasdf") (Just "as")
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
