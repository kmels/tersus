{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE QuasiQuotes           #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeFamilies          #-}
module Handler.TFile where

import           Control.Applicative ((<$>), (<*>))
import           Data.Text           (Text)
import           Import
import           Yesod
import           Yesod.Form.Jquery

{- Handler methods for operations on files. -}

-- A way to convert between urls and a file path.
-- See: Dynamic multi in http://www.yesodweb.com/book/routing-and-handlers
data TFilePath = TFilePath [Text]  -- 2 or more
instance PathMultiPiece TFilePath where
    toPathMultiPiece (TFilePath anyPath) = anyPath
    fromPathMultiPiece (p:ps) = Just $ TFilePath ([p] ++ ps)
    fromPathMultiPiece _ = Nothing

getFileR :: Text -> TAppKey -> Path -> Handler RepHtml
getFileR username appKey path = do
  defaultLayout $ do
    [whamlet|
     <h1>Username: #{username}
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

data FileContent = FileContent{
  fileContentContent :: Text
} deriving Show

writeFileForm :: Html -> MForm App App (FormResult FileContent, Widget)
writeFileForm = renderDivs $ FileContent
    <$> areq textField "Content" Nothing

-- This won't really exist after, it is used for testing purposes only.
getWriteFileR :: Username -> TAppKey -> Path -> Handler RepHtml
getWriteFileR username appKey path = do
  --get the form
  (formWidget, enctype) <- generateFormPost writeFileForm
  defaultLayout [whamlet|
     <h1>Dear user #{username}, you are going to write content to file #{show path}
     <form method=post action=@{WriteFileR username appKey path} enctype=#{enctype}>
                  ^{formWidget}
                  <input type=submit>
     |]

postWriteFileR :: Username -> TAppKey -> Path -> Handler RepHtml
postWriteFileR username appKey path = do
  user <- runDB $ getBy $ UniqueNickname $ username --find user by username
  case user of
    -- Do we have a user?
    Just (Entity uid _ ) -> do
      -- process form
      ((result, widget), enctype) <- runFormPost writeFileForm
      --      file <- runDB $ insert $ Email "asdf" (Just "zasdf") (Just "as")
      --  let file = user >>= \u -> Just $ TFile u
      case result of
        FormSuccess fc -> defaultLayout $ do
          [whamlet|
           <h1>Lieber #{show $ user}
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
