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

getFileR :: Text -> TAppKey -> [Text] -> Handler RepHtml
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
getWriteFileR :: Username -> TAppKey -> [Text] -> Handler RepHtml
getWriteFileR username appKey path = do
  --get the form
  (formWidget, enctype) <- generateFormPost writeFileForm
  defaultLayout [whamlet|
     <h1>Dear user #{username}, you are going to write content to file #{show path}
     <form method=post action=@{WriteFileR username appKey path} enctype=#{enctype}>
                  ^{formWidget}
                  <input type=submit>
     |]

postWriteFileR :: Username -> TAppKey -> [Text] -> Handler RepHtml
postWriteFileR username appKey path = do
  ((result, widget), enctype) <- runFormPost writeFileForm
  case result of
    FormSuccess fc -> defaultLayout $ do
      [whamlet|
       <h1>You wrote: #{show $ fileContentContent fc}
       <h3>in #{show path}
              |]
    _ -> defaultLayout [whamlet|<p>Invalid input!|]
