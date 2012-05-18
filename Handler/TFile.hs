module Handler.TFile where

import Import

{- Handler methods for operations on files. -}

-- A way to convert between urls and a file path.
-- See: Dynamic multi in http://www.yesodweb.com/book/routing-and-handlers
data TFilePath = TFilePath [Text]  -- 2 or more
instance PathMultiPiece TFilePath where
    toPathMultiPiece (TFilePath anyPath) = anyPath
    fromPathMultiPiece (p:ps) = Just $ TFilePath ([p] ++ ps)
    fromPathMultiPiece _ = Nothing

getFileR :: Int -> TAppKey -> [Text] -> Handler RepHtml
getFileR userId appKey path = do
  defaultLayout $ do
    [whamlet|
     <h1>User id: #{userId}
     <h1>AppKey: #{appKey}
     <h1>Path: #{show path}
            |]

getWriteFileR :: Int -> TAppKey -> [Text] -> Handler RepHtml
getWriteFileR userId appKey path = do
  defaultLayout $ do
    [whamlet|
     <h1>User id: #{userId}
     <h1>AppKey: #{appKey}
     <h1>Path: #{show path}
            |]
            
postWriteFileR :: Int -> TAppKey -> [Text] -> Handler RepHtml
postWriteFileR userId appKey path = do
  defaultLayout $ do 
    [whamlet|
     <h1>User id: #{userId}
     <h1>AppKey: #{appKey}
     <h1>Path: #{show path}
            |]