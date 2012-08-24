{- |
Module      :  Handler.JavascriptAPI
Copyright   :  (C) 2012 Carlos López-Camey, Ernesto Rodríguez
License     :  GNU 2

Maintainer  :  <c.lopez@kmels.net>
Stability   :  stable

Responds javascript files, which are to be embedded in Tersus applications.
-}

module Handler.JavascriptAPI(
--  getJavascriptAPIR --GET /api.js
  ) where
  
import Import
import Text.Julius(juliusFile)

--getJavascriptAPIR :: Handler ChooseRep
--getJavascriptAPIR = defaultLayout $ do 
--  $(juliusFile "templates/JavascriptAPI/api.julius")
