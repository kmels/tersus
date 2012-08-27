{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE OverloadedStrings         #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Main where

import           Application          (makeFoundation)
import           Import
import           Settings
import           Yesod.Default.Config
import           Yesod.Test

import           HomeTest

main :: IO a
main = do
    conf <- loadConfig $ (configSettings Testing) { csParseExtra = parseExtra }
    logger <- defaultDevelopmentLogger
    foundation <- makeFoundation conf
    app <- toWaiAppPlain foundation
    runTests app (connPool foundation) homeSpecs
