{-# LANGUAGE DoAndIfThenElse #-}

{- |
Module      :  Handler.User
Copyright   :  (C) 2012 Carlos López-Camey, Ernesto Rodríguez
License     :  GNU 2

Maintainer  :  <c.lopez@kmels.net>
Stability   :  stable

 - API calls that are related to user
 - A set of monadic and non monadic functions for handling users in requests
-}

module Handler.User(
  --API calls
  getLoggedUserR, --maybe json logged user
  getUserAccessKeyR, --(request) plain access key request for given username,appkey
  --Handlers
  getUsernameSearchR,
  --GHandler function helpers  
  requireSuperAdmin, --get a superadmin user
  requireAdminFor,
  verifyUserKey, --validate accesskey for given username
  verifyUserKeyM, --monadic version of verifyUserKey    
  verifyValidUser --match authkey with username
  ) where
  
import           Import

import           Data.Aeson(encode)
import           Data.Text                as T

import qualified Data.Conduit as C
import qualified Data.Conduit.List as CL
import           Data.Either (lefts,rights)

-- tersus
import           Tersus.HandlerMachinery

--monads
import           Control.Monad(guard)
import           Control.Monad.Trans.Maybe

--yesod
import           Yesod.Json(jsonToRepJson)
import           Yesod.Auth

import           Tersus.Global(accessKeyParameterName)

-- | Returns a JSON representation of the logged user. Returns a 412 status code (Precondition failed) with an empty string
getLoggedUserR :: Handler RepJson
getLoggedUserR = do
  tersus <- getYesod
  eitherUser <- maybeLoggedUser $ redisConnection tersus
  case eitherUser of
    Nothing -> permissionDenied $ "User not logged in"
    Just user -> jsonToRepJson user

-- | Returns the access key of a logged user. Returns a 412 status code (Precondition failed) with an empty string
-- TODO: This request *must* be secure (with https)
-- TODO: Change the parameter from ApplicationIdentifier to ApplicationKey (implies modifying newHexRandomAccessKey)
getUserAccessKeyR :: ApplicationIdentifier -> Handler RepJson
getUserAccessKeyR appId = do
  {-maybeUserId <- maybeAuth
  case maybeUserId of
    Just (Entity _ u) -> do
      accessKey <- liftIO $ newAccessKey (nickname u) appId
      jsonToRepJson $ accessKey
    Nothing -> error "Result: Empty user"-}
  permissionDenied "TODO"
      
-- | Returns Nothing iff the access key doesn't correspond to the given username. Returns a user if the access key is valid for the given username.
verifyValidUser :: Username -> AccessKey -> GHandler s m (Maybe User)
verifyValidUser u ak = do  
  {-userMaybe <- runDB $ getBy $ UniqueNickname $ u --query db
  case ak `verifyUserKey` u of
    Just uname -> do  
      case userMaybe of
        Just (Entity _ user') -> return $ Just user'
        Nothing -> return Nothing
    _ -> return Nothing-}
  permissionDenied "TODO"

-- | Returns Nothing iff the access key doesn't correspond to the given username. Returns the given username if the access key belongs to him.
verifyUserKey :: AccessKey -> Username -> Maybe Username
verifyUserKey key uname = decompose key >>= \(u',_) -> if (u'==uname) 
                                                     then Just uname
                                                     else Nothing
 
-- | A method wrapped in the GHandler monad, returning a Maybe u iff the logged user `u` has super admin rights
requireSuperAdmin :: GHandler s Tersus User
requireSuperAdmin = do
  conn <- getConn
  user <- requireLogin conn
  case (isSuperAdmin user) of
    True -> return user
    _ -> permissionDenied "Requires super admin powers"
    
-- | A method that returns Just the logged user if it has admin permissions over an application, Nothing otherwise
requireAdminFor :: ApplicationIdentifier -> GHandler s Tersus User
requireAdminFor appIdentifier = do 
  m <- getYesod 
  let conn = redisConnection m
  user <- requireLogin conn
  tapp <- io $ getTApplicationByName conn appIdentifier  
  either returnTError (checkAdmin user) tapp
  where
    checkAdmin user tapp = if (uid user `elem` owners tapp)
                            then return user
                            else permissionDenied "Permission denied. The logged user is not an application administrator"
    
-- | This function is written in the spirit of fromPersistValues in Database.Persist, it takes a list of persist values (equivalent to columns coming from a sql query) and extracts the ones who have only one element of text (intented to use in getUsernameSearchR because only the `nickname` field is SELECT'ed
--extractNickname :: [PersistValue] -> Either String Text
--extractNickname [PersistText x] = Right x
--extractNickname _ = Left $ "Input error on extractNickname: number of fields are more than one (ignoring)"

-- | Handler to autocomplete a user nickname from a query
getUsernameSearchR :: Text -> Handler RepJson
getUsernameSearchR query = do
  --TODO SECURITY IMPORTANT, prevent SQL INJECTION? PENDING REVIEW
  {-TODO let stripped_query = T.filter (/= '\'') query 
  let sql = "SELECT nickname FROM public.user WHERE nickname LIKE '%" `T.append` stripped_query `T.append` "%' LIMIT 5;" 
  extracts <- runDB $ C.runResourceT $ withStmt sql ([]::[PersistValue]) C.$= CL.map extractNickname C.$$ CL.consume
  jsonToRepJson . array . rights $ extracts-}
  permissionDenied "TODO" -- See Tersus.Auth

-- | Helper function that decomposes
requireValidAccessKey :: GHandler s m (Maybe AuthPair)
requireValidAccessKey = requireAccessKey >>= decomposeM

-- | Monadic version of `verifyUserKey`
verifyUserKeyM :: AccessKey -> Username -> GHandler s m (Maybe Username)
verifyUserKeyM ak u = return $ ak `verifyUserKey` u

