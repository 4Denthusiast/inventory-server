{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeOperators #-}
module PlayerInterface (
    PlayerAPI,
    playerServer
) where

import Data

import Control.Concurrent
import Control.Concurrent.MVar
import Control.Monad
import Control.Monad.Error.Class
import Control.Monad.IO.Class
import Data.Char
import qualified Data.Map.Lazy as M
import Data.Maybe
import qualified Data.Set as S
import GHC.Generics
import Network.HTTP.Types.Header hiding (Header)
import Servant
import Web.Internal.FormUrlEncoded

data CharacterSubmission = CharacterSubmission {
  csName :: String,
  csDesc :: String,
  csTall :: Double,
  csMuscle :: Double,
  csFat :: Double,
  csImpulse :: Double,
  csEmpathy :: Double
} deriving (Generic, Show)

instance FromForm CharacterSubmission

type PlayerAPI = AddCharacterAPI
  :<|> ItemUpdatesAPI
  :<|> ChangeListeningAPI
type AddCharacterAPI = "add-character" :> ReqBody '[FormUrlEncoded] CharacterSubmission :> Verb 'POST 303 '[PlainText] (Headers '[Header "Location" String] String)
type ItemUpdatesAPI = "listen-updates" :> QueryParam "name" String :> ReqBody '[JSON] [(ID,Int)] :> Post '[JSON] [(ID,Item,Int)]
type ChangeListeningAPI = "change-listening" :> QueryParam "name" String :> ReqBody '[JSON] [(ID,Int)] :> PostNoContent

playerServer :: MVar World -> Server PlayerAPI
playerServer worldVar = addCharacterServer worldVar :<|> itemUpdatesServer worldVar :<|> changeListeningServer worldVar

addCharacterServer :: MVar World -> Server AddCharacterAPI
addCharacterServer worldVar char = liftIO (putStrLn (show char)) >> return (addHeader url ("Please proceed to "++url))
  where simpleName = map toLower $ takeWhile (/=' ') $ csName char --TODO: sanitise this more, and make it explicitly avoid overlaps.
        url = "/player.html?name="++simpleName

itemUpdatesServer :: MVar World -> Server ItemUpdatesAPI
itemUpdatesServer worldVar (Just name) clientWants = do
  clientState <- changeListening worldVar name clientWants
  idList <- liftIO $ queuePopTimeout 30000000 $ itemUpdateQueue clientState
  world' <- liftIO $ readMVar worldVar --Explicitly refresh this because queuePop is designed to block so this read will execute much later in general.
  return $ mapMaybe (\i -> (i,,) <$> M.lookup i (itemStore world') <*> M.lookup i (itemNewness world')) idList
itemUpdatesServer _ Nothing _ = throwError $ ServerError 422 "Unprocessable Entity" "Could not process request due to missing \"name\" parameter." []

changeListeningServer :: MVar World -> Server ChangeListeningAPI
changeListeningServer worldVar (Just name) clientWants = do
  changeListening worldVar name clientWants
  return NoContent
changeListeningServer _ Nothing _ = throwError $ ServerError 422 "Unprocessable Entity" "Could not process request due to missing \"name\" parameter." []

changeListening :: (MonadIO m, MonadError ServerError m) => MVar World -> String -> [(ID,Int)] -> m ClientState
changeListening worldVar name clientWants = do
  mWorldClient <- liftIO $ modifyMVar worldVar $ \world -> return $ case M.lookup name (clientStates world) of {Nothing -> (world,Nothing); Just cs -> (world{clientStates = M.insert name cs{itemsListened = S.fromList (map fst clientWants), clientTimeout = 0} (clientStates world)}, Just (world, cs))}
  case mWorldClient of
    Nothing -> throwError $ ServerError 422 "Unprocessable Entity" "Could not process request due to unrecognised value of \"name\" parameter." []
    Just (world, clientState) -> do --The outdated version of world is used here, but this does not affect any of the values used.
      let outdated = map fst (filter (\(i,t) -> maybe False (>t) $ M.lookup i $ itemNewness world) clientWants) ++ if take 3 name /= "gm " then [] else filter (\i -> not $ S.member i $ S.fromList $ map fst clientWants) (locationList world)
      liftIO $ forM_ outdated $ enqueue (itemUpdateQueue clientState)
      return clientState
