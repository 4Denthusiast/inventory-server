{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}
module PlayerInterface (
    PlayerAPI,
    playerServer
) where

import Data
import Model

import Control.Concurrent
import Control.Monad
import Control.Monad.Error.Class
import Control.Monad.IO.Class
import Data.Char
import Data.List
import qualified Data.Map.Lazy as M
import Data.Maybe
import qualified Data.Set as S
import GHC.Generics
import Servant
import Web.Internal.FormUrlEncoded

data CharacterSubmission = CharacterSubmission {
  csName :: String,
  csDesc :: String,
  csTall :: Double,
  csMuscle :: Double,
  csFat :: Double,
  csImpulse :: Double,
  csEmpathy :: Double,
  csImprov :: Double,
  csExpertise :: Int
} deriving (Generic, Show)

instance FromForm CharacterSubmission

type PlayerAPI = AddCharacterAPI
  :<|> ItemUpdatesAPI
  :<|> ChangeListeningAPI
  :<|> MoveItemAPI
  :<|> MoveCommodityAPI
type AddCharacterAPI = "add-character" :> ReqBody '[FormUrlEncoded] CharacterSubmission :> Verb 'POST 303 '[PlainText] (Headers '[Header "Location" String] String)
type ItemUpdatesAPI = "listen-updates" :> QueryParam "name" String :> ReqBody '[JSON] [(ID,Int)] :> Post '[JSON] [(ID,Item,Int)]
type ChangeListeningAPI = "change-listening" :> QueryParam "name" String :> ReqBody '[JSON] [(ID,Int)] :> PostNoContent
type MoveItemAPI = "move-item" :> ReqBody '[JSON] (ID,ID) :> PostNoContent
type MoveCommodityAPI = "move-commodity" :> ReqBody '[JSON] (ID, String, Double, Bool, ID) :> PostNoContent

playerServer :: MVar World -> Server PlayerAPI
playerServer worldVar = addCharacterServer worldVar :<|> itemUpdatesServer worldVar :<|> changeListeningServer worldVar :<|> moveItemServer worldVar :<|> moveCommodityServer worldVar

addCharacterServer :: MVar World -> Server AddCharacterAPI
addCharacterServer worldVar char = do
  name <- addCharacter worldVar char
  let url = "/player.html?name="++name
  return (addHeader url ("Please proceed to "++url))

addCharacter :: (MonadIO m) => MVar World -> CharacterSubmission -> m String
addCharacter worldVar char = do
    name <- findName (name0:map (\n -> name0 ++ show n) [0 :: Int ..])
    liftIO $ modifyMVar_ worldVar $ \world -> return world{playerStates = M.insert name (PlayerState (-1)) (playerStates world)}
    updateWorld worldVar $ do
      body <- addItem $ makeBody name char
      mSpawn <- spawnPoint <$> getWorld
      forM_ mSpawn $ modifyItemAttribute @"container" (++[ItemRef body])
    return name
  where name0 = map toLower $ takeWhile (/=' ') $ csName char --TODO: sanitise this more.
        findName (n:ns) = do
          success <- addClient worldVar n False
          if success then return n else findName ns

makeBody :: String -> CharacterSubmission -> Item
makeBody name CharacterSubmission{
    csName = fullName,
    csDesc = desc,
    csTall = tallScore,
    csMuscle = muscleScore,
    csFat = fatScore,
    csImpulse = impulseScore,
    csEmpathy = empathyScore,
    csImprov = improvScore,
    csExpertise = expertiseIndex
  } = setAtt @"name" fullName $
      setAtt @"desc" desc $
      setAtt @"containerInline" [
        ItemI $
        setAtt @"name" "Clothing" $
        setAtt @"desc" ("Whatever clothing "++fullName++" happened to be wearing upon arrival.") $
        setAtt @"componentsInline" [
          CommodityI "fibre" 2000 --TODO: customise the weight of the clothing based on how big the character is.
        ] []
      ] $
      setAtt @"componentsInline" [
        ItemI $
        setAtt @"name" "Head" $
        setAtt @"origin" fullName $
        setAtt @"componentsInline" [
          ItemI $
          setAtt @"name" "Brain" $
          setAtt @"origin" fullName $
          setAtt @"componentsInline" [
            --nerves
            mind
          ] []
          --mouth
          --eye
          --eye
          --nose
          --ear
          --ear
          --bone
          --meat
          --blood
          --sinew
          --nerves
          --fat
          --hair
        ] []
        --torso
        --arm
        --arm
        --leg
        --leg
      ] []
  where mind = ItemI $
               setAtt @"name" "Mind" $
               setAtt @"origin" fullName $
               setAtt @"componentsInline" [
                 ItemI $
                 setAtt @"name" "Transcendent Self" $
                 setAtt @"origin" fullName $
                 setAtt @"soul" name []
                 --instinct
                 --passions
                 --conscience
                 --rationality
                 --habit
                 --knowledge
                 --sensorium
                 --cortical homunculus
               ] []

itemUpdatesServer :: MVar World -> Server ItemUpdatesAPI
itemUpdatesServer worldVar (Just name) clientWants = do
    liftIO $ replaceListeningThread
    clientState <- changeListening worldVar name clientWants
    idList <- liftIO $ queuePopTimeout 30000000 $ itemUpdateQueue clientState
    world' <- liftIO $ readMVar worldVar --Explicitly refresh this because queuePop is designed to block so this read will execute much later in general.
    return $ mapMaybe (\i -> (i,,) <$> M.lookup i (itemStore world') <*> M.lookup i (itemNewness world')) $ nub idList
  where replaceListeningThread = modifyMVar_ worldVar $ \world -> case M.lookup name (clientStates world) of
          Just cs -> do
            maybe (return ()) killThread (clientListeningThread cs)
            t <- myThreadId
            return world{clientStates = M.insert name cs{clientListeningThread = Just t} (clientStates world)}
          Nothing -> putStrLn ("Error, unknown user "++name) >> return world
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
      let mPlayer = M.lookup name $ playerStates world
      let outdated = map fst (filter (\(i,t) -> maybe False (>t) $ M.lookup i $ itemNewness world) clientWants) ++ filter (\i -> not $ S.member i $ S.fromList $ map fst clientWants) (case mPlayer of
              Nothing -> S.toList $ rootsList world
              Just (PlayerState soul) -> filter (>=0) [soul]
            )
      liftIO $ forM_ outdated $ enqueue (itemUpdateQueue clientState)
      return clientState

moveItemServer :: MVar World -> Server MoveItemAPI
moveItemServer worldVar (target, destination) = updateWorld worldVar $ do
    hasDestination <- hasItem destination
    mTargetItem <- getItem target
    hasLoop <- checkLoop destination
    case (hasDestination, hasLoop, mTargetItem) of
      (True, False, Just _) -> do
        modifyItemAttribute @"container" (union [ItemRef target]) destination
        return NoContent
      _ -> return NoContent -- Fail silently because it might just be that the client hasn't received some relevant update yet.
  where checkLoop i | i == target = return True
        checkLoop i | otherwise   = do
          mUp <- getItemAttribute @".." i
          maybe (return False) checkLoop mUp

moveCommodityServer :: MVar World -> Server MoveCommodityAPI
moveCommodityServer worldVar (target, commodityType, quantity, attached, destination) = updateWorld worldVar $ do
    mTargetItem <- getItem target
    let contentList = (maybe [] id) $ (if attached then getAtt @"components" else getAtt @"container") <$> mTargetItem
    hasDestination <- hasItem destination
    case (takeQuantity 0 contentList, hasDestination) of
      ((0, _), _) -> return NoContent
      (_, False) -> return NoContent
      ((q,content'), True) -> do
        if attached then setItemAttribute @"components" content' target else setItemAttribute @"container" content' target
        modifyItemAttribute @"container" (giveQuantity q) destination
        return NoContent
  where -- In takeQuantity, q is the amount found so far.
        takeQuantity q [] = (q,[])
        takeQuantity q (ItemRef i:os) = (ItemRef i:) <$> takeQuantity q os
        takeQuantity q (Commodity t q':os) | t /= commodityType = (Commodity t q':) <$> takeQuantity q os
        takeQuantity q (Commodity t q':os) = case compare (q+q') quantity of
          GT -> (quantity,Commodity t (q+q'-quantity):os)
          EQ -> (quantity,os)
          LT -> takeQuantity (q+q') os
        -- In giveQuantity, q is the amount left to give.
        giveQuantity q [] = [Commodity commodityType q]
        giveQuantity q (ItemRef i:os) = ItemRef i : giveQuantity q os
        giveQuantity q (Commodity t q':os) | t /= commodityType = Commodity t q' : giveQuantity q os
        giveQuantity q (Commodity t q':os) = Commodity t (q'+q):os
