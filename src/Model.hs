{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
module Model (
  updateWorld,
  getItem,
  hasItem,
  getItemAttribute,
  setItemAttribute,
  modifyItemAttribute,
  modifyItemAttribute',
  setItem,
  addItem
) where

import Data

import Control.Concurrent
import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Trans.State
import Data.Aeson hiding (Object)
import qualified Data.Map.Lazy as M
import Data.Map.Lazy (Map)
import Data.Maybe
import qualified Data.Set as S
import Data.Set (Set)

type WorldState a = StateT (World,Set ID) IO a

updateWorld :: (MonadIO m) => MVar World -> WorldState a -> m a
updateWorld worldVar s = liftIO $ modifyMVar worldVar $ \world -> do
  (x, (world', updates)) <- runStateT s (world, S.empty)
  forM_ updates $ \i -> forM (clientStates world') $ \ClientState{itemUpdateQueue = q, itemsListened = list, clientIsGM = isGM} -> if (S.member i list || (isGM && S.member i (rootsList world'))) then enqueue q i else return ()
  let world'' = world'{itemNewness = foldr (M.adjust (+1)) (itemNewness world') updates}
  return (world'', x)

getWorld :: WorldState World
getWorld = fst <$> get

getItem :: ID -> WorldState (Maybe Item)
getItem i = M.lookup i <$> itemStore <$> getWorld

hasItem :: ID -> WorldState Bool
hasItem i = isJust <$> getItem i

getItemAttribute :: forall s. Attribute s => ID -> WorldState (Maybe (AData s))
getItemAttribute i = (getAtt' @s =<<) <$> getItem i

setItemAttribute :: forall s. (Attribute s, ToJSON (AData s), Eq (AData s)) => AData s -> ID -> WorldState ()
setItemAttribute x i = do
  mItem <- getItem i
  case mItem of
    Nothing -> return ()
    Just item -> setItem (setAtt @s x item) i

modifyItemAttribute :: forall s. (Attribute s, ToJSON (AData s), Eq (AData s)) => (AData s -> AData s) -> ID -> WorldState ()
modifyItemAttribute f i = do
  mItem <- getItem i
  case mItem of
    Nothing -> return ()
    Just item -> setItem (setAtt @s (f $ getAtt @s item) item) i

modifyItemAttribute' :: forall s. (Attribute s, ToJSON (AData s), Eq (AData s)) => (Maybe (AData s) -> Maybe (AData s)) -> ID -> WorldState ()
modifyItemAttribute' f i = do
  mItem <- getItem i
  case (mItem, f $ getAtt' @s =<< mItem) of
    (Nothing, _) -> return ()
    (Just item, Just att) -> setItem (setAtt @s att item) i
    (Just item, Nothing) -> setItem (removeAtt @s item) i

setItem :: Item -> ID -> WorldState ()
setItem item i = do
  prev <- getItem i
  if prev == Just item then return () else do
    item' <- consistencify item
    setItemRaw item' i
    mapM_ triggerUpdate $ getAtt' @".." item'

triggerUpdate :: ID -> WorldState ()
triggerUpdate i = do
  mItem <- getItem i
  mItem' <- mapM consistencify mItem
  if mItem == mItem' then return () else do
    setItemRaw (fromJust mItem') i
    mapM_ triggerUpdate $ getAtt' @".." (fromJust mItem')

setItemRaw :: Item -> ID -> WorldState ()
setItemRaw item i = modify $ \(world,updates) -> (world{itemStore = M.insert i item (itemStore world), rootsList = (if hasAtt @"root" item then S.insert else S.delete) i (rootsList world)}, S.insert i updates)

addItem :: Item -> WorldState ID
addItem item = do
  item' <- consistencify item
  (w@World{itemStore = is, nextID = n, itemNewness = times, rootsList = rs}, updates) <- get
  put (w{
      itemStore = M.insert n item' is,
      nextID = n + 1,
      itemNewness = M.insert n 0 times,
      rootsList = if hasAtt @"root" item' then S.insert n rs else rs
    }, S.insert n updates)
  return n

-- This should be idempotent unless the world changes in between.
consistencify :: Item -> WorldState Item
consistencify item = checkRoot item
  where checkRoot it = do
          isRoot <- if hasAtt @"location" it then return True else
            case getAtt' @"soul" it of 
              Nothing -> return False
              Just name -> M.member name <$> playerStates <$> getWorld
          return $ if isRoot then setAtt' @"root" it else removeAtt @"root" it
