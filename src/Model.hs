{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
module Model (
  addClient,
  updateWorld,
  getWorld,
  getItem,
  hasItem,
  getItemAttribute,
  setItemAttribute,
  modifyItemAttribute,
  modifyItemAttribute',
  addItem
) where

import Data

import Control.Concurrent
import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Trans.State
import Data.Aeson hiding (Object)
import Data.List (delete)
import qualified Data.Map.Lazy as M
import Data.Map.Lazy (Map)
import Data.Maybe
import qualified Data.Set as S
import Data.Set (Set)
import Debug.Trace
import GHC.TypeLits
import System.Timeout

addClient :: (MonadIO m) => MVar World -> String -> Bool -> m Bool
addClient worldVar name isGM = liftIO $ modifyMVar worldVar $ \world@World{clientStates = css} -> case M.lookup name css of
  Just _ -> return (world,False)
  Nothing -> do
    q <- newEmptyQueue
    return (world{clientStates = M.insert name (ClientState q S.empty 0 Nothing isGM) css},True)

type WorldState a = StateT (World,Set ID) IO a

updateWorld :: (MonadIO m) => MVar World -> WorldState a -> m a
updateWorld worldVar s = liftIO $ fmap (fromMaybe (trace "Update timed out." $ error "Update timed out.")) $ timeout 1000000 $ modifyMVar worldVar $ \world -> do
  (x, (world', updates)) <- runStateT s (world, S.empty)
  forM_ updates $ \i -> forM (clientStates world') $ \ClientState{itemUpdateQueue = q, itemsListened = list, clientIsGM = isGM} -> if (S.member i list || (isGM && S.member i (rootsList world'))) then enqueue q i else return ()
  let world'' = world'{itemNewness = foldr (M.adjust (+1)) (itemNewness world') updates}
  return (world'', x)

getWorld :: WorldState World
getWorld = fst <$> get

modifyWorld :: (World -> World) -> WorldState ()
modifyWorld f = modify (\(w,u) -> (f w, u))

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
    Just item -> setItem (SomeAttributes [symbolVal @s undefined]) (setAtt @s x item) i

modifyItemAttribute :: forall s. (Attribute s, ToJSON (AData s), Eq (AData s)) => (AData s -> AData s) -> ID -> WorldState ()
modifyItemAttribute f i = do
  mItem <- getItem i
  case mItem of
    Nothing -> return ()
    Just item -> setItem (SomeAttributes [symbolVal @s undefined]) (setAtt @s (f $ getAtt @s item) item) i

modifyItemAttribute' :: forall s. (Attribute s, ToJSON (AData s), Eq (AData s)) => (Maybe (AData s) -> Maybe (AData s)) -> ID -> WorldState ()
modifyItemAttribute' f i = do
  mItem <- getItem i
  let mAtt0 = getAtt' @s =<< mItem
  case (mItem, f mAtt0) of
    (Just item, Just att) -> setItem (SomeAttributes [symbolVal @s undefined]) (setAtt @s att item) i
    (Just item, Nothing) -> setItem (SomeAttributes [symbolVal @s undefined]) (removeAtt @s item) i
    _ -> return ()

setItem :: AttChangeList -> Item -> ID -> WorldState ()
setItem changes item i = do
  prev <- getItem i
  if prev == Just item then return () else do
    item' <- consistencify changes i item
    setItemRaw item' i
    mapM_ (triggerUpdate (SomeAttributes ["components", "contents"])) $ getAtt' @".." item'

triggerUpdate :: AttChangeList -> ID -> WorldState ()
triggerUpdate changes i = do
  mItem <- getItem i
  mItem' <- mapM (consistencify changes i) mItem
  if mItem == mItem' then return () else do
    setItemRaw (fromJust mItem') i
    mapM_ (triggerUpdate (SomeAttributes ["components", "contents"])) $ getAtt' @".." (fromJust mItem')

setItemRaw :: Item -> ID -> WorldState ()
setItemRaw item i = modify $ \(world,updates) -> (world{itemStore = M.insert i item (itemStore world)}, S.insert i updates)

addItem :: Item -> WorldState ID
addItem item = do
  (w@World{itemStore = is, nextID = n, itemNewness = times}, updates) <- get
  put (w{
      itemStore = M.insert n item is,
      nextID = n + 1,
      itemNewness = M.insert n 0 times
    }, S.insert n updates)
  triggerUpdate AllAttributes n
  return n

data AttChangeList = AllAttributes | SomeAttributes [String] deriving (Show)

areAttsChanged :: AttChangeList -> [String] -> Bool
areAttsChanged AllAttributes _ = True
areAttsChanged (SomeAttributes atts0) atts1 = any (flip elem atts0) atts1

-- This should be idempotent unless the world changes in between. It mustn't depend on the value of ..
consistencify :: AttChangeList -> ID -> Item -> WorldState Item
consistencify changes i item = foldr runCheck (return item) [
    (checkComponents,["components"]),
    (checkContents,["container"]),
    (checkSpawnPoint,["spawnPoint"]),
    (checkPlayerSoul,["soul"]),
    (checkRoot,["location","soul","root"])
  ]
  where runCheck (f,atts) it | areAttsChanged changes atts = f =<< it
        runCheck _ it        | otherwise = it
        checkRoot it = do
          isRoot <- if hasAtt @"location" it then return True else
            case getAtt' @"soul" it of 
              Nothing -> return False
              Just name -> M.member name <$> playerStates <$> getWorld
          modifyWorld $ \w -> w{rootsList = (if isRoot then S.insert else S.delete) i (rootsList w)}
          return $ if isRoot then setAtt' @"root" it else removeAtt @"root" it
        checkPlayerSoul it = case getAtt' @"soul" it of
          Nothing -> return it
          Just name -> modifyWorld (\w -> w{playerStates = M.adjust (\_ -> PlayerState i) name (playerStates w)}) >> return it
        checkSpawnPoint it = if hasAtt @"spawnPoint" it
          then do
            prevPoint <- spawnPoint <$> getWorld
            modifyWorld (\w -> w{spawnPoint = Just i})
            case prevPoint of
              Nothing -> return ()
              Just i' | i' == i   -> return ()
              Just i' | otherwise -> modifyItemAttribute' @"spawnPoint" (\_ -> Nothing) i'
            return it
          else modifyWorld (\w -> w{spawnPoint = if spawnPoint w == Just i then Nothing else spawnPoint w}) >> return it
        checkComponents it = checkContainedHere (getAtt @"components" it) >> return it
        checkContents it = checkContainedHere (getAtt @"container" it) >> return it
        checkContainedHere = mapM (\obj -> case obj of
            Commodity _ _ -> return ()
            ItemRef cid -> do
              mc <- getItem cid
              case mc of
                Nothing -> return ()
                Just c  -> let cUp = getAtt' @".." c in if cUp /= Just i
                  then do
                    setItemRaw (setAtt @".." i c) cid -- Don't trigger an update otherwise it would cause a loop. This should be safe so long as .. doesn't matter to consistencify.
                    case cUp of
                      Nothing -> return ()
                      Just i' -> modifyItemAttribute' @"container" (fmap (delete (ItemRef cid))) i' >> modifyItemAttribute' @"components" (fmap (delete (ItemRef cid))) i'
                  else return ()
          )
