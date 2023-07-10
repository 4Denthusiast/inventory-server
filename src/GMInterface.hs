{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}
module GMInterface (
    GMAPI,
    gmServer,
    authCheckContext
) where

import Data

import Control.Concurrent.MVar
import Control.Monad
import Control.Monad.IO.Class
import qualified Data.ByteString.UTF8 as UTF8
import qualified Data.Map.Lazy as M
import qualified Data.Set as S
import Network.HTTP.Types.Header
import Network.HTTP.Types.Status
import Network.Wai
import Servant
import WaiAppStatic.Storage.Filesystem

type GMAPI = GMPageAPI :<|> CreateLocationAPI
type GMPageAPI = "gm.html" :> BasicAuth "gm" String :> QueryParam "name" String :> Raw
type CreateLocationAPI = "create-location" :> ReqBody '[PlainText] String :> PostNoContent

gmServer :: MVar World -> Server GMAPI
gmServer worldVar = gmPageServer worldVar :<|> createLocationServer worldVar

gmPageServer :: MVar World -> Server GMPageAPI
gmPageServer _ name Nothing = Tagged $ \_ c -> c $ responseLBS seeOther303 [(hLocation, UTF8.fromString $ "/gm.html?name=gm%20"++name)] "" --Make the name available in JS
gmPageServer worldVar _ (Just name) = Tagged $ \_ c -> do
    liftIO $ modifyMVar_ worldVar $ addGM
    c $ responseFile status200 [(hContentType, "text/html")] "./pages/gm.html" Nothing
  where addGM world@World{clientStates = css} = case M.lookup name css of
          Just _ -> return world
          Nothing -> do
            q <- newEmptyQueue
            return world{clientStates = M.insert name (ClientState q S.empty 0 Nothing) css}

authCheckContext :: Context '[BasicAuthCheck String]
authCheckContext = check :. EmptyContext
  where check = BasicAuthCheck $ \(BasicAuthData name password) -> case password of
          "15293" -> return $ Authorized (UTF8.toString name) -- I'm aware hardcoding in passwords is a bad idea, but this use-case really doesn't require much actual security.
          _ -> return BadPassword

createLocationServer :: MVar World -> Server CreateLocationAPI
createLocationServer worldVar name = liftIO $ do
  i <- modifyMVar worldVar $ \world -> return $ let
      item = setAtt' @"container" $ setAtt' @"location" $ setAtt @"name" name []
    in addItem item world
  publishLocation worldVar i
  return NoContent

publishLocation :: MVar World -> ID -> IO ()
publishLocation worldVar i = modifyMVar_ worldVar $ \world -> do
  forM_ (M.toList $ clientStates world) $ \(name, state) -> if take 3 name == "gm " then enqueue (itemUpdateQueue state) i else return ()
  return world{locationList = i:locationList world}
