{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}
module GMInterface (
    GMAPI,
    gmServer,
    authCheckContext
) where

import Data
import Model

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

type GMAPI = GMPageAPI :<|> CreateLocationAPI :<|> AddAttributeAPI :<|> CreateItemAPI :<|> CreateCommodityAPI
type GMPageAPI = "gm.html" :> BasicAuth "gm" String :> QueryParam "name" String :> Raw
type CreateLocationAPI = "create-location" :> ReqBody '[PlainText] String :> PostNoContent
type AddAttributeAPI = "add-attribute" :> ReqBody '[JSON] (ID,Item) :> PostNoContent
type CreateItemAPI = "create-item" :> ReqBody '[JSON] (ID,Bool) :> PostNoContent
type CreateCommodityAPI = "create-commodity" :> ReqBody '[JSON] (ID,Bool,String,Double) :> PostNoContent

gmServer :: MVar World -> Server GMAPI
gmServer worldVar = gmPageServer worldVar :<|> createLocationServer worldVar :<|> addAttributeServer worldVar :<|> createItemServer worldVar :<|> createCommodityServer worldVar

gmPageServer :: MVar World -> Server GMPageAPI
gmPageServer _ name Nothing = Tagged $ \_ c -> c $ responseLBS seeOther303 [(hLocation, UTF8.fromString $ "/gm.html?name=gm%20"++name)] "" --Make the name available in JS
gmPageServer worldVar _ (Just name) = Tagged $ \_ c -> do
    addClient worldVar name True
    c $ responseFile status200 [(hContentType, "text/html")] "./pages/gm.html" Nothing

authCheckContext :: Context '[BasicAuthCheck String]
authCheckContext = check :. EmptyContext
  where check = BasicAuthCheck $ \(BasicAuthData name password) -> case password of
          "15293" -> return $ Authorized (UTF8.toString name) -- I'm aware hardcoding in passwords is a bad idea, but this use-case really doesn't require much actual security.
          _ -> return BadPassword

createLocationServer :: MVar World -> Server CreateLocationAPI
createLocationServer worldVar name = do
  updateWorld worldVar $ addItem (setAtt' @"container" $ setAtt' @"location" $ setAtt @"name" name [])
  return NoContent

addAttributeServer :: MVar World -> Server AddAttributeAPI
addAttributeServer worldVar (i,[AnAtt @s _ att]) = updateWorld worldVar (setItemAttribute @s att i) >> return NoContent
addAttributeServer _ _ = error "wrong number of attributes received in add-attribute."

createItemServer :: MVar World -> Server CreateItemAPI
createItemServer worldVar (i,attached) = updateWorld worldVar $ do
  newId <- addItem $ setAtt @".." i []
  (if attached then modifyItemAttribute @"components" else modifyItemAttribute @"container") (++[ItemRef newId]) i
  return NoContent

createCommodityServer :: MVar World -> Server CreateCommodityAPI
createCommodityServer worldVar (i,attached,commodityType,quantity) = do
  updateWorld worldVar $ (if attached then modifyItemAttribute @"components" else modifyItemAttribute @"container") (++[Commodity commodityType quantity]) i
  return NoContent
