{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeOperators #-}
module GMInterface (
    GMAPI,
    gmServer,
    authCheckContext
) where

import Data

import Control.Concurrent.MVar
import Network.HTTP.Types.Header
import Network.HTTP.Types.Status
import Network.Wai
import Servant
import WaiAppStatic.Storage.Filesystem

type GMAPI = "gm.html" :> BasicAuth "gm" () :> Raw

gmServer :: Server GMAPI
gmServer () = Tagged $ \_ c -> c $ responseFile status200 [(hContentType, "text/html")] "./pages/gm.html" Nothing

authCheckContext :: Context '[BasicAuthCheck ()]
authCheckContext = check :. EmptyContext
  where check = BasicAuthCheck $ \(BasicAuthData _ password) -> case password of
          "15293" -> return $ Authorized () -- I'm aware hardcoding in passwords is a bad idea, but this use-case really doesn't require much actual security.
          _ -> return BadPassword
