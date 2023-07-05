{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeOperators #-}
module Lib (
  startAppNet,
  startAppSocket,
  app
) where

import Data
import PlayerInterface
import GMInterface

import Control.Concurrent.MVar
import Control.Exception
import Control.Monad.IO.Class
import Data.Maybe
import Network.HTTP.Types.Header
import Network.HTTP.Types.Status
import Network.Socket
import Network.Wai
import Network.Wai.Handler.Warp
import Servant
import System.Directory
import System.Posix.Files
import WaiAppStatic.Storage.Filesystem
import WaiAppStatic.Types

type API = PlayerAPI :<|> GMAPI :<|> Raw

internalSocket :: String -> IO Socket
internalSocket addr = do
  sock <- socket AF_UNIX Stream defaultProtocol
  bind sock (SockAddrUnix addr)
  setFileMode addr stdFileMode
  return sock

startAppNet :: IO ()
startAppNet = do
  worldVar <- loadWorld
  run 8080 (app worldVar)

startAppSocket :: String -> IO ()
startAppSocket addr = bracket
  (internalSocket addr)
  (\sock -> do
    close sock
    status <- getFileStatus addr
    if isSocket status --Ideally I'd only remove the file if this program created it. I certainly don't want to accidentally delete files if something goes wrong. The user accidentally specifying a different socket seems unlikely though.
      then removeFile addr
      else return ()
  )
  (\sock -> do
    listen sock 5
    worldVar <- loadWorld
    runSettingsSocket defaultSettings sock (app worldVar)
  )

loadWorld :: IO (MVar World)
loadWorld = newMVar $ emptyWorld

app :: MVar World -> Application
app = serveWithContext api authCheckContext . server

api :: Proxy API
api = Proxy

server :: MVar World -> Server API
server worldVar = playerServer worldVar :<|> gmServer worldVar :<|> staticServer

staticServer :: Server Raw
staticServer = serveDirectoryWith (defaultWebAppSettings "./pages"){
    ssIndices = [fromJust $ toPiece "index.html"],
    ss404Handler = Just notFoundApp
  }
-- TODO: Set all the required headers in the 404 handler (it's the empty list currently).

notFoundApp :: Application
notFoundApp _ c = do
  let path = "./pages/404.html"
  file <- getFileStatus path
  let size = toInteger $ fileSize file --I have to provide the file size explicitly otherwise Warp ignores that it's meant to be using a 404 status for some reason.
  c $ responseFile status404 [(hContentType, "text/html")] path (Just $ FilePart 0 size size)
