{-# LANGUAGE DataKinds       #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeOperators   #-}
module Lib (
  startAppNet,
  startAppSocket,
  app
) where

import Control.Exception
import Data.Aeson
import Data.Aeson.TH
import Network.Socket
import Network.Wai
import Network.Wai.Handler.Warp
import Servant
import System.Directory
import System.Posix.Files

data User = User
  { userId        :: Int
  , userFirstName :: String
  , userLastName  :: String
  } deriving (Eq, Show)

$(deriveJSON defaultOptions ''User)

type API = "users" :> Get '[JSON] [User]

internalSocket :: String -> IO Socket
internalSocket addr = do
  sock <- socket AF_UNIX Stream defaultProtocol
  bind sock (SockAddrUnix addr)
  setFileMode addr stdFileMode
  return sock

startAppNet :: IO ()
startAppNet = run 8080 app

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
    runSettingsSocket defaultSettings sock app
  )

app :: Application
app = serve api server

api :: Proxy API
api = Proxy

server :: Server API
server = return users

users :: [User]
users = [ User 1 "Isaac" "Newton"
        , User 2 "Albert" "Einstein"
        ]
