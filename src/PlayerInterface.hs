{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeOperators #-}
module PlayerInterface (
    PlayerAPI,
    playerServer
) where

import Data

import Control.Concurrent.MVar
import Control.Monad.IO.Class
import Data.Char
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
  csEmpathy :: Double
} deriving (Generic, Show)

instance FromForm CharacterSubmission

type PlayerAPI = "add-character" :> ReqBody '[FormUrlEncoded] CharacterSubmission :>Verb 'POST 303 '[PlainText] (Headers '[Header "Location" String] String)

playerServer :: Server PlayerAPI
playerServer char = liftIO (putStrLn (show char)) >> return (addHeader url ("Please proceed to "++url))
  where simpleName = map toLower $ takeWhile (/=' ') $ csName char --TODO: sanitise this more, and make it explicitly avoid overlaps.
        url = "/player.html?name="++simpleName
