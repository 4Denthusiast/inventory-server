module Main (main) where

import Lib

import System.Environment

main :: IO ()
main = do
  args <- getArgs
  case args of
    [] ->  startAppNet
    (addr:_) -> startAppSocket addr
