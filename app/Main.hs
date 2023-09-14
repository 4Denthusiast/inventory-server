module Main (main) where

import Lib

import System.Environment
import System.Posix.Directory

main :: IO ()
main = do
  args <- getArgs
  case args of
    [] ->  startAppNet
    [addr] -> startAppSocket addr
    [addr,dir] -> changeWorkingDirectory dir >> startAppSocket addr
    _ -> error "Too many arguments."
