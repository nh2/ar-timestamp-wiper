module Main where

import           System.Environment (getArgs)

import           ArTimestampWiper

main :: IO ()
main = do
  args <- getArgs
  case args of
    [file] -> arFileWipeTimeStamps file
    _      -> error "Usage: ar-timestamp-wiper [file.a]"

