module Tokens where

import Control.Monad
import System.Directory

getTokens :: String -> IO [String]
getTokens tokenFilename =
  do
    c <- doesFileExist tokenFilename
    case c of
      True ->
        do
          let f = readFile tokenFilename
          liftM lines f
      False -> return ([])
