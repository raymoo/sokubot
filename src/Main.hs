{-# LANGUAGE OverloadedStrings #-}
module Main (main) where

import Needles.Bot.Configuration
import Needles.Bot
import Needles.Bot.Trigger
import Trigger.Tell
import Trigger.Anon
import Trigger.Jibun
import Trigger.Rooms
import qualified Data.Text.IO as T
import System.Environment

config :: String -> String -> Configuration
config user pass = Configuration { cUsername = user
                                 , cPassword = pass
                                 , cServer = mainServer
                                 , cPort = mainPort
                                 , cPath = mainPath
                                 , cTriggers = [ tellTrig
                                               , anonTrig
                                               , jibunTrig
                                               , joinTrig
                                               , leaveTrig
                                               ]
                                 , cRooms = ["animeandmanga"]
                                 , cLogger = T.putStrLn
                                 }

main :: IO ()
main = do
  args <- getArgs
  runBot $ config (args !! 0) (args !! 1)
