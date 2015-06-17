{-# LANGUAGE OverloadedStrings #-}
module Trigger.Anon (anonTrig) where

import Needles.Bot.Trigger
import Data.Text (Text)
import qualified Data.Text as T
import Data.Map (Map)
import qualified Data.Map as M
import Data.Time.Clock
import Control.Monad.IO.Class
import Control.Applicative
import Data.Maybe


anonTrig :: Trigger
anonTrig = mkTrigger "anon" (ProtoTrigger anonTest anonAct) M.empty
  
  
anonTest :: MessageInfo -> Bool
anonTest = startsWith "!mess " <&&> ((== MTPm) . mType)


negCooldown :: NominalDiffTime
negCooldown = -20

usageStr :: Text
usageStr = "Usage: !mess [#]destination, message\n" `T.append`
           "Use # to send to rooms"

coolDownStr :: Text
coolDownStr = "You must wait 20 seconds between each message."


anonAct :: MessageInfo -> TriggerAct (Map Text UTCTime) b ()
anonAct mi = do
  curTime <- liftIO getCurrentTime
  needCooldown <- maybe False (> addUTCTime negCooldown curTime) .  M.lookup normalName <$> getVar
  if needCooldown
    then sendCooldownMessage
    else do
    case (dest, mess) of
     ("", _) -> sendUsage
     (_, "") -> sendUsage
     _       -> responder >> updateTS
  where (dest, mess') = T.breakOn "," . T.strip . T.drop 6 . what $ mi
        mess = T.append "Anonymous Message: " . T.strip . T.drop 1 $ mess'
        sendUsage = respond mi usageStr
        sendCooldownMessage = respond mi coolDownStr
        normalName = normalizeName (who mi)
        updateTS = do
          oldMap <- getVar
          curTime <- liftIO getCurrentTime
          storeVar $ M.insert normalName curTime oldMap
        responder = if T.take 1 dest == "#"
                    then do
                      sendChat (T.drop 1 dest) mess
                      respond mi $ "Sending chat to " `T.append` dest
                    else do
                      sendPm dest mess
                      respond mi $ "Sending pm to " `T.append` dest
