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


accessList :: [Text]
accessList = map normalizeName $ 
  [ "Reimu Raymoo"
  , "Yuzuko"
  , "NixOS"
  , "Magichatman2"
  , "prem"
  , "yuihirasawa"
  , "waddict"
  , "runningfatkids"
  , "heterosexualzekrom"
  , "vexeniv"
  , "morfent"
  , "hippopotas"
  , "ascriptmaster"
  ]


anonTrig :: Trigger
anonTrig = mkTrigger "anon" (ProtoTrigger anonTest anonAct) M.empty
  

-- | Message should start with ".mess", and should be in a PM (otherwise it's not
-- really private)
anonTest :: MessageInfo -> Bool
anonTest mi = (startsWith ".mess " <&&> ((== MTPm) . mType)) mi &&
              normalizeName (who mi) `elem` accessList


-- | Negative of the anonymous message cooldown
negCooldown :: NominalDiffTime
negCooldown = -20


-- | Help message for malformed messages
usageStr :: Text
usageStr = "Usage: .mess [#]destination, message\n" `T.append`
           "Use # to send to rooms"


coolDownStr :: Text
coolDownStr = "You must wait 20 seconds between each message."


logMessage :: Text -> Text -> TriggerAct a b ()
logMessage user mess = writeLog $ user `T.append` " sent anon message: " `T.append` mess


-- | The state is just a map of the last time each user sent a message
anonAct :: MessageInfo -> TriggerAct (Map Text UTCTime) b ()
anonAct mi = do

  -- | Check if the user is in cooldown
  curTime <- liftIO getCurrentTime
  needCooldown <- maybe False (> addUTCTime negCooldown curTime) .  M.lookup normalName <$> getVar
  if needCooldown
    then sendCooldownMessage
    else do
    case (dest, mess') of
     
     -- | Missing destination or message
     ("", _) -> sendUsage
     (_, "") -> sendUsage

     -- | Send the message
     _       -> responder >> updateTS >> logMessage (who mi) mess'
  where (dest, mess') = T.breakOn "," . T.strip . T.drop 6 . what $ mi
        -- ^ Separate the destination from the message to send

        -- | Append the anonymous message thing to the beginning of the message
        mess = T.append "Anonymous Message: " . T.strip . T.drop 1 $ mess'

        -- | Action that sends help info to the user
        sendUsage = respond mi usageStr

        -- | Action for sending cooldown notice
        sendCooldownMessage = respond mi coolDownStr

        -- | Canonical name
        normalName = normalizeName (who mi)

        -- | Action for updating the timestamp info for the user
        updateTS = do
          oldMap <- getVar
          curTime <- liftIO getCurrentTime
          storeVar $ M.insert normalName curTime oldMap
          
        -- | Action that actually sends the message
        responder = if T.take 1 dest == "#" -- ^ Sending to a room
                    then do
                      sendChat (T.drop 1 dest) mess
                      respond mi $ "Sending chat to " `T.append` dest
                    else do -- ^ Sending to a user
                      sendPm dest mess
                      respond mi $ "Sending pm to " `T.append` dest
