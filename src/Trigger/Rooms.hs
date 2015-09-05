{-# LANGUAGE OverloadedStrings #-}
module Trigger.Rooms (joinTrig, leaveTrig) where

import Needles.Bot.Trigger
import Data.Text (Text)
import qualified Data.Text as T


accessList :: [Text]
accessList = map normalizeName $
             [ "Reimu Raymoo"
             , "Yuzuko"
             ] 


joinPrefix :: Text
joinPrefix = "/join "

leavePrefix :: Text
leavePrefix = "/leave "


joinTest :: MessageInfo -> Bool
joinTest mi = startsWith ".join" mi && normalizeName (who mi) `elem` accessList


joinAct :: MessageInfo -> TriggerAct a b ()
joinAct mi
  | targetRoom == "" = respond mi usage
  | otherwise = respond mi "Ok." >> command "" (joinPrefix `T.append` targetRoom)
  where targetRoom = T.drop 6 (what mi)
        usage = "Usage: .join room"


joinTrig :: Trigger
joinTrig = mkTrigger "join" (ProtoTrigger joinTest joinAct) ()


leaveTest :: MessageInfo -> Bool
leaveTest mi = startsWith ".leave" mi && normalizeName (who mi) `elem` accessList


leaveAct :: MessageInfo -> TriggerAct a b ()
leaveAct mi
  | targetRoom == "" = respond mi usage
  | otherwise = respond mi "Ok." >> command "" (leavePrefix `T.append` targetRoom)
  where targetRoom = T.drop 7 (what mi)
        usage = "Usage: .leave room"


leaveTrig :: Trigger
leaveTrig = mkTrigger "leave" (ProtoTrigger leaveTest leaveAct) ()
