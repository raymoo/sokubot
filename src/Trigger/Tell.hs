{-# LANGUAGE OverloadedStrings #-}
module Trigger.Tell (tellTrig) where

import Control.Monad
import Needles.Bot.Trigger
import qualified Data.Map as M
import Data.Char
import Data.Map (Map)
import Data.Text (Text)
import qualified Data.Text as T

-- | Utils


-- | Tell message
data TellMess = TellMess { tmSender :: Text, tmMessage :: Text }

type TellMap = Map Text [TellMess]

type Recipient = Text
type Sender = Text


addTell :: Recipient -> Sender -> Text -> TellMap -> TellMap
addTell recip sender mess =
  M.alter (maybe (Just [newRec]) (Just . (newRec:))) recip'
  where newRec = TellMess sender' mess
        recip' = normalizeName recip
        sender' = normalizeName sender


addTellAct :: Recipient -> Sender -> Text -> TriggerAct TellMap b ()
addTellAct recip sender mess = do
  oldMap <- getVar
  let newMap = addTell recip sender mess oldMap
  storeVar newMap


takeTells :: Recipient -> TellMap -> Maybe ([TellMess], TellMap)
takeTells recip oldMap =
  let recip' = normalizeName recip
      (mRes, newMap) = M.updateLookupWithKey (const (const Nothing)) recip' oldMap
  in case mRes of
      Just res -> Just (res, newMap)
      Nothing  -> Nothing


takeTellsAct :: Recipient -> TriggerAct TellMap b (Maybe [TellMess])
takeTellsAct recip  = do
  oldMap <- getVar
  case takeTells recip oldMap of
   Nothing -> return Nothing
   Just (res, newMap) -> storeVar newMap >> return (Just res)


-- | The parts for sending
tellSendTest :: MessageInfo -> Bool
tellSendTest mi = rank mi /= ' ' &&
                  (mType mi `elem` [MTChat, MTPm]) &&
                  (".tell " `T.isPrefixOf` what mi)

tellSendAct :: MessageInfo -> TriggerAct TellMap b ()
tellSendAct mi = do
  addTellAct recip (who mi) mess
  respond mi infoMessage
  where infoMessage = "I will tell " `T.append`
                      recip `T.append`
                      " next time I see that user."
        spaceds = T.words . T.drop 6 $ what mi
        recip = T.concat $ take 1 spaceds
        mess = T.intercalate " " $ drop 1 spaceds


-- | The parts for checking
tellGetTest :: MessageInfo -> Bool
tellGetTest mi = (mType mi `elem` [MTChat, MTPm]) &&
                 not (".tell" `T.isPrefixOf` what mi)

tellGetAct :: MessageInfo -> TriggerAct TellMap b ()
tellGetAct mi = do
  mRes <- takeTellsAct (who mi)
  case mRes of
   Nothing -> return ()
   Just ress -> mapM_ replyTell ress
   where replyString sender mess = (who mi) `T.append`
                                   ": [" `T.append` sender `T.append` "] " `T.append`
                                   mess
         replyTell (TellMess sender mess) = respond mi (replyString sender mess)
  

tellTrig :: Trigger
tellTrig = clusterTrigger "tell" [ ProtoTrigger tellGetTest tellGetAct
                                 , ProtoTrigger tellSendTest tellSendAct
                                 ] M.empty
