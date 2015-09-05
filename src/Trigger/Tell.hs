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
data TellMess = TellMess { tmSender :: Sender, tmMessage :: Text }


type TellMap = Map Recipient [TellMess]

type Recipient = Text
type Sender = Text


addTell :: Recipient -> Sender -> Text -> TellMap -> TellMap
addTell recip sender mess =
  -- | If there are no messages for the recipient already, create a new list.
  -- otherwise just append it.
  M.alter (maybe (Just [newRec]) (Just . (newRec:))) recip'
  where newRec = TellMess sender' mess

        -- | Canonical names
        recip' = normalizeName recip
        sender' = normalizeName sender


-- | 'TriggerAct' version of 'addTell'
addTellAct :: Recipient -> Sender -> Text -> TriggerAct TellMap b ()
addTellAct recip sender mess = do
  oldMap <- getVar
  let newMap = addTell recip sender mess oldMap
  storeVar newMap


-- | Get the current tells for a recipient, if there are any
takeTells :: Recipient -> TellMap -> Maybe ([TellMess], TellMap)
takeTells recip oldMap =
  let recip' = normalizeName recip
      (mRes, newMap) = M.updateLookupWithKey (const (const Nothing)) recip' oldMap
  in case mRes of
      Just res -> Just (res, newMap)
      Nothing  -> Nothing


-- | 'TriggerAct' version of 'takeTells'
takeTellsAct :: Recipient -> TriggerAct TellMap b (Maybe [TellMess])
takeTellsAct recip  = do
  oldMap <- getVar
  case takeTells recip oldMap of
   Nothing -> return Nothing
   Just (res, newMap) -> storeVar newMap >> return (Just res)


-- | User must have voice or above, and must use ".tell" in the chat or pm.
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
        spaceds = T.words . T.drop 6 $ what mi -- ^ Stuff after ".tell " separated into words
        recip = T.concat $ take 1 spaceds -- ^ The recipient
        mess = T.intercalate " " $ drop 1 spaceds -- ^ The message to send


-- | A user can get their tells read to them if they chat.
tellGetTest :: MessageInfo -> Bool
tellGetTest mi = (mType mi `elem` [MTChat, MTPm]) &&
                 not (".tell" `T.isPrefixOf` what mi)


tellGetAct :: MessageInfo -> TriggerAct TellMap b ()
tellGetAct mi = do
  mRes <- takeTellsAct (who mi) -- ^ Get any current tells for the user
  case mRes of
   Nothing -> return () -- ^ No tells, don't do anything
   Just ress -> mapM_ replyTell . reverse $ ress -- ^ Send a response for each tell
   where replyString sender mess = (who mi) `T.append` 
                                   ": [" `T.append` sender `T.append` "] " `T.append`
                                   mess
         -- ^ Make the formatted inbox message

         -- ^ Send a formatted inbox message of a 'TellMess'
         replyTell (TellMess sender mess) = respond mi (replyString sender mess)
  

tellTrig :: Trigger
tellTrig = clusterTrigger "tell" [ ProtoTrigger tellGetTest tellGetAct
                                 , ProtoTrigger tellSendTest tellSendAct
                                 ] M.empty
