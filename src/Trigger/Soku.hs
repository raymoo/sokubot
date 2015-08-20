{-# LANGUAGE OverloadedStrings #-}
module Trigger.Soku (sokuTrig) where

import Control.Applicative
import Control.Monad
import Needles.Bot.Trigger
import Data.Text (Text)
import qualified Data.Text as T
import Data.Map (Map)
import qualified Data.Map.Strict as M
import Data.Time.Clock
import Control.Monad.IO.Class

import Data.Attoparsec.Text (Parser)
import qualified Data.Attoparsec.Text as AP


parseIP :: Text -> Maybe [Text]
parseIP = either (const Nothing) Just . AP.parseOnly ipFromRaw


ipFromRaw :: Parser [Text]
ipFromRaw = prelude *> (ipv4T `AP.sepBy1` (AP.string ", "))
  where prelude =
          AP.takeWhile1 (/= 'I') *> ((AP.string "IP: " <|> AP.string "IPs: ") <|> (AP.letter *> prelude))
        ipv4T = fmap (T.intercalate "." . map (T.pack . show)) ipv4


ipv4 :: Parser [Integer]
ipv4 = makeList <$> AP.decimal <*> oneAfter <*> oneAfter <*> oneAfter
  where makeList x y z a = [x,y,z,a]
        oneAfter = AP.char '.' *> AP.decimal


data HostRec = HostRec { hrName :: Text, hrAddress :: Text, hrTime :: UTCTime }

data HostReq = HostReq { hrqName :: Text, hrqPort :: Text, hrqRoom :: Text }

type HostDB = (Map Text HostRec, HostReq)


voicePlus :: MessageInfo -> Bool
voicePlus mi = rank mi `elem` "+%@#&~"

emptyDB :: HostDB
emptyDB = (M.empty, HostReq "" "" "")


addRequest :: HostReq -> TriggerAct HostDB b ()
addRequest hr = do
  (recMap, reqMap) <- getVar
  storeVar $ (recMap, hr)


addRecord :: HostRec -> TriggerAct HostDB b ()
addRecord hr = do
  (recMap, reqMap) <- getVar
  storeVar $ (M.insert (normalizeName (hrName hr)) hr recMap, reqMap)


getRequest :: TriggerAct HostDB b HostReq
getRequest = snd <$> getVar

getRecord :: Text -> TriggerAct HostDB b (Maybe HostRec)
getRecord name = M.lookup (normalizeName name) . fst <$> getVar


deleteRecord :: Text -> TriggerAct HostDB b Bool
deleteRecord name = do
  (recMap, req) <- getVar
  storeVar $ (M.delete (normalizeName name) recMap, req)
  return $ M.member (normalizeName name) recMap


sokuHostAct :: MessageInfo -> TriggerAct HostDB b ()
sokuHostAct mi = do
  addRequest $ HostReq (who mi) port (mRoom mi)
  command (mRoom mi) ("/ip " `T.append` who mi)
  where port = case T.drop 6 . what $ mi of
                "" -> "10800"
                x  -> x


sokuHostTest :: MessageInfo -> Bool
sokuHostTest = (contentIs "^host" <||> startsWith "^host ") <&&>
               ((== MTChat) . mType) <&&> voicePlus


sokuHostPT :: ProtoTrigger HostDB b
sokuHostPT = ProtoTrigger sokuHostTest sokuHostAct


sokuIPTest :: MessageInfo -> Bool
sokuIPTest mi =
  mType mi == MTRaw &&
  (("IP: " `T.isInfixOf` what mi) || ("IPs:" `T.isInfixOf` what mi))


sokuIPAct :: MessageInfo -> TriggerAct HostDB b ()
sokuIPAct mi = do
  HostReq rName rPort rRoom <- getRequest
  case ip rPort of
   Nothing -> return ()
   Just addrs ->
     do
       curTime <- liftIO getCurrentTime
       addRecord $ HostRec rName addrs curTime
       sendChat rRoom $ rName `T.append` " is hosting at " `T.append` addrs
  where ip portnum =
          fmap (T.intercalate ", " . fmap (`T.append` (":" `T.append` portnum))) . parseIP . what $ mi
        

sokuIPPT :: ProtoTrigger HostDB b
sokuIPPT = ProtoTrigger sokuIPTest sokuIPAct


unhostTest :: MessageInfo -> Bool
unhostTest mi = mType mi == MTChat && contentIs "^unhost" mi && voicePlus mi

unhostAct :: MessageInfo -> TriggerAct HostDB b ()
unhostAct mi = do
  mayRec <- getRecord (who mi)
  case mayRec of
   Nothing -> respond mi "You are not hosting."
   Just _  -> do
     deleteRecord (who mi)
     respond mi $ who mi `T.append` " is no longer hosting."


unhostPT :: ProtoTrigger HostDB b
unhostPT = ProtoTrigger unhostTest unhostAct


kickTest :: MessageInfo -> Bool
kickTest mi =
  startsWith "^kickhost " mi &&
  mType mi == MTChat &&
  rank mi `elem` "%@#&~"


kickAct :: MessageInfo -> TriggerAct HostDB b ()
kickAct mi =
  case T.drop 10 . what $ mi of
   ""   -> respond mi "Specify a user to kick"
   user -> do
     res <- deleteRecord user
     respond mi $ if res
                  then "User kicked."
                  else "User " `T.append` user `T.append` " is not hosting."


kickPT :: ProtoTrigger HostDB b
kickPT = ProtoTrigger kickTest kickAct


hostingTest :: MessageInfo -> Bool
hostingTest = contentIs "^hosting" <&&>
                 ((`elem` [MTChat, MTPm]) . mType)


hostingAct :: MessageInfo -> TriggerAct HostDB b ()
hostingAct mi = do
  messages <- generateHostTexts <$> liftIO getCurrentTime <*> getVar
  sendPm (who mi) "Hosts:"
  mapM_ (sendPm (who mi)) messages


hostingPT :: ProtoTrigger HostDB b
hostingPT = ProtoTrigger hostingTest hostingAct


generateHostTexts :: UTCTime -> HostDB -> [Text]
generateHostTexts curTime (recMap, _) = map recToMess recList
  where recList = map snd . M.toList $ recMap
        recToMess (HostRec hrName hrAddress hrTime) =
          createHostMessage hrName hrAddress (diffUTCTime curTime hrTime)
        createHostMessage user addr timeDif = user `T.append`
                                              " is hosting at " `T.append`
                                              addr `T.append`
                                              " (" `T.append`
                                              (T.pack . show $ timeDif) `T.append`
                                              " ago)"

sokuTrig :: Trigger
sokuTrig =
  clusterTrigger "soku" [ sokuHostPT
                        , sokuIPPT
                        , hostingPT
                        , unhostPT
                        , kickPT
                        ] emptyDB
