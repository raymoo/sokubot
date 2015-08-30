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


-- | Parse a list of IPs out of some Text
parseIP :: Text -> Maybe [Text]
parseIP = either (const Nothing) Just . AP.parseOnly ipFromRaw


-- | The actual parser for parsing the IPs out
ipFromRaw :: Parser [Text]
ipFromRaw = prelude *> (ipv4T `AP.sepBy1` (AP.string ", "))
  where prelude = -- ^ Parses everything that comes before the IP addresses
          AP.takeWhile1 (/= 'I') *> ((AP.string "IP: " <|> AP.string "IPs: ") <|> (AP.letter *> prelude))
        ipv4T = fmap (T.intercalate "." . map (T.pack . show)) ipv4 -- ^ Parser that parses the
                                                                    -- textified IP address


-- | IPv4 address parser, parses to a list of 'Integer'
ipv4 :: Parser [Integer]
ipv4 = makeList <$> AP.decimal <*> oneAfter <*> oneAfter <*> oneAfter
  where makeList x y z a = [x,y,z,a]
        oneAfter = AP.char '.' *> AP.decimal


-- | A record representing a single host.
data HostRec = HostRec { hrName :: Text, hrAddress :: Text, hrTime :: UTCTime }


-- | A record representing a request to host. This is used to hold the current
-- requester until the server sends back an IP address.
data HostReq = HostReq { hrqName :: Text, hrqPort :: Text, hrqRoom :: Text }


-- | State for the trigger, containing a map of hosts, as well as the last
-- person to make a request.
type HostDB = (Map Text HostRec, HostReq)


-- | 'MessageInfo' predicate checking the sender for voice or up
voicePlus :: MessageInfo -> Bool
voicePlus mi = rank mi `elem` "+%@#&~"


-- | DB with no hosts and no previous request.
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
  addRequest $ HostReq (who mi) port (mRoom mi) -- ^ Set the sender as the current requester
  command (mRoom mi) ("/ip " `T.append` who mi) -- ^ Ask the server for the sender's IP
  where port = case T.drop 6 . what $ mi of
                "" -> "10800"
                x  -> x


-- | User must have at least voice, and must make a host request in chat
sokuHostTest :: MessageInfo -> Bool
sokuHostTest = (contentIs "^host" <||> startsWith "^host ") <&&>
               ((== MTChat) . mType) <&&> voicePlus


sokuHostPT :: ProtoTrigger HostDB b
sokuHostPT = ProtoTrigger sokuHostTest sokuHostAct


-- | Does a message contain an IP?
sokuIPTest :: MessageInfo -> Bool
sokuIPTest mi =
  mType mi == MTRaw &&
  (("IP: " `T.isInfixOf` what mi) || ("IPs:" `T.isInfixOf` what mi))


sokuIPAct :: MessageInfo -> TriggerAct HostDB b ()
sokuIPAct mi = do
  HostReq rName rPort rRoom <- getRequest -- ^ Get the current requester's info
  case ip rPort of
   Nothing -> return () -- ^ IP failed to parse for some reason
   Just addrs ->
     do
       curTime <- liftIO getCurrentTime
       addRecord $ HostRec rName addrs curTime
       sendChat rRoom $ rName `T.append` " is hosting at " `T.append` addrs
  where ip portnum = -- ^ Formats the IP parsed addresses, given a port number
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


-- | Drivers and up can kick anyone, including other auth
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


-- | Generate the info messages sent in host lists.
generateHostTexts :: UTCTime -> HostDB -> [Text]
generateHostTexts curTime (recMap, _) = case recList of
                                         [] -> nobodyMess
                                         recs -> map recToMess recs
  where nobodyMess = ["Nobody is hosting"]
        recList = map snd . M.toList $ recMap
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
  clusterTrigger "soku" [ sokuHostPT -- ^ For starting to host 
                        , sokuIPPT -- ^ Receiving IPs
                        , hostingPT -- ^ Host list request
                        , unhostPT -- ^ Unhosting
                        , kickPT -- ^ Kicking hosters
                        ] emptyDB
