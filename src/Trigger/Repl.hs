{-# LANGUAGE OverloadedStrings #-}
module Trigger.Repl (replTrig) where


import Trigger.Repl.Repl
import Control.Concurrent.Chan
import Control.Monad.IO.Class
import Needles.Bot.Trigger
import Data.List (isPrefixOf)
import Data.Text (Text)
import qualified Data.Text as T


type ReplState = Maybe (Repl [String])


importList = [ "import Prelude" ]

waitTime = 3

lineLength = 300

-- | Initializes the repl. Don't call multiple times or repls will hang around
createRepl :: TriggerAct ReplState b (Repl [String])
createRepl = do
  inChan <- liftIO newChan
  outChan <- liftIO newChan
  repl <-
    liftIO $
    repl' inChan outChan importList defaultFlags defaultBuildExpr defaultProcessOutput waitTime lineLength
  storeVar (Just repl)
  return repl


-- | Gets the current repl or initializes it
checkRepl :: TriggerAct ReplState b (Repl [String])
checkRepl = do
  currState <- getVar
  case currState of
   Nothing -> createRepl
   Just repl -> return repl


isPrefixed :: Text -> Bool
isPrefixed = T.isPrefixOf ":"


isExpr :: Text -> Bool
isExpr = T.isPrefixOf "> "


replTest :: MessageInfo -> Bool
replTest mi = (isExpr . what <||> isPrefixed . what) mi &&
              (rank mi /= ' ' && mType mi == MTChat) ||
              mType mi == MTPm


replAct :: MessageInfo -> TriggerAct ReplState b ()
replAct mi = do
  repl <- checkRepl
  result <- liftIO $ prompt repl feedString
  mapM_ (respond mi . T.pack) . take 2 $ result
  where inString = what mi
        feedText
          | isExpr inString = T.drop 2 inString
          | otherwise = inString
        feedString = T.unpack feedText


replTrig :: Trigger
replTrig = mkTrigger "repl" (ProtoTrigger replTest replAct) Nothing
