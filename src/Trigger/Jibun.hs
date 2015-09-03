{-# LANGUAGE OverloadedStrings #-}
module Trigger.Jibun (jibunTrig) where

import Needles.Bot.Trigger
import Data.Text (Text)
import qualified Data.Text as T


jibunwo :: Text
jibunwo = "JIBUN WOOOOOO"


jibunTrig :: Trigger
jibunTrig = mkTrigger "jibunwo" (ProtoTrigger jibunTest jibunAct) ()


jibunTest :: MessageInfo -> Bool
jibunTest mi = contentIs ".jibun" mi && (rank mi `elem` "+%@#~&")


jibunAct :: MessageInfo -> TriggerAct a b ()
jibunAct mi = respond mi jibunwo
