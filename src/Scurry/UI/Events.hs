{-# LANGUAGE DeriveDataTypeable #-}

module Scurry.UI.Events (
    EventID,

    EventCode(..),
    UIEvent(..),

    ResponseCode(..),
    UIResponse(..),
) where

import Data.Data

type EventID = String

data EventCode = Shutdown
               | NoEvent
    deriving (Show,Data,Typeable)
data UIEvent = UIEvent { eventID :: EventID, eventCode :: EventCode }
    deriving (Show,Data,Typeable)

data ResponseCode = OK
                  | TimeOut
                  | NoResponse
    deriving (Show,Data,Typeable)
data UIResponse = UIResponse { responseID :: EventID, responseCode :: ResponseCode }
    deriving (Show,Data,Typeable)

