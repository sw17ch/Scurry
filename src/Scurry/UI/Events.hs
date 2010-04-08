module Scurry.UI.Events (
    EventID,

    EventCode(..),
    UIEvent(..),

    ResponseCode(..),
    UIResponse(..),
) where

type EventID = String

data EventCode = Shutdown
               | NoEvent
    deriving (Show)
data UIEvent = UIEvent { eventID :: EventID, eventCode :: EventCode }
    deriving (Show)

data ResponseCode = OK
                  | TimeOut
                  | NoResponse
    deriving (Show)
data UIResponse = UIResponse { responseID :: EventID, responseCode :: ResponseCode }
    deriving (Show)

