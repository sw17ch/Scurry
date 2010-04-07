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
data UIEvent = UIEvent EventID EventCode
    deriving (Show)

data ResponseCode = OK
    deriving (Show)
data UIResponse = UIResponse EventID ResponseCode
    deriving (Show)

