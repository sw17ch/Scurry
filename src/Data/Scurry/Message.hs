{-# LANGUAGE DeriveDataTypeable #-}
module Data.Scurry.Message (
    SeqNumber, -- Only the type and the instances
    Message(..),
    Packet(..),
    Manage(..),
    Ack(..),
    Syn(..)
) where

import Data.Data
import Data.Word

newtype SeqNumber = SeqNumber { unSeqNumber :: Word32 }
    deriving (Data,Typeable,Eq,Ord,Show)

data Message = MPacket Packet
             | MManage
    deriving (Data,Typeable,Show)

data Packet = Packet
    deriving (Data,Typeable,Show)

data Manage = MSyn Syn
            | MAck Ack
    deriving (Data,Typeable,Show)

data Ack = Ack
    deriving (Data,Typeable,Show)

data Syn = Syn
    deriving (Data,Typeable,Show)
