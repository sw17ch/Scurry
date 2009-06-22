{-# LANGUAGE DeriveDataTypeable #-}
module Scurry.Data.Message (
    SeqNumber, -- Only the type and the instances
    Payload(..),
    Packet(..),
    Control(..),
    Ack(..),
    Syn(..),
    Message(..)
) where

import Data.Data
import Data.Word

-- | Uniquely identifies a message from one host to
-- another. Each message on the network can be uniquely
-- identified by the (Src,Dst,SeqNumber) triplet.
newtype SeqNumber = SeqNumber { unSeqNumber :: Word32 }
    deriving (Data,Typeable,Eq,Ord,Show)

-- | The top level message type. We send and receive
-- encoded Payload types over the UDP socket.
data Payload = PPacket Packet
             | PControl Control
    deriving (Data,Typeable,Show)

-- | A Packet payload contains a string of bytes
-- representing a packet from the overlying network.
data Packet = Packet
    deriving (Data,Typeable,Show)

-- | A Control payload contains a message used to 
-- control the behavior of the network.
data Control = CSyn Syn
             | CAck Ack
    deriving (Data,Typeable,Show)

-- | An Ack control payload confirms the receipt of
-- a message from the sender. We need only respond
-- with the sequence number we're ack'ing, but other
-- meta data may be useful.
data Ack = Ack SeqNumber
    deriving (Data,Typeable,Show)

-- | A Syn control payload contains a message used
-- to query and control peers and information on 
-- the network.
data Syn = Syn SeqNumber Message
    deriving (Data,Typeable,Show)

-- | The different types of control messages available
-- on the network.
data Message = MInitiate   -- | Request to initiate a connection.
             | MKeepAlive  -- | Hold the UDP connection open and
                           -- query the availability of the peer.
    deriving (Data,Typeable,Show)
