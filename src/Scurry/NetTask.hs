module Scurry.NetTask (
    NetMsg(..),
    netTask,
) where

import Control.Concurrent.MVar
import Control.Monad
import Data.ByteString.Lazy

import Scurry.Crypto
import Scurry.Scurry

data NetMsg = Reject
            | Join   { key :: ScurryPubKey }
            | Accept { key :: ScurryPubKey }
            | EncMsg { msg :: ByteString }
    deriving (Show)

netTask :: MVar Scurry -> MVar () -> IO ()
netTask s c = forever $ return ()
