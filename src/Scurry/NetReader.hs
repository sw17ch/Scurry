module Scurry.NetReader (
) where

import Scurry.Crypto
import Data.ByteString.Lazy

data NetMsg = Reject
            | Join   { key :: ScurryPubKey }
            | Accept { key :: ScurryPubKey }
            | EncMsg { msg :: ByteString }
    deriving (Show)
