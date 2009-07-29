module Scurry.Exception (
) where

import Control.Exception.Base

data ScurryException = Teardown
    deriving (Show,Data,Typeable)
