{-# LANGUAGE DeriveDataTypeable #-}
module Scurry.Exception (
    ScurryException(..),
) where

import Data.Data
import Control.Exception.Base

data ScurryException = TearDown
    deriving (Show,Typeable)

instance Exception ScurryException


