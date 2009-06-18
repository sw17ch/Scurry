{-# LANGUAGE DeriveDataTypeable #-}
module Data.Scurry (
    Scurry(..),
) where

import Data.Data

data Scurry = Scurry
    deriving (Data,Typeable,Show)
