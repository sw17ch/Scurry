module Scurry.Config (
    appName,
    userDataDir,
    uiIndex,
) where

import System.Directory
import Paths_Scurry

appName :: String
appName = "scurry"

userDataDir :: IO FilePath
userDataDir = do
    d <- getAppUserDataDirectory appName
    createDirectoryIfMissing True d
    return d

uiIndex :: IO FilePath
uiIndex = getDataFileName "ui/index.html"
