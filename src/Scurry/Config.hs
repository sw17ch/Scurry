module Scurry.Config (
    appName,
    userDataDir,
    uiFile,
    indexFile,
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

indexFile :: String
indexFile = "/scurry.html"

uiFile :: String -> IO (Maybe FilePath)
uiFile m = do
    n <- getDataFileName $ "ui" ++ m
    e <- doesFileExist n

    return $ case e of
                False -> Nothing
                True -> Just n
