{-# LANGUAGE OverloadedStrings #-}
module Main where

import Control.Applicative
import Control.Exception
import Control.Lens
import Data.Aeson
import Data.Aeson.Lens
import qualified Data.ByteString.Lazy.Char8 as BL
import Data.Time.Clock
import Data.Time.Format.Human
import Network.Wreq
import System.Directory

data RHELVersion = EL5 | EL6 | EL7

rhelToFilename :: RHELVersion -> String
rhelToFilename EL5 = "pkg_el5.json"
rhelToFilename EL6 = "pkg_el6.json"
rhelToFilename EL7 = "pkg_el7.json"
{-# INLINE rhelToFilename #-}

jsonURL :: RHELVersion -> String
jsonURL ver =
  "https://infrastructure.fedoraproject.org/repo/json/" ++ rhelToFilename ver
{-# INLINE jsonURL #-}

-- | Create cache directory if missing, then return path to the cache file for
-- a given RHEL version.
cachedFile :: RHELVersion -> IO String
cachedFile ver = do
  home <- getHomeDirectory
  let path = home ++ "/.cache/rhelsearch/"
  createDirectoryIfMissing True path
  return $ path ++ rhelToFilename ver

-- | Given a 'RHELVersion', get a 'BL.ByteString' by using the following logic:
--
--
jsonFile :: RHELVersion -> Bool -> IO (Maybe BL.ByteString)
jsonFile ver True = do
  -- offline = True
  cacheFile <- cachedFile ver
  exists <- doesFileExist cacheFile
  if exists
    then openAndReturn cacheFile
    else return Nothing
jsonFile ver False = do
  -- offline = False
  cacheFile <- cachedFile ver
  current <- getCurrentTime
  mdtm <- try (getModificationTime cacheFile) :: IO (Either IOException UTCTime)
  case mdtm of
    Left _ -> storeAndReturn ver
    Right t ->
      let offset = fromRational . toRational $ diffUTCTime current t :: Double
      in if offset > 604800 -- seconds in a week
         then storeAndReturn ver
         else openAndReturn cacheFile

storeFile :: RHELVersion -> IO BL.ByteString
storeFile ver = do
  resp <- get (jsonURL ver)
  cacheFile <- cachedFile ver
  BL.writeFile cacheFile (resp ^. responseBody)
  return (resp ^. responseBody)

storeAndReturn ver = Just <$> storeFile ver
openAndReturn cacheFile = Just <$> BL.readFile cacheFile


main :: IO ()
main = error "boo!"
