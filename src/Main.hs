{-# LANGUAGE OverloadedStrings #-}
module Main where

import Control.Exception
import Control.Lens hiding (argument)
import Control.Monad
import Data.Aeson
import qualified Data.ByteString.Lazy.Char8 as BL
import qualified Data.Map as Map
import Data.Maybe (fromMaybe)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import Data.Time.Clock
import Network.Wreq hiding (header, options)
import Options.Applicative
import System.Directory

data RHELVersion = EL5 | EL6 | EL7
                 deriving (Eq, Show)

rhelToFilename :: RHELVersion -> String
rhelToFilename EL5 = "pkg_el5.json"
rhelToFilename EL6 = "pkg_el6.json"
rhelToFilename EL7 = "pkg_el7.json"
{-# INLINE rhelToFilename #-}

strToRhel :: String -> Maybe RHELVersion
strToRhel "5" = Just EL5
strToRhel "6" = Just EL6
strToRhel "7" = Just EL7
strToRhel _   = Nothing
{-# INLINE strToRhel #-}

allRhelVersions :: [RHELVersion]
allRhelVersions = [EL5, EL6, EL7]
{-# INLINE allRhelVersions #-}

jsonURL :: RHELVersion -> String
jsonURL ver =
  "https://infrastructure.fedoraproject.org/repo/json/" ++ rhelToFilename ver
{-# INLINE jsonURL #-}

toVersionError :: String -> RHELVersion
toVersionError ver =
  let elver = strToRhel ver
  in fromMaybe (error "Invalid version specified") elver
{-# INLINE toVersionError #-}

data PkgResults = PkgResults {
    responseArches   :: [T.Text]
  , responsePackages :: Map.Map T.Text Package
  } deriving (Eq, Ord, Show)

instance FromJSON PkgResults where
    parseJSON (Object v) = PkgResults <$>
                           v .: "arches" <*>
                           v .: "packages"
    parseJSON _          = mzero

data Package = Package {
    packageRelease :: T.Text
  , packageVersion :: T.Text
  , packageEpoch   :: T.Text
  , packageArch    :: [T.Text]
  , packageChannel :: [T.Text]
  } deriving (Eq, Ord, Show)

instance FromJSON Package where
    parseJSON (Object v) = Package <$>
                           v .: "release" <*>
                           v .: "version" <*>
                           v .: "epoch" <*>
                           v .: "arch" <*>
                           v .: "channel"
    parseJSON _          = mzero

data GlobalOptions =
  GlobalOptions {
    globalOptionOffline :: Bool
  , globalOptioncommand :: Command
  } deriving (Eq, Show)

data RefreshOptions =
  RefreshOptions {
    refreshOptionsRhelVersion :: String
  } deriving (Eq, Show)

data InfoOptions =
  InfoOptions {
    infoOptionsRhelVersion :: String
  , infoQuery              :: String
  } deriving (Eq, Show)

data Command =
  Refresh RefreshOptions
  | Info InfoOptions
  deriving (Eq, Show)

globalOptions :: Parser Command -> Parser GlobalOptions
globalOptions cmd = GlobalOptions
                    <$> switch ( long "offline"
                              <> help "Don't do anything requiring network" )
                    <*> cmd

refreshOptions :: Parser Command
refreshOptions = Refresh <$> (RefreshOptions <$> argument str (
                                  metavar "RHEL_VERSION"
                               <> help "Numeric of RHEL to refresh. Defaults to 'all'."
                               <> value "all"))

infoOptions :: Parser Command
infoOptions = Info <$> (InfoOptions <$>
                            strOption ( metavar "RHEL_VERSION"
                                     <> value "7"
                                     <> long "version"
                                     <> help "Numeric RHEL version to search.")
                       <*> argument str ( metavar "QUERY" ))

options :: Parser GlobalOptions
options = subparser $
          command "refresh" (info (helper <*> globalOptions refreshOptions) $
                             fullDesc
                          <>  progDesc "Refresh local caches from Fedora" )
       <> command "info" (info (helper <*> globalOptions infoOptions) $
                            fullDesc
                          <>  progDesc "Search packages by name" )

info' :: InfoOptions -> GlobalOptions -> IO ()
info' (InfoOptions ver query) (GlobalOptions offline _) = do
  res <- jsonFile (toVersionError ver) offline False
  case res of
    Just r ->
      case decode r :: Maybe PkgResults of
        Just res' ->
          case responsePackages res' ^. at (T.pack query) of
            Just pkg -> do
              pkgPrint pkg "Version" packageVersion
              pkgPrint pkg "Release" packageRelease
              pkgPrint pkg "Epoch" packageEpoch
              pkgPrintList pkg "Arch" packageArch
              pkgPrintList pkg "Channel" packageChannel
            Nothing -> error "Package not found."
        Nothing -> error "Unable to decode JSON. Try `rhelsearch refresh`?"
    Nothing -> error "No cached files found and unable to retrieve new files"
  where
    pkgPrint pkg label f = T.putStrLn $ label <> ": " <> f pkg
    pkgPrintList pkg label f = T.putStrLn $ label <> ": " <> T.pack (show (f pkg))


runRHELSearch :: GlobalOptions -> IO ()
runRHELSearch g@(GlobalOptions offline cmd) = case cmd of
  Refresh (RefreshOptions ver) ->
    case ver of
      "all" -> mapM_ refresh allRhelVersions
      _ -> refresh (toVersionError ver)
  Info opts -> info' opts g
  where
    refresh :: RHELVersion -> IO ()
    refresh version = do
      putStrLn $ "Refreshing " ++ show version
      res <- jsonFile version offline True
      case res of
        Just _ -> return ()
        Nothing -> error "No cached files found and unable to retrieve new files"

main :: IO ()
main = execParser opts >>= runRHELSearch
  where
    opts = info (helper <*> options)
      ( fullDesc
     <> progDesc "Search RHEL packages"
     <> header "rhelsearch - search RHEL packages" )

-- | Create cache directory if missing, then return path to the cache file for
-- a given RHEL version.
cachedFile :: RHELVersion -> IO String
cachedFile ver = do
  home <- getHomeDirectory
  let path = home ++ "/.cache/rhelsearch/"
  createDirectoryIfMissing True path
  return $ path ++ rhelToFilename ver

-- | Given a 'RHELVersion', get a 'BL.ByteString', by [TODO: write docs]
jsonFile :: RHELVersion -> Bool -> Bool -> IO (Maybe BL.ByteString)
jsonFile ver True _ = do
  -- offline = True
  cacheFile <- cachedFile ver
  exists <- doesFileExist cacheFile
  if exists
    then openAndReturn cacheFile
    else return Nothing
jsonFile ver False force = do
  -- offline = False
  cacheFile <- cachedFile ver
  current <- getCurrentTime
  mdtm <- try (getModificationTime cacheFile) :: IO (Either IOException UTCTime)
  case mdtm of
    Left _ -> storeAndReturn ver
    Right t ->
      let offset = fromRational . toRational $ diffUTCTime current t :: Double
      in if force || offset > 604800 -- seconds in a week
         then storeAndReturn ver
         else openAndReturn cacheFile

storeFile :: RHELVersion -> IO BL.ByteString
storeFile ver = do
  resp <- get (jsonURL ver)
  cacheFile <- cachedFile ver
  BL.writeFile cacheFile (resp ^. responseBody)
  return (resp ^. responseBody)

storeAndReturn :: RHELVersion -> IO (Maybe BL.ByteString)
storeAndReturn ver = Just <$> storeFile ver

openAndReturn :: FilePath -> IO (Maybe BL.ByteString)
openAndReturn cacheFile = Just <$> BL.readFile cacheFile
