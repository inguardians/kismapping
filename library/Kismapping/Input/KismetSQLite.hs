{-# LANGUAGE OverloadedStrings #-}
module Kismapping.Input.KismetSQLite
  ( readGpsSQLite
  ) where

import Control.Applicative
import Control.Monad
import Data.Aeson (FromJSON, Value(..), (.:), decodeStrict', parseJSON)
import Data.Foldable
import Data.HashMap.Lazy (HashMap)
import qualified Data.HashMap.Lazy as HashMap
import Data.Maybe
import Data.Monoid
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Vector as Vector
import Data.Vector (Vector)
import qualified Database.SQLite3 as SQLite
import Database.SQLite3 (ColumnIndex(..), Database)
import qualified Filesystem.Path.CurrentOS as Path
import Kismapping.Input.KismetPoints
import Kismapping.Types
import Linear.V3
-- https://www.stackage.org/lts-8.20/package/direct-sqlite-2.3.19


{-SELECT * FROM devices WHERE type = "Wi-Fi AP"-}

{-SELECT lat, lon, alt, signal FROM packets WHERE sourcemac = "48:5D:36:60:C1:4A"-}

{-or, if we want to do the filtering ourselves.-}

{-SELECT lat, lon, alt, signal, sourcemac FROM packets-}


execAndCollect ::
     SQLite.Database -> Text -> (SQLite.Statement -> IO a) -> IO [a]
execAndCollect db query processRow = do
  statement <- SQLite.prepare db query

  let lp SQLite.Done = pure []
      lp SQLite.Row = do
        value <- processRow statement
        nextStep <- SQLite.step statement
        rest <- lp nextStep
        pure (value : rest)

  firstStep <- SQLite.step statement
  results <- lp firstStep
  SQLite.finalize statement

  pure results



data AccessPointDetails = AccessPointDetails
  { accessPointMacAddress :: Text
  , accessPointName :: Text
  }

instance FromJSON AccessPointDetails where
  parseJSON (Object v) =
    AccessPointDetails <$>
    v .: "kismet.device.base.macaddr" <*>
    v .: "kismet.device.base.name"
  parseJSON _ = empty

-- returns a HashMap that maps a BSSID to the corresponding ESSID
retrieveAccessPoints :: SQLite.Database -> IO (HashMap Text Text)
retrieveAccessPoints db = do
  deviceJsonBlobs <-
    execAndCollect
      db
      "SELECT device FROM devices WHERE type = \"Wi-Fi AP\""
      (\statement -> SQLite.columnBlob statement (ColumnIndex 0))

  let deviceDetails = mapMaybe decodeStrict' deviceJsonBlobs
      devicePairs =
        map (\(AccessPointDetails mac name) -> (mac, name)) deviceDetails

  return (HashMap.fromList devicePairs)
  

retrieveReadings :: SQLite.Database -> IO EssidMap
retrieveReadings db = do
  accessPointMap <- retrieveAccessPoints db
  readingInsertions <-
    execAndCollect
      db
      "SELECT lat, lon, signal, sourcemac FROM packets"
      (\statement -> do
         lat <- SQLite.columnDouble statement (ColumnIndex 0)
         lon <- SQLite.columnDouble statement (ColumnIndex 1)
         sig <- SQLite.columnDouble statement (ColumnIndex 2)
         mac <- SQLite.columnText statement (ColumnIndex 3)
         let polar = Polar (V3 lon lat 0)
         return
           (case HashMap.lookup mac accessPointMap of
              Nothing -> Endo id
              Just essid -> Endo (insertGpsPoint essid mac polar sig)))
  let readings = appEndo (fold readingInsertions) HashMap.empty
  return readings
  
collectReadingsFromFiles :: (Vector Path.FilePath) -> IO EssidMap
collectReadingsFromFiles files = do
  allReadings <-
    forM files $ \file -> do
      database <- SQLite.open (Text.pack (Path.encodeString file))
      retrieveReadings database
  return (foldr unionEssidMaps HashMap.empty allReadings)

readGpsSQLite ::
     (Vector Path.FilePath) -> (Vector Text) -> IO (Vector (Vector HeatPoint))
readGpsSQLite files names = do
  readings <- collectReadingsFromFiles files
  let pointsForName name =
        toHeatpoints (HashMap.lookupDefault HashMap.empty name readings)
  return (Vector.concatMap pointsForName names)

