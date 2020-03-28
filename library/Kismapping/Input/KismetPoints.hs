{-# LANGUAGE OverloadedStrings #-}

module Kismapping.Input.KismetPoints where

import Data.Foldable
import Data.HashMap.Lazy (HashMap)
import qualified Data.HashMap.Lazy as HashMap
import Data.Hashable
import Data.Monoid
import Data.Sequence (Seq)
import qualified Data.Sequence as Seq
import Data.Text (Text)
import Data.Vector (Vector)
import qualified Data.Vector as Vector
import Kismapping.Types
import qualified Statistics.Sample as Statistics

type EssidMap = HashMap Text BssidMap

type BssidMap = HashMap Text APReadings

-- APReadings is a Map from points in space (using polar coordinates) to signal readings
type APReadings = HashMap HashablePolar (Seq Double)

newtype HashablePolar =
  HashablePolar Polar
  deriving (Eq)

instance Hashable HashablePolar where
  hashWithSalt salt (HashablePolar (Polar x)) = hashWithSalt salt x

-- | Given a Map of BSSID keys to ESSID values, get the Essid for a given BSSID.
-- If none is found, use "<HIDDEN>" as the Essid, because the BSSID has no
-- corresponding ESSID.
lookupEssid :: HashMap Text Text -> Text -> Text
lookupEssid ssidMap bssid = HashMap.lookupDefault "" bssid ssidMap

-- Unify two BSSID maps, preserving datapoints from both.
unionBssidMaps :: BssidMap -> BssidMap -> BssidMap
unionBssidMaps = HashMap.unionWith (HashMap.unionWith (<>))

unionEssidMaps :: EssidMap -> EssidMap -> EssidMap
unionEssidMaps = HashMap.unionWith unionBssidMaps

-- Creates a BssidMap for a single datapoint
bssidMapSingleton :: Text -> Polar -> Double -> BssidMap
bssidMapSingleton bssid loc db =
  HashMap.singleton
    bssid
    (HashMap.singleton (HashablePolar loc) (Seq.singleton db))

insertGpsPoint :: Text -> Text -> Polar -> Double -> EssidMap -> EssidMap
insertGpsPoint essid bssid loc db =
  HashMap.insertWith unionBssidMaps essid (bssidMapSingleton bssid loc db)

toHeatpoints :: BssidMap -> Vector (Vector HeatPoint)
toHeatpoints bssidMap =
  let meanHeatpoint (HashablePolar p, dbSeq) =
        HeatPoint
          (fromPolar p)
          (Statistics.mean (Vector.fromList (toList dbSeq)))
      bssidHeatpoints apReadings =
        Vector.fromList (fmap meanHeatpoint (HashMap.toList apReadings))
   in Vector.fromList (toList (fmap bssidHeatpoints bssidMap))
