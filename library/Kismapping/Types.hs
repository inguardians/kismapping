{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
module Kismapping.Types
  ( Euclidean(..)
  , Polar(..)
  , Degrees(..)
  , Radians(..)
  , HeatPoint(..)
  , heatPointStrength
  , heatPointLocation
  , Render(..)
  , Rendered(..)
  , Polygon(..)
  , PolyPoint(..)
  , lonlatToEuclidean
  , lonlatFromEuclidean
  , euclideanBounds
  , toPolar
  , fromPolar
  , MapRegion(..)
  , toMapRegion
  , fromMapRegion
  , toRadians
  , fromRadians
  , module Data.Region
  ) where

import Algebra.Lattice
import Codec.Picture
import Control.Lens
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Types as Aeson
import Data.Char
import Data.Region
import qualified Data.Text as Text
import qualified Data.Vector as Vector
import GHC.Generics (Generic)
import Linear

newtype Euclidean = Euclidean
  { getEuclidean :: V3 Double
  } deriving (Eq, Read, Show)

newtype Polar = Polar
  { getPolar :: V3 Double
  } deriving (Eq, Read, Show)

newtype Degrees = Degrees
  { getDegrees :: Double
  } deriving (Eq, Read, Show)

newtype Radians = Radians
  { getRadians :: Double
  } deriving (Eq, Read, Show)

data HeatPoint = HeatPoint
  { _heatPointLocation :: Euclidean
  , _heatPointStrength :: Double
  } deriving (Eq, Read, Show)

data PolyPoint = PolyPoint
  { polyPointLng :: Double
  , polyPointLat :: Double
  } deriving (Eq, Read, Show, Generic)

data Polygon = Polygon
  { polygonPaths :: Vector.Vector PolyPoint
  , polygonStrokeColor :: Text.Text
  , polygonStrokeOpacity :: Double
  , polygonStrokeWeight :: Double
  , polygonFillColor :: Text.Text
  , polygonFillOpacity :: Double
  } deriving (Eq, Read, Show, Generic)

data MapRegion = MapRegion
  { mapRegionWest :: Double
  , mapRegionSouth :: Double
  , mapRegionEast :: Double
  , mapRegionNorth :: Double
  } deriving (Eq, Read, Show, Generic)

  
data Rendered a where
  RenderedImage :: Image PixelRGBA8 -> Rendered (Image PixelRGBA8)
  RenderedPolygons :: Vector.Vector Polygon -> Rendered (Vector.Vector Polygon)

newtype Render s a = Render { runRender :: s -> (MapRegion, Rendered a) }

makeWrapped ''Euclidean

makeWrapped ''Polar

makeWrapped ''Degrees

makeWrapped ''Radians

makeLenses ''HeatPoint

-- Conversions between types

toMapRegion :: Region Double -> MapRegion
toMapRegion (Region west south east north) = MapRegion west south east north

fromMapRegion :: MapRegion -> Region Double
fromMapRegion (MapRegion west south east north) = Region west south east north

lonlatFromEuclidean :: Euclidean -> V2 Double
lonlatFromEuclidean p = V2 lon lat
  where
    Polar (V3 lon lat _) = toPolar p

lonlatToEuclidean :: V2 Double -> Euclidean
lonlatToEuclidean (V2 lon lat) = fromPolar . Polar $ V3 lon lat 0

euclideanBounds :: Euclidean -> Double -> Region Double
euclideanBounds c f =
  foldMapBy (\/) rempty (region . lonlatFromEuclidean) corners
  where
    Euclidean (V3 x y z) = c
    corners =
      [ Euclidean $ V3 (x + dx) (y + dy) (z + dz)
      | dx <- [-f, f]
      , dy <- [-f, f]
      , dz <- [-f, f]
      ]
    region (V2 lon lat) = fromPoint lon lat
    rempty = region $ lonlatFromEuclidean c

toRadians :: Degrees -> Radians
toRadians (Degrees d) = Radians $ d * pi / 180

fromRadians :: Radians -> Degrees
fromRadians (Radians r) = Degrees $ r * 180 / pi

-- | Approximate earth radius in meters.
earthRadius :: Double
earthRadius = 6371000

-- | Convert polar coordinates to euclidean coordinates within the context of
-- the earth. Latitude must be within the range [-180, 180].
fromPolar :: Polar -> Euclidean
fromPolar (Polar (V3 lon lat alt))
  | lat < -180 || lat > 180 =
    error $
    "*** Exception: Kismapping.Types.fromPolar: latitude must be within [-180, 180], was " ++
    show lat
  | otherwise = Euclidean $ V3 x y z ^* (alt + earthRadius)
  where
    Radians lon' = toRadians $ Degrees lon
    Radians lat' = toRadians $ Degrees lat
    r = cos lat'
    x = r * cos lon'
    y = sin lat'
    z = r * sin lon'

-- | Convert euclidean coordinates to polar coordinates within the context of
-- the earth.
toPolar :: Euclidean -> Polar
toPolar (Euclidean p@(V3 x y z)) = Polar $ V3 lon lat alt
  where
    alt = norm p - earthRadius -- Distance from surface of earth
    Degrees lat = fromRadians . Radians $ asin (y / norm p)
    Degrees lon = fromRadians . Radians $ atan2 z x

-- JSON instances

jsonOptsPrefix :: String -> Aeson.Options
jsonOptsPrefix prefix =
  Aeson.defaultOptions
  {Aeson.fieldLabelModifier = over _head toLower . drop (length prefix)}

polyPointJsonOptions :: Aeson.Options
polyPointJsonOptions = jsonOptsPrefix "polyPoint"

instance Aeson.ToJSON PolyPoint where
  toEncoding = Aeson.genericToEncoding polyPointJsonOptions
  toJSON = Aeson.genericToJSON polyPointJsonOptions

instance Aeson.FromJSON PolyPoint where
  parseJSON = Aeson.genericParseJSON polyPointJsonOptions

polygonJsonOptions :: Aeson.Options
polygonJsonOptions = jsonOptsPrefix "polygon"

instance Aeson.ToJSON Polygon where
  toEncoding = Aeson.genericToEncoding polygonJsonOptions
  toJSON = Aeson.genericToJSON polygonJsonOptions

instance Aeson.FromJSON Polygon where
  parseJSON = Aeson.genericParseJSON mapRegionJsonOptions

mapRegionJsonOptions :: Aeson.Options
mapRegionJsonOptions = jsonOptsPrefix "mapRegion"

instance Aeson.ToJSON MapRegion where
  toEncoding = Aeson.genericToEncoding mapRegionJsonOptions
  toJSON = Aeson.genericToJSON mapRegionJsonOptions

instance Aeson.FromJSON MapRegion where
  parseJSON = Aeson.genericParseJSON mapRegionJsonOptions
