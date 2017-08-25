{-# LANGUAGE OverloadedStrings #-}
module Kismapping.Render.Polygon
  ( renderPolygons
  ) where

import Algebra.Lattice
import Data.Reflection (foldMapBy)
import Data.Vector (Vector)
import Kismapping.Propagation
import Kismapping.Types
import Linear

polyPoint :: HeatPoint -> PolyPoint
polyPoint (HeatPoint position _) = PolyPoint lon lat
  where
    V2 lon lat = lonlatFromEuclidean position

polygon :: Vector HeatPoint -> Polygon
polygon points =
  Polygon
  { polygonPaths = fmap polyPoint . fst $ perimeter (-75) points
  , polygonStrokeColor = "#FF0000"
  , polygonStrokeOpacity = 0.0
  , polygonStrokeWeight = 0.0
  , polygonFillColor = "#FF0000"
  , polygonFillOpacity = 0.5
  }

polygonBounds :: Region Double -> Polygon -> Region Double
polygonBounds rempty = foldMapBy (\/) rempty r . polygonPaths
  where
    r (PolyPoint lon lat) = fromPoint lon lat

polygonGroupBounds :: Region Double -> Vector Polygon -> Region Double
polygonGroupBounds rempty = foldMapBy (\/) rempty $ polygonBounds rempty

polygonGroup :: Vector (Vector HeatPoint) -> Vector Polygon
polygonGroup = fmap polygon

polygons :: Vector (Vector HeatPoint) -> (MapRegion, Rendered (Vector Polygon))
polygons points = (toMapRegion region, RenderedPolygons polys)
  where
    rempty = Region 180 90 (-180) (-90)
    polys = polygonGroup points
    region = polygonGroupBounds rempty polys

renderPolygons :: Render (Vector (Vector HeatPoint)) (Vector Polygon)
renderPolygons = Render polygons
