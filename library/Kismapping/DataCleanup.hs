module Kismapping.DataCleanup
  ( removeInvalidData
  ) where

import Data.Function (on)
import qualified Data.Vector as Vector
import Data.Vector (Vector)
import Kismapping.Propagation
import Kismapping.Types
import Linear
import qualified Statistics.Sample as Statistics

meanAndStdErr :: Vector Double -> (Double, Double)
meanAndStdErr s = (mean, stdErr)
  where
    (mean, variance) = Statistics.meanVarianceUnb s
    stdDev = sqrt variance
    stdErr = stdDev / sqrt (fromIntegral $ length s)

-- | See https://en.wikipedia.org/wiki/Standard_error
removeOutliersBy :: Double -> (a -> Double) -> Vector a -> Vector a
removeOutliersBy e f s =
  Vector.filter (\x -> abs (f x - mean) <= stdErr * e) s
  where
    (mean, stdErr) = meanAndStdErr $ fmap f s

removeOutlierPoints :: Vector HeatPoint -> Vector HeatPoint 
removeOutlierPoints s = removeOutliersBy 1.96 f s
  where
    c = centerOf s
    f = falloffDistance (-75) c

removeOutlierAPs :: Vector (Vector HeatPoint) -> Vector (Vector HeatPoint)
removeOutlierAPs = removeOutliersBy 1.96 radius
  where
    e = getEuclidean . _heatPointLocation
    radius points =
      let (p, c) = perimeter (-75) points
      in maximum $ fmap (on distance e c) p

removeInvalidData :: Vector (Vector HeatPoint) -> Vector (Vector HeatPoint)
removeInvalidData =
  removeOutlierAPs .
  Vector.filter (\x -> length x >= 4) . fmap removeOutlierPoints
