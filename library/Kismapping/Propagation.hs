module Kismapping.Propagation
  ( perimeter
  , strength
  , centerOf
  , falloffDistance
  , falloffPoint
  ) where

import Control.Lens
import Control.Monad.ST (runST)
import Data.Function (on)
import Data.Semigroup
import Data.Vector (Vector)
import qualified Data.Vector as Vector
import qualified Data.Vector.Algorithms.Intro
       as Vector.Algorithms.Intro
import Kismapping.Types
import Linear

-- Falloff is -75db
-- which is a power of p
-- Power at distance d = (p * falloffDistance) / d
-- Decibels at distance d = 10 * log_10 ((p * falloffDistance) / d)
--
-- Max strength is -40 DB, Minimum is -80
--
-- Maybe use a cubic curve? Like:
--         -40dB
--          /
--    ------
--   /
-- -75dB
strength :: Euclidean -> Double -> Euclidean -> Double
strength (Euclidean c) falloffDist (Euclidean p) = db
  where
    fd' = falloffDist ^ (2 :: Int)
    d' = qd c p
    db = (-75) & dbToPower %~ (* (fd' / d'))

falloffDistance :: Double -> HeatPoint -> HeatPoint -> Double
falloffDistance falloffdb (HeatPoint (Euclidean la) dba) (HeatPoint (Euclidean lb) dbb) = dpc * dr
  where
    dpa = dba ^. dbToDistance
    dpb = dbb ^. dbToDistance
    dpc = falloffdb ^. dbToDistance
    dr = distance lb la / abs (dpb - dpa) -- Ratio from real-world distance to power-distance

falloffPoint :: Double -> HeatPoint -> HeatPoint -> HeatPoint
falloffPoint falloffdb a@(HeatPoint (Euclidean la) _) b@(HeatPoint (Euclidean lb) _) =
  HeatPoint (Euclidean $ la + dir ^* fd) falloffdb
  where
    fd = falloffDistance falloffdb a b
    dir = normalize $ lb - la


dbToPower :: Iso' Double Double
dbToPower = iso toPower fromPower
  where
    toPower db = 10 ** (db / 10)
    fromPower p = 10 * logBase 10 p

-- Power = 1 / (distance^2)
-- distance = sqrt (1 / power)
-- This is a unitless value and is useless without a reference "real" position
powerToDistance :: Iso' Double Double
powerToDistance = iso toDistance fromDistance
  where
    toDistance p = sqrt (1 / p)
    fromDistance d = 1 / (d ^ (2 :: Int))

dbToDistance :: Iso' Double Double
dbToDistance = dbToPower . powerToDistance
    
-- | Calculate access point position given a number of signal readings via
-- trilateration.
-- https://stackoverflow.com/questions/3472469/trilateration-in-a-2d-plane-with-signal-strengths
centerOf :: Foldable f => f HeatPoint -> HeatPoint
centerOf inPoints = HeatPoint center 0
  where
    radius = view dbToDistance
    (Sum sumP, Sum sumR) =
      flip foldMap inPoints $ \(HeatPoint p r) ->
        (Sum $ getEuclidean p ^* radius r, Sum $ radius r)
    center = Euclidean $ sumP ^/ sumR

rotation :: HeatPoint -> HeatPoint -> Double
rotation (HeatPoint a _) (HeatPoint b _) = atan2 lat lon
  where
    Polar pa = toPolar a
    Polar pb = toPolar b
    V3 lon lat _ = pb - pa

perimeter :: Double -> Vector HeatPoint -> (Vector HeatPoint, HeatPoint)
perimeter falloffdb points = (p, c)
  where
    p =
      runST $ do
        v <- Vector.thaw (fmap (falloffPoint falloffdb c) points)
        Vector.Algorithms.Intro.sortBy (on compare (rotation c)) v
        Vector.freeze v
    c = centerOf points

