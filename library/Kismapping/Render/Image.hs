{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE Strict #-}
module Kismapping.Render.Image
  ( renderImage
  ) where

import Algebra.Lattice
import Codec.Picture
import Codec.Picture.Types
import Control.Lens
import Data.Array.Repa ((:.)(..), Array, D, DIM2, U, Z(..))
import qualified Data.Array.Repa as R
import Data.Colour.RGBSpace.HSL
import Data.Colour.SRGB
import Data.Foldable
import Data.Monoid
import qualified Data.Sequence as Seq
import Data.Sequence (Seq)
import Data.Vector (Vector)
import qualified Data.Vector as Vector
import Data.Word
import Kismapping.Propagation
import Kismapping.Types
import Linear
import qualified Statistics.Sample as Statistics

imageDivisionFactor :: Int
imageDivisionFactor = 7

minSignalStrength :: Double
minSignalStrength = -75

maxSignalStrength :: Double
maxSignalStrength = -40

strengthDefault :: Euclidean -> Int
strengthDefault = const (gradientResolution - 1)

gradientResolution :: Int
gradientResolution = 8192

gradient :: Vector Word32
gradient = Vector.generate res (packPixel . f)
  where
    res = fromIntegral gradientResolution
    f i =
      color $
      strength
        (Euclidean (V3 0 0 0))
        1
        (Euclidean (V3 (sqrt $ fromIntegral i / fromIntegral (res - 1)) 0 0))

data Heatmap = Heatmap
  { heatmapRegion :: Region Double
  , heatmapDistSq :: Euclidean -> Int
  }

subdivide :: Region Double -> Seq (Region Double)
subdivide region =
  Seq.unstableSortBy compareRegion (f (imageDivisionFactor, region))
  where
    f (0, r) = Seq.singleton r
    f (n, Region minX minY maxX maxY) = f tl <> f tr <> f bl <> f br
      where
        w = maxX - minX
        h = maxY - minY
        midX = minX + w / 2
        midY = minY + h / 2
        tl = (n - 1, Region minX minY midX midY)
        tr = (n - 1, Region midX minY maxX midY)
        bl = (n - 1, Region minX midY midX maxY)
        br = (n - 1, Region midX midY maxX maxY)
    compareRegion (Region lxa lya _ _) (Region lxb lyb _ _) =
      compare (lya, lxa) (lyb, lxb)

blocks :: Region Double -> Vector Heatmap -> Vector (Euclidean -> Int)
blocks region heatmaps = Vector.fromList (toList (fmap distFunc regions))
  where
    regions = subdivide region
    mapsAt r = Vector.filter (intersects r . heatmapRegion) heatmaps
    distFunc =
      foldl' (\f g x -> min (f x) (heatmapDistSq g x)) strengthDefault . mapsAt

flipArray :: R.Source r e => Array r DIM2 e -> Array D DIM2 e
flipArray xs = R.backpermute (Z :. h :. w) idx xs
  where
    (Z :. h :. w) = R.extent xs
    idx (Z :. y :. x) = Z :. (h - y - 1) :. x

pixels :: Region Double -> Vector (Euclidean -> Int) -> Int -> Int -> Array D DIM2 Word32
pixels (Region minLon minLat maxLon maxLat) imgBlocks w h =
  flipArray $ R.fromFunction (Z :. h :. w) pixel
  where
    polar lo hi s x = (hi - lo) / s * x + lo
    lon x = polar minLon maxLon (fromIntegral w) $ fromIntegral x
    lat y = polar minLat maxLat (fromIntegral h) $ fromIntegral y
    blockDim = 2 ^ imageDivisionFactor
    block x y =
      Vector.unsafeIndex
        imgBlocks
        (x * blockDim `div` w + y * blockDim `div` h * blockDim)
    pixel (Z :. y :. x) =
      Vector.unsafeIndex gradient . block x y . lonlatToEuclidean $
      V2 (lon x) (lat y)

heatmap :: Vector HeatPoint -> Heatmap
heatmap points = Heatmap region dist
  where
    e = getEuclidean . _heatPointLocation
    (p, c) = bimap (fmap e) e $ perimeter (-75) points
    region = euclideanBounds (Euclidean c) falloff
    falloff = Statistics.mean $ fmap (distance c) p
    falloffSq = falloff ^ (2 :: Int)
    dist (Euclidean x) =
      floor $ qd c x / falloffSq * fromIntegral (gradientResolution - 1)

heatmapImage :: Int -> Int -> Vector Heatmap -> (Region Double, Image PixelRGBA8)
heatmapImage w h xs = (region, img)
  where
    maxRegion = Region 180 90 (-180) (-90) -- Negative region covering the whole earth's latitude and longitude
    region = foldMapBy (\/) maxRegion heatmapRegion xs
    pix = R.AInterleave $ pixels region (blocks region xs) w h
    img = repaToImage . runIdentity . R.computeUnboxedP $ pix

renderImage :: Int -> Render (Vector (Vector HeatPoint)) (Image PixelRGBA8)
renderImage s =
  Render $
  bimap toMapRegion RenderedImage .
  heatmapImage s s . fmap heatmap 

repaToImage :: Array U DIM2 Word32 -> Image PixelRGBA8
repaToImage arr = generateImage f width height
  where
    Z :. height :. width = R.extent arr
    f x y = unpackPixel $ R.unsafeIndex arr (Z :. y :. x)

color :: Double -> PixelRGBA8
color db = PixelRGBA8 (toW r) (toW g) (toW b) (toW a)
  where
    constrainedDB = min maxSignalStrength . max minSignalStrength $ db
    normStr =
      1 +
      (maxSignalStrength - constrainedDB) /
      (minSignalStrength - maxSignalStrength) -- Normalized signal strength from 0 to 1
    hueAt
      | normStr > 0.05 = wrap ((-14) + ((normStr - 0.05) / 0.9) * (54 - (-14)))
      | otherwise = wrap ((-120) + (normStr / 0.05) * ((-14) - (-120)))
    hueStr
      | normStr < 0.05 = (normStr / 0.05) * 0.66
      | otherwise = normStr * 0.3
    toW x = floor $ x * 255
    (RGB r g b) = hsl hueAt 1 0.7
    a =
      0.6 *
      if normStr <= 0.05
        then sin (normStr / 0.05 * pi / 2)
        else 1
    wrap x
      | x < 0 = wrap $ x + 360
      | otherwise = x


