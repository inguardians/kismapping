{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE BangPatterns #-}
module Kismapping.Output.WebApp
  ( outputWebApp
  ) where

import Codec.Picture
import Data.Aeson (ToJSON, (.=), object, pairs, toEncoding, toJSON)
import qualified Data.ByteString.Lazy as Lazy
import qualified Data.ByteString as Strict
import qualified Data.ByteString.Search as ByteString.Search
import Data.FileEmbed
import qualified Data.Text as Text
import qualified Data.Vector as Vector
import Kismapping.Types
import Web.Spock


newtype HeatmapConfig =
  HeatmapConfig Text.Text

instance ToJSON HeatmapConfig where
  toEncoding (HeatmapConfig t) = pairs ("type" .= t)
  toJSON (HeatmapConfig t) = object ["type" .= t]

type Server = SpockM () () () ()

outputWebApp :: forall a. Strict.ByteString -> (MapRegion, Rendered a) -> Server
outputWebApp apiKey (region, RenderedImage img) = appOverlayImage apiKey region img
outputWebApp apiKey (region, RenderedPolygons polys) = appOverlayPolygons apiKey region polys

appCommon :: Strict.ByteString -> MapRegion -> Text.Text -> Server
appCommon apiKey region mapType =
  index apiKey *> coords region *> heatmapConfig (HeatmapConfig mapType)

googleMapsFile :: Strict.ByteString -> Strict.ByteString
googleMapsFile apiKey =
  (Lazy.toStrict . ByteString.Search.replace "${{KEY}}" apiKey)
    $(embedFile "library/Kismapping/Output/map.html")

index :: Strict.ByteString -> Server
index apiKey =
  let indexFile = googleMapsFile apiKey
  in get root $ do
       setHeader "Content-Type" "text/html; charset=utf-8"
       bytes indexFile

coords :: MapRegion -> Server
coords = get "coords" . json

heatmapConfig :: HeatmapConfig -> Server
heatmapConfig = get "config" . json

appOverlayImage :: Strict.ByteString -> MapRegion -> Image PixelRGBA8 -> Server
appOverlayImage apiKey region heatmap =
  seq pngBytes $ do
    appCommon apiKey region "overlayImage"
    get "overlay.png" $ do
      setHeader "Content-Type" "image/png"
      bytes pngBytes
  where
    pngBytes = Lazy.toStrict $ encodePng heatmap

appOverlayPolygons ::
     Strict.ByteString -> MapRegion -> Vector.Vector Polygon -> Server
appOverlayPolygons apiKey region polygons =
  seq polygons $ do
    appCommon apiKey region "overlayPolygon"
    get "overlay.json" $ json polygons
