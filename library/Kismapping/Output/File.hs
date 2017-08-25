{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}
module Kismapping.Output.File
  ( outputFiles
  , OutputPaths(..)
  ) where

import Kismapping.Types
import qualified Codec.Picture as Picture
import qualified Data.Aeson as Aeson
import qualified Data.ByteString.Lazy as ByteString.Lazy

data OutputPaths = OutputPaths
  { outputPathCoords :: FilePath
  , outputPathImage :: FilePath
  , outputPathPolygons :: FilePath
  } deriving (Eq, Read, Show)

outputFiles :: forall a. OutputPaths -> (MapRegion, Rendered a) -> IO ()
outputFiles (OutputPaths coordFile imageFile polyFile) (region, rendered) = do
  ByteString.Lazy.writeFile coordFile $ Aeson.encode region
  case rendered of
    RenderedImage image -> Picture.writePng imageFile image
    RenderedPolygons polys ->
      ByteString.Lazy.writeFile polyFile $ Aeson.encode polys
