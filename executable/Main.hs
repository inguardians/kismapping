{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE GADTs #-}
{-# OPTIONS_GHC -funbox-strict-fields #-}
module Main
  ( main
  ) where

import Control.Monad.Except
import Data.Semigroup
import Data.Text (Text)
import qualified Data.Text as Text
import Data.Vector (Vector)
import Kismapping
import Options
import System.Exit (die)
import qualified Web.Spock as Spock
import qualified Web.Spock.Config as Spock
import qualified Data.ByteString as Strict


writeRendered :: forall a. FilePath -> (MapRegion, Rendered a) -> IO ()
writeRendered dir =
  outputFiles
    OutputPaths
    { outputPathCoords = prefix "coords.json"
    , outputPathImage = prefix "overlay.png"
    , outputPathPolygons = prefix "overlay.json"
    }
  where
    prefix x = dir <> "/" <> x


-- | Serve a google maps webpage with an image overlayed over a region.
-- As heatmap generation can take awhile, the render is forced before the
-- webserver is started.
runWebServer ::
     forall a. Strict.ByteString -> Int -> (MapRegion, Rendered a) -> IO ()
runWebServer apiKey port render =
  outputWebApp apiKey render `seq` do
    putStrLn $ "Running webserver on 0.0.0.0:" <> show port
    putStrLn $
      "Navigate to http://127.0.0.1:" <> show port <> "/ to access map."
    spockCfg <- Spock.defaultSpockCfg () Spock.PCNoDatabase ()
    Spock.runSpockNoBanner port . Spock.spock spockCfg $
      outputWebApp apiKey render

withRendered ::
     OutputType
  -> Vector (Vector HeatPoint)
  -> (forall a. (MapRegion, Rendered a) -> r)
  -> r
withRendered outputType points f =
  case outputType of
    OutputTypeImage res -> f $ runRender (renderImage res) points
    OutputTypePolygons -> f $ runRender renderPolygons points

readPoints :: Options -> ExceptT Text IO (Vector (Vector HeatPoint))
readPoints opts =
  readGpsXml (optGpsXmlFile opts) (optEssid opts) >>= \bssids ->
    if null bssids
      then throwError
             ("No BSSIDs found with given ESSIDs: " <>
              Text.pack (show (optEssid opts)))
      else pure (removeInvalidData bssids)
  
processMap :: Options -> IO ()
processMap opts = do
  putStrLn "Parsing GPS Points"
  points <-
    runExceptT (readPoints opts) >>=
    either (die . Text.unpack . Text.append "ERROR: ") pure
  withRendered (optOutputType opts) points $
    case optOutputMethod opts of
      OutputMethodFile dir -> writeRendered dir
      OutputMethodWebServer apiKey port -> runWebServer apiKey port

main :: IO ()
main = runOptions >>= processMap
