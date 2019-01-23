{-# LANGUAGE OverloadedStrings #-}

module Options
  ( OutputType(..)
  , OutputMethod(..)
  , Options(..)
  , runOptions
  ) where

import Data.Semigroup hiding (option)
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text
import Data.Vector (Vector)
import qualified Data.Vector as Vector
import qualified Filesystem.Path.CurrentOS as FS
import Options.Applicative
import qualified Data.ByteString as Strict

data OutputType
  = OutputTypeImage Int
  | OutputTypePolygons
  deriving (Eq, Read, Show)

data OutputMethod
  = OutputMethodFile FilePath
  | OutputMethodWebServer Strict.ByteString Int
  deriving (Eq, Read, Show)

data Options = Options
  { optEssid :: Vector Text
  , optGpsXmlFile :: Vector FS.FilePath
  , optOutputType :: OutputType
  , optOutputMethod :: OutputMethod
  } deriving (Eq, Show)

runOptions :: IO Options
runOptions = customExecParser (prefs showHelpOnEmpty) options

options :: ParserInfo Options
options =
  info
    (helper <*> optParser)
    (fullDesc <> progDesc desc <> header "kismapping - WiFi Heatmap Generator")
  where
    optParser = Options <$> essid <*> gpsXmlFile <*> outputType <*> outputMethod
    desc =
      "kismapping is a tool for generating and displaying visualizations of WiFi heatmap data overlayed on a map. Provide an ESSID, and a gpsxml file, and kismapping will generate a heatmap from all BSSIDs associated with the specified ESSID. By default, kismapping will generate a 2048x2048 overlay image, and serve it on port 8080."

essid :: Parser (Vector Text)
essid =
  fmap
    Vector.fromList
    (some
       (option
          (Text.pack <$> str)
          (short 'e' <> long "essid" <>
           help
             "Specify ESSID to use for input data. Use multiple times to include multiple ESSIDs in the same map." <>
           metavar "ESSID")))

gpsXmlFile :: Parser (Vector FS.FilePath)
gpsXmlFile =
  fmap
    Vector.fromList
    (some
      (option
        (FS.fromText . Text.pack <$> str)
        (short 'i' <> long "input" <> help "Set input GPS XML file. Use multiple times to include multiple input files in the same map." <> metavar "FILE")))


outputTypeImage :: Parser OutputType
outputTypeImage =
  option
    (OutputTypeImage <$> auto)
    (short 'r' <> long "resolution" <>
     help
       "Output an image of the set resolution. When used with -o, this will be saved to output.png." <>
     metavar "RES" <>
     value (OutputTypeImage 2048) <>
     showDefaultWith (\(OutputTypeImage x) -> show x))

outputTypePolygons :: Parser OutputType
outputTypePolygons =
  flag'
    OutputTypePolygons
    (long "polygons" <>
     help
       "Output polygons, one per BSSID. When used with -o, these will be saved to output.json")

outputType :: Parser OutputType
outputType = outputTypeImage <|> outputTypePolygons

outputMethodFile :: Parser OutputMethod
outputMethodFile =
  option
    (OutputMethodFile <$> str)
    (short 'o' <> long "output" <> help "Set output directory for file output." <>
     metavar "DIRECTORY")

outputMethodWebServer :: Parser OutputMethod
outputMethodWebServer =
  OutputMethodWebServer <$> outputMethodWebServerKey <*>
  outputMethodWebServerPort

outputMethodWebServerKey :: Parser Strict.ByteString
outputMethodWebServerKey =
  option
    (Text.encodeUtf8 . Text.pack <$> str)
    (short 'k' <> long "apikey" <>
     help "Launch a web server using the specified Google Maps API Key for the output web page." <>
     metavar "APIKEY")

outputMethodWebServerPort :: Parser Int
outputMethodWebServerPort =
  option
    auto
    (short 'p' <> long "port" <>
     help "Specify the port of the web server." <>
     metavar "PORT" <>
     value 8080 <>
     showDefault)

outputMethod :: Parser OutputMethod
outputMethod = outputMethodWebServer <|> outputMethodFile

