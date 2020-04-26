{-# LANGUAGE OverloadedStrings #-}

module Options
  ( InputType(..)
  , OutputType(..)
  , OutputMethod(..)
  , Options(..)
  , runOptions
  , mungeToPrintable
  ) where

import Data.Bits
import qualified Data.ByteString as Strict
import Data.Semigroup hiding (option)
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text
import Data.Vector (Vector)
import qualified Data.Vector as Vector
import qualified Filesystem.Path.CurrentOS as FS
import Options.Applicative

data InputType
  = InputTypeGpsXML
  | InputTypeSQLite
  deriving (Eq, Read, Show)

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
  , optInputFile :: Vector (InputType, FS.FilePath)
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
    optParser = Options <$> essid <*> inputFiles <*> outputType <*> outputMethod
    desc =
      "kismapping is a tool for generating and displaying visualizations of WiFi heatmap data overlayed on a map. Provide an ESSID, and a gpsxml file, and kismapping will generate a heatmap from all BSSIDs associated with the specified ESSID. By default, kismapping will generate a 2048x2048 overlay image, and serve it on port 8080."

-- Kismet doesn't make much attempt to deal with text encoding, so it
-- munges non-ascii characters to octal, with the format \xxx. We have to
-- apply the same transformation to our own ESSIDs to be able to map
-- essids with emojis and such
-- See https://github.com/kismetwireless/kismet/blob/fe048e266e9713425bc50af1057b9068a4ce28d4/util.cc#L75 for the original algorithm
--
-- The way we implement this is to encode the text to UTF-8 as a ByteString,
-- perform the munging, and then decode the result
mungeToPrintable :: Text -> Text
mungeToPrintable text =
  let munge x
        | x >= 32 && x <= 126 = Strict.singleton x
        | otherwise =
          Strict.pack
            [ fromIntegral (fromEnum '\\')
            , ((x `shiftR` 6) .&. 0x03) + fromIntegral (fromEnum '0')
            , ((x `shiftR` 3) .&. 0x07) + fromIntegral (fromEnum '0')
            , ((x `shiftR` 0) .&. 0x07) + fromIntegral (fromEnum '0')
            ]
      mungedText = Strict.concatMap munge (Text.encodeUtf8 text)
   in Text.decodeUtf8 mungedText


essid :: Parser (Vector Text)
essid =
  fmap
    Vector.fromList
    (some
       (option
          ((mungeToPrintable . Text.pack) <$> str)
          (short 'e' <> long "essid" <>
           help
             "Specify ESSID to use for input data. Use multiple times to include multiple ESSIDs in the same map." <>
           metavar "ESSID")))

inputFiles :: Parser (Vector (InputType, FS.FilePath))
inputFiles =
  let singleFile = do
        fname <- str
        let path = FS.fromText (Text.pack fname)
        itype <-
          case FS.extension path of
            Just "kismet" -> pure InputTypeSQLite
            Just "gpsxml" -> pure InputTypeGpsXML
            _ -> empty
        pure (itype, path)
      optInfo =
        short 'i' <>
        long "input" <>
        help
          "Set input file. Use multiple times to include multiple input files in the same map. Provide either .gpsxml or .kismet files" <>
        metavar "FILE"
   in fmap Vector.fromList (some (option singleFile optInfo))

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

