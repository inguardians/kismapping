{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Kismapping.Input.KismetXML
  ( readGpsXml
  ) where
import Conduit
import Control.Monad.Catch
import Control.Monad.Except
import Control.Monad.Trans.Either
import Data.Foldable
import Data.Functor
import Data.HashMap.Lazy (HashMap)
import qualified Data.HashMap.Lazy as HashMap
import Data.Hashable
import Data.Maybe
import Data.Monoid
import Data.Sequence (Seq)
import qualified Data.Sequence as Seq
import Data.Text (Text)
import Data.Text.Read
import Data.Typeable
import Data.Vector (Vector)
import qualified Data.Vector as Vector
import Data.XML.Types
import qualified Filesystem.Path.CurrentOS as Path
import Kismapping.Types
import Linear
import Text.XML.Stream.Parse
import Kismapping.Input.KismetPoints

{-import qualified Data.Attoparsec.ByteString as Atto-}

-- TODO unsafe parser

type GpsTag = (Alt Maybe Text, (Text -> Text) -> Endo EssidMap)

gpsNetworkFilenameTag :: Text -> GpsTag
gpsNetworkFilenameTag x = (Alt $ Just x, mempty)

gpsPointTag :: ((Text -> Text) -> EssidMap -> EssidMap) -> GpsTag
gpsPointTag x = (mempty, Endo . x)

type SsidTag = (Alt Maybe Text, Alt Maybe Text)

bssidTag :: Text -> SsidTag
bssidTag x = (Alt $ Just x, mempty)

essidTag :: Text -> SsidTag
essidTag x = (mempty, Alt $ Just x)

type Parser a = forall o m. MonadThrow m => ConduitM Event o m (Maybe a)

data GpsParseException =
  GpsParseException String
  deriving (Show, Typeable)

instance Exception GpsParseException

-- | Parse the `network-file` tag, which contains the name of the file which
-- provides ESSID information.
parseNetworkFilenameTag :: Parser GpsTag
parseNetworkFilenameTag =
  tagIgnoreAttrs "network-file" $ gpsNetworkFilenameTag <$> content

-- | Parse a datapoint, containing signal strength for a BSSID at a specific
-- position.
parseGpsPointTag :: Parser GpsTag 
parseGpsPointTag =
  flip (tagName "gps-point") pure $
  eitherT (throwM . GpsParseException) pure $ do
    bssid <- lift $ requireAttr "bssid"
    case bssid of
      "GP:SD:TR:AC:KL:OG" -> lift ignoreAttrs $> gpsPointTag (const id) -- Not a real datapoint, ignore it
      _ -> do
        lon <- requireAttrRead "lon" $ signed double
        lat <- requireAttrRead "lat" $ signed double
        alt <- pure 0 -- TODO We can't use the data's altitude yet, because we need to adjust our offset when rendering, or nothing shows up. However, there's no code to do that yet, so for now we just use an altitude of 0.
        dbm <- requireAttrRead "signal_dbm" $ signed decimal
        let loc = Polar $ V3 lon lat alt
        lift ignoreAttrs
        pure . gpsPointTag $ \toEssid ->
          insertGpsPoint (toEssid bssid) bssid loc (fromIntegral (dbm :: Int))
  where
    requireAttrRead a f = lift (requireAttr a) >>= hoistEither . fmap fst . f

parseGpsRunFile :: Parser (Text, (Text -> Text) -> EssidMap -> EssidMap)
parseGpsRunFile =
  tagIgnoreAttrs "gps-run" $ do
    runData <- fold <$> many' (parseGpsPointTag `orE` parseNetworkFilenameTag)
    case runData of
      (Alt Nothing, _) -> throwM $ GpsParseException "No network-file tag found"
      (Alt (Just netFileName), points) -> pure (netFileName, appEndo . points)

-- (ResourceT (ExceptT Text IO))
parseNetworkFile :: Parser (HashMap Text Text)
parseNetworkFile =
  tagIgnoreAttrs "detection-run" $ fold <$> many' wirelessNetwork
  where
    pBssidTag = tagIgnoreAttrs "BSSID" $ bssidTag <$> content
    pEssidTag = tagIgnoreAttrs "essid" $ essidTag <$> content
    pSsidTag = tagIgnoreAttrs "SSID" $ fold <$> many' pEssidTag
    wirelessNetwork =
      tagIgnoreAttrs "wireless-network" $ do
        (Alt bssid, Alt essid) <- fold <$> many' (pBssidTag `orE` pSsidTag)
        pure . fromMaybe HashMap.empty $ HashMap.singleton <$> bssid <*> essid

-- Parse an XML file with a specific XML parser inside the ExceptT monad
parseFileWith ::
     (MonadBaseControl IO m, MonadThrow m, MonadIO m, MonadError Text m)
  => Sink Event (ResourceT m) (Maybe b)
  -> Path.FilePath
  -> m b
parseFileWith p f = do
  result <- runConduitRes $ parseFile def (Path.encodeString f) .| p
  case result of
    Nothing -> throwError $ "Error parsing file " <> either id id (Path.toText f)
    Just x -> pure x

-- parseGpsRunFile :: Parser (Text, (Text -> Text) -> EssidMap -> EssidMap)

readSingleGpsXml ::
     (MonadBaseControl IO m, MonadThrow m, MonadIO m, MonadError Text m)
  => Path.FilePath
  -> m ((Text -> Text) -> EssidMap -> EssidMap, HashMap Text Text)
readSingleGpsXml file = do
  let gpsParentDir = Path.directory file
  (networkFile, points) <- parseFileWith parseGpsRunFile file
  ssids <-
    parseFileWith parseNetworkFile $ gpsParentDir <> Path.fromText networkFile
  pure (points, ssids)


readGpsXml ::
     (MonadBaseControl IO m, MonadThrow m, MonadIO m, MonadError Text m)
  => (Vector Path.FilePath)
  -> (Vector Text)
  -> m (Vector (Vector HeatPoint))
readGpsXml files essids = do
  (pointsVec, ssidsVec) <- fmap Vector.unzip (traverse readSingleGpsXml files)
  let points = foldr (\l r f x -> l f (r f x)) (const id) pointsVec
      ssids = fold ssidsVec
  pure
    (Vector.concatMap
       (\essid ->
          maybe
            Vector.empty
            toHeatpoints
            (HashMap.lookup essid (points (lookupEssid ssids) HashMap.empty)))
       essids)
