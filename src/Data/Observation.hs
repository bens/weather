{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RankNTypes                 #-}

module Data.Observation
  ( Temp, celsius, fahrenheit, kelvin
  , Distance, kilometers, miles, meters
  , Position, _Position, distance
  , Station, _Station
  , Observation, obsTime, obsPos, obsTemp, obsStation
  , observationText
  , genObservation, genObservations, genObservations', genDateTime, genStations
  , parseObservation, parseObservations
  ) where

import           Control.Applicative              ((<|>))
import           Control.DeepSeq                  (NFData (..))
import           Control.Lens
import           Control.Monad.Trans.Class
import           Control.Monad.Trans.State.Strict
import qualified Data.Attoparsec.Text             as Atto
import           Data.List                        (unfoldr)
import           Data.Monoid                      ((<>))
import qualified Data.Text                        as T
import qualified Data.Text.Lazy.Builder           as TL
import qualified Data.Text.Lazy.Builder.Int       as TL
import qualified Data.Time.Calendar               as Time
import qualified Data.Time.Clock                  as Time
import qualified Data.Time.LocalTime              as Time
import qualified Data.Vector                      as V
import qualified Test.QuickCheck                  as QC
import qualified VectorBuilder.Builder            as VB
import qualified VectorBuilder.Vector             as VB

newtype Temp = Temp Double
  deriving (Eq, Ord, Show, NFData)

newtype Distance = Distance Double
  deriving (Eq, Ord, Show, NFData)

instance Monoid Distance where
  mempty = Distance 0
  mappend (Distance x) (Distance y) = Distance (x + y)

data Position = Position !Distance !Distance
  deriving (Eq, Ord, Show)

instance NFData Position where
  rnf (Position _ _) = ()

newtype Station = Station T.Text
  deriving (Eq, Ord, Show, NFData)

data Observation = Obs !Time.UTCTime !Position !Temp !Station deriving (Eq, Show)

instance NFData Observation where
  rnf (Obs t _ _ _) = rnf t

obsTime :: Lens' Observation Time.UTCTime
obsTime = lens (\(Obs t _ _ _) -> t) (\(Obs _ p x o) t -> Obs t p x o)

obsPos :: Lens' Observation Position
obsPos = lens (\(Obs _ p _ _) -> p) (\(Obs t _ x o) p -> Obs t p x o)

obsTemp :: Lens' Observation Temp
obsTemp = lens (\(Obs _ _ x _) -> x) (\(Obs t p _ o) x -> Obs t p x o)

obsStation :: Lens' Observation Station
obsStation = lens (\(Obs _ _ _ o) -> o) (\(Obs t p x _) o -> Obs t p x o)

celsius :: Iso' Temp Double
celsius = iso (\(Temp x) -> x) Temp

fahrenheit :: Iso' Temp Double
fahrenheit = iso g f
  where
    f x = Temp (((x - 32) * 10) / 18)
    g (Temp x) = (x * 1.8) + 32

kelvin :: Iso' Temp Double
kelvin = iso g f
  where
    f x = Temp (x - 273.15)
    g (Temp x) = x + 273.15

kilometers :: Iso' Distance Int
kilometers = iso g f
  where
    f x = Distance (fromIntegral x * 1000)
    g (Distance x) = round (x/1000)

miles :: Iso' Distance Int
miles = iso g f
  where
    f x = Distance (fromIntegral x * 1609.3470878864446)
    g (Distance x) = round (x / 1609.3470878864446)

meters :: Iso' Distance Int
meters = iso g f
  where
    f x = Distance (fromIntegral x)
    g (Distance x) = round x

_Position :: Iso' Position (Distance, Distance)
_Position = iso f (uncurry Position)
  where
    f (Position x y) = (x, y)

_PositionX :: Lens' Position Distance
_PositionX = lens (\(Position x _) -> x) (\(Position _ y) x -> Position x y)

_PositionY :: Lens' Position Distance
_PositionY = lens (\(Position _ y) -> y) (\(Position x _) y -> Position x y)

distance :: Position -> Position -> Distance
distance (Position (Distance x1) (Distance y1)) (Position (Distance x2) (Distance y2)) =
  Distance (sqrt (xdelta*xdelta + ydelta*ydelta))
  where
    xdelta = x2 - x1
    ydelta = y2 - y1

_Station :: Iso' Station T.Text
_Station = iso (\(Station x) -> x) Station

magnitude :: (Ord a, Integral a) => a -> Int
magnitude n =
  1 `max` sum (unfoldr (\x -> if x < 1 then Nothing else Just (1::Int, x `div` 10)) n)

observationText :: Observation -> TL.Builder
observationText (Obs (Time.UTCTime date time) pos temp station) =
  mconcat [sT, "|", sPos, "|", sTemp, "|", sStation]
  where
    (year, month, day) = Time.toGregorian date
    Time.TimeOfDay hour minute _ = Time.timeToTimeOfDay time
    fmt w n =
      let nzeros = fromIntegral (w - magnitude n)
          zeros = T.replicate nzeros "0"
      in TL.fromText zeros <> TL.decimal n
    sT = mconcat [fmt 4 year, "-", fmt 2 month, "-", fmt 2 day, "T", fmt 2 hour, ":", fmt 2 minute]
    fetch units = over both (view units) (view _Position pos)
    sPos = TL.decimal x <> "," <> TL.decimal y
      where (x,y) = fetch (_StationDistance station)
    sTemp = TL.decimal (round (temp ^. _StationTemp station) :: Int)
    sStation = TL.fromText (station ^. _Station)

genDateTime :: QC.Gen Time.UTCTime
genDateTime = do
  let upto x = QC.choose (0, x)
  day  <- Time.fromGregorian <$> upto 9999 <*> upto 11 <*> upto 31
  time <- Time.timeOfDayToTime <$> (Time.TimeOfDay <$> upto 23 <*> upto 59 <*> pure 0)
  return (Time.UTCTime day time)

genNominalDiffTime :: QC.Gen Time.NominalDiffTime
genNominalDiffTime = (fromIntegral . (*60)) <$> QC.frequency [(1, neg), (50, pos)]
  where
    neg = QC.choose (-100, 0)
    pos = QC.choose (1, 5000 :: Int)

genStations :: QC.Gen [Station]
genStations = do
  let standardStations =
        ["AU", "US", "FR"]
  let otherStation = do
        n <- QC.choose (2,20)
        T.pack <$> QC.vectorOf n (QC.choose ('A', 'z'))
  n <- QC.choose (0,5)
  (map (review _Station) . (standardStations ++)) <$> QC.vectorOf n otherStation

-- | Generate a single observation given a list of observatory stations.
genObservation :: [Station] -> QC.Gen Observation
genObservation stations = do
  t <- genDateTime
  (xPos, yPos) <- QC.arbitrary
  station <- QC.elements stations
  let pos = _Position # ( _StationDistance station # xPos
                        , _StationDistance station # yPos )
  temp <- fromIntegral <$> (QC.arbitrary :: QC.Gen Int)
  return $ Obs t pos (_StationTemp station # temp) station

genObservations :: [Station] -> QC.Gen (V.Vector Observation)
genObservations stations = do
  baseTime <- genDateTime
  QC.NonNegative sz <- QC.arbitrary
  genObservations' sz stations baseTime

-- | Generate a series of observations by accumulating small differences from
-- the current state (time, position, temperature).
genObservations' :: Int -> [Station] -> Time.UTCTime -> QC.Gen (V.Vector Observation)
genObservations' sz stations baseTime =
  flip evalStateT baseTime . V.replicateM sz $ do
    t <- get
    x <- lift $ genObservation stations
    dt <- lift genNominalDiffTime
    let t' = Time.addUTCTime dt t
    (x & obsTime .~ t') <$ put t'

_StationTemp :: Station -> Iso' Temp Double
_StationTemp station = case station ^. _Station of
  "AU" -> celsius
  "US" -> fahrenheit
  _    -> kelvin

_StationDistance :: Station -> Iso' Distance Int
_StationDistance station = case station ^. _Station of
  "US" -> miles
  "FR" -> meters
  _    -> kilometers

parseUTCTime :: Atto.Parser Time.UTCTime
parseUTCTime = do
  let num c = Atto.decimal <* Atto.char c
  day <- Time.fromGregorian <$> num '-' <*> num '-' <*> num 'T'
  time <- Time.timeOfDayToTime <$> (Time.TimeOfDay <$> num ':' <*> num '|' <*> pure 0)
  return (Time.UTCTime day time)

parseObservations :: Int -> Atto.Parser (V.Vector Observation, V.Vector T.Text)
parseObservations = go mempty mempty
  where
    go xs errs n = case n of
      0 -> return (VB.build xs, VB.build errs)
      _ -> goodParse xs errs n <|> junkLine xs errs n <|> lastLine xs errs n
    goodParse xs errs n = do
      obs <- parseObservation
      go (xs <> VB.singleton obs) errs (n-1)
    junkLine xs errs n = do
      ln <- Atto.takeTill (== '\n') <* Atto.endOfLine
      let errs' = if T.null ln then errs else errs <> VB.singleton ln
      go xs errs' (n-1)
    lastLine xs errs _ = do
      ln <- Atto.takeText
      let errs' = if T.null ln then errs else errs <> VB.singleton ln
      return (VB.build xs, VB.build errs')

parseObservation :: Atto.Parser Observation
parseObservation = do
  let num c = Atto.decimal <* Atto.char c
  utctime <- parseUTCTime
  xpos <- fromIntegral <$> (Atto.signed (num ',') :: Atto.Parser Int)
  ypos <- fromIntegral <$> (Atto.signed (num '|') :: Atto.Parser Int)
  tempRaw <- fromIntegral <$> (Atto.signed (num '|') :: Atto.Parser Int)
  station <- review _Station <$> Atto.takeTill (== '\n')
  let pos = _Position # ( _StationDistance station # xpos
                        , _StationDistance station # ypos )
  let temp = _StationTemp station # tempRaw
  Obs utctime pos temp station <$ Atto.option () Atto.endOfLine
