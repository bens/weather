{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import           Control.Applicative       ((<|>))
import           Control.Comonad           (extract)
import           Control.Lens              hiding (folding)
import           Control.Monad             (unless)
import           Control.Monad.IO.Class
import           Control.Monad.Trans.State
import qualified Data.Attoparsec.Text      as Atto
import           Data.Machine              ((~>))
import qualified Data.Machine              as Mach
import qualified Data.Map.Strict           as M
import           Data.Monoid               (Sum (..), (<>))
import qualified Data.Text                 as T
import qualified Data.Text.IO              as T
import qualified Data.Text.Lazy.Builder    as TL
import qualified Data.Text.Lazy.IO         as TL
import qualified Data.Time.Clock           as Time
import qualified Data.Vector               as V
import           System.Environment        (getArgs, getProgName)
import qualified System.IO                 as IO
import qualified Test.QuickCheck           as QC

import           Data.Foldl                (Foldl)
import qualified Data.Foldl                as F
import qualified Data.Observation          as Obs

main :: IO ()
main = do
  progName <- getProgName
  args <- getArgs
  case args of
    ["gen", path] -> generateData path 10000 50
    ["proc", path] -> processData path
    _ -> putStrLn ("USAGE: " ++ progName ++ " " ++ "(gen | proc) PATH")

-- GENERATING TEST DATA

generateData :: FilePath -> Int -> Int -> IO ()
generateData path nlines nblocks =
  IO.withFile path IO.WriteMode $ \h ->
    Mach.runT_ $ generating nlines ~> dumping h ~> Mach.taking nblocks

generating :: MonadIO m => Int -> Mach.ProcessT m a (V.Vector Obs.Observation)
generating sz = Mach.construct $ do
  t0 <- liftIO (QC.generate Obs.genDateTime)
  stations <- liftIO (QC.generate Obs.genStations)
  go t0 stations
  where
    go t stations = do
      v <- liftIO (f t stations)
      Mach.yield v
      let lastV = V.last v
      go (lastV ^. Obs.obsTime) stations
    f t stations = QC.generate (QC.resize 1000 $ Obs.genObservations' sz stations t)

dumping :: MonadIO m => IO.Handle -> Mach.ProcessT m (V.Vector Obs.Observation) ()
dumping h = Mach.autoM $
  liftIO . TL.hPutStr h . TL.toLazyText . foldMap (\x -> Obs.observationText x <> "\n")

-- PARSING/PROCESSING

data Measures
  = Measures
  { _minTemp           :: F.Min Obs.Temp
  , _maxTemp           :: F.Max Obs.Temp
  , _meanTemp          :: Maybe Obs.Temp
  , _observationCounts :: M.Map Obs.Station (Sum Int)
  , _distanceTraveled  :: Obs.Distance
  } deriving Show

processData :: FilePath -> IO ()
processData path =
  IO.withFile path IO.ReadMode $ \h ->
    Mach.runT_
      $ parsing h 10000
      ~> Mach.autoM (\x -> x <$ print (length x))
      ~> increasing (view Obs.obsTime)
      ~> Mach.autoM (\x -> x <$ print (length $ snd x))
      ~> Mach.mapping snd
      ~> folding (F.foldable (temps <*> countObs <*> traveled))
      ~> Mach.autoM print
  where
    temps = lmap (view Obs.obsTemp) (Measures <$> minTemp <*> maxTemp <*> meanTemp)

parsing :: MonadIO m => IO.Handle -> Int -> Mach.ProcessT m a (V.Vector Obs.Observation)
parsing h sz =
  Mach.construct (liftIO (T.hGetChunk h) >>= go . Atto.parse (Obs.parseObservations sz))
  where
    go res = case res of
      Atto.Fail _ ctxs err    -> error (show (err, ctxs))
      Atto.Partial f          -> liftIO (f <$> T.hGetChunk h) >>= go
      Atto.Done i (xs, _junk) -> do
        unless (null xs) $ Mach.yield xs
        if T.null i
          then do
            i' <- liftIO (T.hGetChunk h)
            unless (T.null i') $ go (Atto.parse (Obs.parseObservations sz) i')
          else go (Atto.parse (Obs.parseObservations sz) i)

increasing :: (Ord b, Monad m) => (a -> b) -> Mach.ProcessT m (V.Vector a) (V.Vector a, V.Vector a)
increasing key = Mach.fitM (`evalStateT` Nothing) (Mach.autoM f)
  where
    f xs = state (extract . flip F.feedN xs . F.increasing key)

minTemp :: Foldl Obs.Temp (F.Min Obs.Temp)
minTemp = view (re F._Min) `lmap` F.monoid

maxTemp :: Foldl Obs.Temp (F.Max Obs.Temp)
maxTemp = view (re F._Max) `lmap` F.monoid

meanTemp :: Foldl Obs.Temp (Maybe Obs.Temp)
meanTemp = dimap (view Obs.celsius) (fmap (review Obs.celsius)) F.mean

countObs :: Foldl Obs.Observation (M.Map Obs.Station (Sum Int))
countObs =
  lmap
    (flip M.singleton (Sum 1) . view Obs.obsStation)
    (F.foldIso F._MonoidMap F.monoidNF)

traveled :: Foldl Obs.Observation Obs.Distance
traveled = lmap (view Obs.obsPos) (F.traveled Obs.distance)

folding :: Monad m => Foldl a b -> Mach.ProcessT m a b
folding = Mach.construct . go
  where
    go fld = do
      x <- Mach.await <|> (Mach.yield (extract fld) *> Mach.stop)
      let fld' = F.feed fld x in fld' `seq` go fld'

-- MISCELLANEOUS

timing :: MonadIO m => String -> Mach.ProcessT m a a
timing label = go
  where
    go = Mach.repeatedly $ do
      t <- liftIO Time.getCurrentTime
      x <- Mach.await
      t' <- liftIO Time.getCurrentTime
      liftIO . putStrLn $ label ++ ": " ++ show (Time.diffUTCTime t' t)
      Mach.yield x
