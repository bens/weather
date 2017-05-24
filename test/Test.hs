{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE RankNTypes            #-}

module Main where

import           Control.Comonad        (extract)
import           Control.Monad          (guard)
import qualified Data.Attoparsec.Text   as Atto
import           Data.List              (sort)
import           Data.Maybe             (fromMaybe)
import           Data.Monoid            ((<>))
import qualified Data.Text              as T
import qualified Data.Text.Lazy         as TL
import qualified Data.Text.Lazy.Builder as TLB
import qualified Data.Vector            as V
import           Test.Tasty
import qualified Test.Tasty.QuickCheck  as QC

import qualified Data.Foldl             as F
import qualified Data.Observation       as Obs

main :: IO ()
main = defaultMain (testGroup "all" suites)
  where
    suites =
      [ testGroup "Data.Observation.Observation"
          [ QC.testProperty "round-trip (single)" testObservation_RoundTrip1
          , QC.testProperty "round-trip (many)" testObservation_RoundTripN
          ]
      , testIncreasing
      , testMonoid
      , testCounted
      , testMean
      ]

testObservation_RoundTrip1 :: QC.Property
testObservation_RoundTrip1 =
  QC.forAll Obs.genStations $ \places ->
    QC.forAll (Obs.genObservation places) $ \x ->
      let printed = TL.toStrict . TLB.toLazyText $ Obs.observationText x
          parsed = Atto.parseOnly Obs.parseObservation printed
          expected = Right x :: Either String Obs.Observation
      in QC.counterexample ("printed:  " ++ show (T.unpack printed)) $
         QC.counterexample ("parsed:   " ++ show parsed) $
         QC.counterexample ("expected: " ++ show expected) $
         expected == parsed

testObservation_RoundTripN :: QC.NonNegative Int -> QC.Property
testObservation_RoundTripN (QC.NonNegative n) =
  QC.forAll Obs.genStations $ \places ->
    QC.forAll (Obs.genObservations places) $ \xs ->
      let printed =
            TL.toStrict
            $ TLB.toLazyText
            $ foldMap (\x -> Obs.observationText x <> "\n") xs
          parsed = Atto.parseOnly (Obs.parseObservations n) printed
          expected =
            Right (V.take n xs, V.empty :: V.Vector T.Text)
            :: (Either String (V.Vector Obs.Observation, V.Vector T.Text))
      in QC.counterexample ("printed:  " ++ show (T.unpack printed)) $
         QC.counterexample ("parsed:   " ++ show parsed) $
         QC.counterexample ("expected: " ++ show expected) $
         expected == parsed

runF :: F.Foldl a b -> [a] -> b
runF f = extract . F.feedN f

precision :: Int -> Double -> Double
precision e n = fromIntegral (round (n * (10^e)) :: Integer) / (10^e)

testIncreasing :: TestTree
testIncreasing =
  testGroup "Data.Foldl.increasing"
    [ QC.testProperty "permutes" tPermutes
    , QC.testProperty "rejects" tRejects
    , QC.testProperty "increasing" tIncreasing
    ]
  where
    tPermutes xs = sort (V.toList rejected ++ V.toList result) == sort xs
      where (rejected, result) = fst $ runF (F.increasing id Nothing) (xs :: [Int])

    tRejects xs = not (null rejected) QC.==> not (null result)
      where (rejected, result) = fst $ runF (F.increasing id Nothing) (xs :: [Int])

    tIncreasing xs = all (> 0) (V.zipWith subtract result (V.drop 1 result))
      where (_, result) = fst $ runF (F.increasing id Nothing) (xs :: [Int])

testMonoid :: TestTree
testMonoid =
  testGroup "Data.Foldl.monoid" [QC.testProperty "equivalent" tEquiv]
  where
    tEquiv xs = runF F.monoid (xs :: [[Int]]) == mconcat xs

testCounted :: TestTree
testCounted =
  testGroup "Data.Foldl.counted" [QC.testProperty "equivalent" tEquiv]
  where
    tEquiv xs = runF F.counted (xs :: [Int]) == length xs

testMean :: TestTree
testMean =
  testGroup "Data.Foldl.mean" [QC.testProperty "combining" tBounds]
  where
    tBounds xs ys =
      QC.counterexample ("expected: " ++ show expected) $
      QC.counterexample ("result:   " ++ show result) $
      fmap (precision 8) result == fmap (precision 8) expected
      where
        expected = runF F.mean (xs ++ ys :: [Double])
        (xsAvg, xsLen) = runF ((,) <$> F.mean <*> F.counted) xs
        (ysAvg, ysLen) = runF ((,) <$> F.mean <*> F.counted) ys
        xsScaled = fromMaybe 0 xsAvg * fromIntegral xsLen
        ysScaled = fromMaybe 0 ysAvg * fromIntegral ysLen
        xsysScaled = xsScaled + ysScaled
        result = (xsysScaled / fromIntegral (xsLen + ysLen)) <$ guard (xsLen + ysLen > 0)
