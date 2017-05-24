{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE RankNTypes                #-}
{-# LANGUAGE ScopedTypeVariables       #-}

module Data.Foldl
  ( Foldl
  , fold, foldable, foldMap, counted, mean, traveled, increasing
  , monoid, monoidNF
  , foldIso, foldPrism
  , feed, feedN
  , Min, _Min
  , Max, _Max
  , MonoidMap, _MonoidMap
  ) where

import           Control.Comonad
import           Control.DeepSeq
import           Control.Lens
import           Data.Foldable         (foldl')
import qualified Data.Map.Strict       as M
import           Data.Monoid           ((<>))
import qualified Data.Strict.Maybe     as Strict
import           Data.Strict.Tuple     (Pair ((:!:)))
import qualified Data.Strict.Tuple     as Strict
import qualified Data.Vector           as V
import           Prelude               hiding (foldMap)
import qualified VectorBuilder.Builder as VB
import qualified VectorBuilder.Vector  as VB

data Foldl a b = forall x. Foldl (x -> a -> x) !x (x -> b)

instance Functor (Foldl a) where
  fmap f (Foldl step st done) = Foldl step st (f . done)

instance Profunctor Foldl where
  dimap f g (Foldl step st done) =
    Foldl (\x a -> step x (f a)) st (g . done)

instance Applicative (Foldl a) where
  pure x = Foldl const () (const x)
  Foldl stepF stF doneF <*> Foldl stepX stX doneX =
    Foldl
      (\(f :!: x) a -> stepF f a :!: stepX x a)
      (stF :!: stX)
      (\(f :!: x) -> doneF f (doneX x))

instance Comonad (Foldl a) where
  extract (Foldl _ st done) = done st
  duplicate (Foldl step st done) =
    Foldl step st (\st' -> Foldl step st' done)

fold :: (b -> a -> b) -> b -> Foldl a b
fold f x = Foldl f x id

foldable :: Foldable f => Foldl a b -> Foldl (f a) b
foldable (Foldl step st done) = Foldl (foldl' step) st done

foldMap :: Monoid m => (a -> m) -> Foldl a m
foldMap f = lmap f monoid

monoid :: Monoid m => Foldl m m
monoid = fold mappend mempty

monoidNF :: (Monoid m, NFData m) => Foldl m m
monoidNF = fold (\x a -> let x' = x <> a in x' `deepseq` x') mempty

feed :: Foldl a b -> a -> Foldl a b
feed (Foldl step st done) x = Foldl step (step st x) done

feedN :: Foldable f => Foldl a b -> f a -> Foldl a b
feedN (Foldl step st done) xs = Foldl step (foldl' step st xs) done

foldIso :: Iso s s a a -> Foldl s s -> Foldl a a
foldIso p = dimap (review p) (view p)

foldPrism :: Prism s s a a -> Foldl s s -> Foldl a (Maybe a)
foldPrism p = dimap (review p) (preview p)

counted :: Foldl a Int
counted = fold (\n _ -> n + 1) 0

mean :: Fractional a => Foldl a (Maybe a)
mean = Strict.snd <$> fold f ((1 :: Int) :!: Nothing)
  where
    f (n :!: Nothing) x  = succ n :!: Just x
    f (n :!: Just avg) x =
      let avg' = avg + (x - avg)/fromIntegral n
      in avg' `seq` succ n :!: Just avg'

traveled :: Monoid b => (a -> a -> b) -> Foldl a b
traveled oper = Strict.fst <$> fold f (mempty :!: Nothing)
  where
    f (sofar :!: Nothing)   pos = sofar :!: Just pos
    f (sofar :!: Just prev) pos = (prev `oper` pos) <> sofar :!: Just pos

-- | Return the accepted values in the right part of the left pair, and the
-- rejected in the left.
increasing :: Ord b => (a -> b) -> Maybe b -> Foldl a ((V.Vector a, V.Vector a), Maybe b)
increasing key threshold =
  over (_1.both) VB.build <$> fold step ((VB.empty, VB.empty), threshold)
  where
    step ((bad, ok), Nothing) x = ((bad, ok <> VB.singleton x), Just (key x))
    step ((bad, ok),  Just b) x | key x > b = ((bad, ok <> VB.singleton x), Just (key x))
                                | otherwise = ((bad <> VB.singleton x, ok), Just b)

newtype Min a = Min{ getMin :: Strict.Maybe a } deriving (Eq, Ord, Show)

instance NFData a => NFData (Min a) where
  rnf (Min Strict.Nothing)  = ()
  rnf (Min (Strict.Just x)) = rnf x

instance Ord a => Monoid (Min a) where
  mempty = Min Strict.Nothing
  mappend (Min Strict.Nothing) y                      = y
  mappend x (Min Strict.Nothing)                      = x
  mappend (Min (Strict.Just x)) (Min (Strict.Just y)) = Min (Strict.Just (min x y))

_Min :: Prism' (Min a) a
_Min = prism' (Min . Strict.Just) (Strict.maybe Nothing Just . getMin)

newtype Max a = Max{ getMax :: Strict.Maybe a } deriving (Eq, Ord, Show)

instance NFData a => NFData (Max a) where
  rnf (Max Strict.Nothing)  = ()
  rnf (Max (Strict.Just x)) = rnf x

instance Ord a => Monoid (Max a) where
  mempty = Max Strict.Nothing
  mappend (Max Strict.Nothing) y                      = y
  mappend x (Max Strict.Nothing)                      = x
  mappend (Max (Strict.Just x)) (Max (Strict.Just y)) = Max (Strict.Just (max x y))

_Max :: Prism' (Max a) a
_Max = prism' (Max . Strict.Just) (Strict.maybe Nothing Just . getMax)

newtype MonoidMap k a = MonoidMap{ getMonoidMap :: M.Map k a }

instance (NFData k, NFData a) => NFData (MonoidMap k a) where
  rnf (MonoidMap m) = rnf m

instance (Ord k, Monoid a) => Monoid (MonoidMap k a) where
  mempty = MonoidMap M.empty
  mappend (MonoidMap a) (MonoidMap b) = MonoidMap (M.unionWith mappend a b)

_MonoidMap :: Iso' (MonoidMap k a) (M.Map k a)
_MonoidMap = iso getMonoidMap MonoidMap
