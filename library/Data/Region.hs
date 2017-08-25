{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}
module Data.Region
  ( Region(..)
  , area
  , bounds
  , bounds'
  , contains
  , containsPoint
  , fromPoint
  , intersection
  , intersects
  , negate
  , polarity
  , regionMaxX
  , regionMaxY
  , regionMinX
  , regionMinY
  , union
  ) where

import Algebra.Lattice
import Data.Foldable
import Data.Traversable
import Prelude hiding (negate)

-- Todo rethink the naming of 'min' and 'max' (maybe left and right?)
data Region a = Region
  { _regionMinX :: !a
  , _regionMinY :: !a
  , _regionMaxX :: !a
  , _regionMaxY :: !a
  } deriving (Eq, Read, Show)

-- Hand-written lens instances to avoid template-haskell dependency
type Lens' s a = forall f. Functor f => (a -> f a) -> s -> f s

regionMinX :: Lens' (Region a) a
regionMinX f (Region lx ly hx hy) = (\lx' -> Region lx' ly hx hy) <$> f lx

regionMaxX :: Lens' (Region a) a
regionMaxX f (Region lx ly hx hy) = (\hx' -> Region lx ly hx' hy) <$> f hx

regionMinY :: Lens' (Region a) a
regionMinY f (Region lx ly hx hy) = (\ly' -> Region lx ly' hx hy) <$> f ly

regionMaxY :: Lens' (Region a) a
regionMaxY f (Region lx ly hx hy) = (\hy' -> Region lx ly hx hy') <$> f hy

instance Functor Region where
  fmap = fmapDefault

instance Foldable Region where
  foldMap = foldMapDefault

instance Traversable Region where
  traverse f (Region lx ly hx hy) = Region <$> f lx <*> f ly <*> f hx <*> f hy

instance Ord a => JoinSemiLattice (Region a) where
  (\/) = union

instance Ord a => MeetSemiLattice (Region a) where
  (/\) = intersection

instance Ord a => Lattice (Region a) where

instance (Bounded a, Ord a) => BoundedJoinSemiLattice (Region a) where
  bottom = Region maxBound maxBound minBound minBound

instance (Bounded a, Ord a) => BoundedMeetSemiLattice (Region a) where
  top = Region minBound minBound maxBound maxBound

instance (Bounded a, Ord a) => BoundedLattice (Region a) where

area :: Num a => Region a -> a
area (Region lx ly hx hy) = (hx - lx) * (hy - ly)

union :: Ord a => Region a -> Region a -> Region a
union (Region lxa lya hxa hya) (Region lxb lyb hxb hyb) =
  Region (min lxa lxb) (min lya lyb) (max hxa hxb) (max hya hyb)

intersection :: Ord a => Region a -> Region a -> Region a
intersection (Region lxa lya hxa hya) (Region lxb lyb hxb hyb) =
  Region (max lxa lxb) (max lya lyb) (min hxa hxb) (min hya hyb)

-- | True if a region contains a given point
containsPoint :: Ord a => a -> a -> Region a -> Bool
containsPoint x y (Region lx ly hx hy) =
  x >= lx && x <= hx && y >= ly && y <= hy

-- | True if a region is positive (max values are greater than min values),
-- False if the region is negative
polarity :: Ord a => Region a -> Bool
polarity (Region lx ly hx hy) = hx >= lx && hy >= ly

negate :: Region a -> Region a
negate (Region lx ly hx hy) = Region hx hy lx ly

-- | True if a region intersects another region
intersects :: Ord a => Region a -> Region a -> Bool
intersects a b = polarity $ a /\ b

-- | True if a region fully contains another region
contains :: Ord a => Region a -> Region a -> Bool
contains = flip joinLeq

fromPoint :: a -> a -> Region a
fromPoint x y = Region x y x y

bounds :: (Foldable f, Ord a) => Region a -> f (a, a) -> Region a
bounds = foldl' (\r x -> r \/ uncurry fromPoint x)

bounds' :: (Foldable f, Ord a, Bounded a) => f (a, a) -> Region a
bounds' = bounds bottom
