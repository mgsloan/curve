-- {-# RankNTypes, ScopedTypeVariables #-}

module Tests where

import Data.Curve
import qualified Data.Curve.Interval as I
import Data.Curve.Util

import Data.AffineSpace
import Data.VectorSpace

import Control.Monad (liftM, liftM2)
import Test.QuickCheck

import Numeric.Rounding (Precision)
import System.Random
--import Test.QuickCheck.Property

instance (Arbitrary a) => Arbitrary (SBasis a) where arbitrary = liftM  SBasis arbitrary
instance (Arbitrary a) => Arbitrary (Bezier a) where arbitrary = liftM  Bezier arbitrary
instance (Arbitrary a) => Arbitrary (Poly a)   where arbitrary = liftM  Poly   arbitrary
instance (Arbitrary a) => Arbitrary (Const a)  where arbitrary = liftM  Const  arbitrary
instance (Arbitrary a) => Arbitrary (Linear a) where arbitrary = liftM2 Linear arbitrary arbitrary
instance (Arbitrary a) => Arbitrary (I.Interval a) where arbitrary = liftM2 (I....) arbitrary arbitrary

prop_union a b c = 
  element c a || element c b ==>
  element c (union a b)

prop_intersect a b c = 
  element c a && element c b ==>
  element c (intersect a b)

ielem :: (Random a) => I.Interval a -> Gen a
ielem ivl = choose (I.inf ivl, I.sup ivl)

isubset :: (Random a, Ord a) => I.Interval a -> Gen (I.Interval a)
isubset i = 
  do f <- ielem i
     t <- ielem i
     return (if f < t then f I.... t else t I.... f)

itval :: (Num a, Ord a, Precision a, Random a) => Gen (I.Interval a)
itval = isubset (0.0 I.... 1.0)

tval :: (Num a, Ord a, Precision a, Random a) => Gen a
tval  = ielem (0.0 I.... 1.0)

prop_is_finite a = 
  do b <- tval 
     isFinite a ==> isFinite (a `at` b)

prop_is_zero a =
  do b <- tval
     isZero a ==> (a `at` b) == 0

prop_zeroV :: (IsZero a, AdditiveGroup a) => a -> Gen Bool
prop_zeroV x = return . isZero $ (sameconst x) zeroV
  where sameconst :: a -> a -> a
        sameconst _ = id

prop_bounds a =
  do b <- itval
     c <- ielem b
     return $ element (a `at` c) (bounds b a)

prop_domain_bounds a = 
  do let dom = bounds (domain a) a
     b <- isubset dom
     c <- ielem b
     return $ any (element c) $ domainBounds a b

similar x y = abs (x - y) < 0.0001

similarCurves a b =
  do c <- tval
     return $ (a `at` c) `similar` (b `at` c)

prop_portion a =
  do b <- itval
     similarCurves (portion b a) (portion b $ at a)

prop_portion_bounds a b = bounds b a `similar` bounds (domain por) por
  where por = portion b a

prop_roots a =
    do let dom = bounds (domain a) a
       b <- ielem dom
       return $ all (similar b) $ map (a `at`) $ rootsAt a b

prop_offset  a b = similarCurves (offset b a)  (offset b $ at a)
prop_sum     a b = similarCurves (a ^+^ b)     ((at a) ^+^ (at b))
prop_scale   a b = similarCurves (b *^ a)      (b *^ (at a))
prop_compose a b = similarCurves (compose a b) (compose (at a) (at b))

prop_to_bezier a = similarCurves a (toBezier a)
prop_to_poly   a = similarCurves a (toPoly a)
prop_to_sbasis a = similarCurves a (toSBasis a)
